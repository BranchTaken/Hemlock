open Basis
open! Basis.Rudiments

let line_raw_indentation line =
  String.C.Slice.fold_until ~init:0L ~f:(fun col cp ->
    match cp with
    | cp when Codepoint.(cp = of_char ' ') -> succ col, false
    | _ -> col, true
  ) line

let line_context_raw_indentation line_context =
  let line =
    line_context
    |> List.map ~f:Hmc.Source.Slice.to_string
    |> String.join
    |> String.C.Slice.of_string
  in
  line_raw_indentation line

let line_context_indentation line_context =
  let raw_indentation = line_context_raw_indentation line_context in
  (* Continuation lines have an extra 2 spaces; omit them from the result if present. *)
  raw_indentation - (raw_indentation % 4L)

let macro_of_line line =
  let open String.C in
  let ldangle = Codepoint.kv 0xabL (*'«'*) in
  let rdangle = Codepoint.kv 0xbbL (*'»'*) in
  match Slice.lfind ldangle line with
  | None -> None
  | Some base -> begin
      let slice = Slice.of_cursors ~base ~past:(Slice.past line) in
      match Slice.rfind rdangle slice with
      | None -> None
      | Some rdangle_base -> begin
          let past = Cursor.succ rdangle_base in
          let macro = Slice.of_cursors ~base ~past |> Slice.to_string in
          Some macro
        end
    end

let module_name conf =
  Path.Segment.to_string_hlt (Conf.module_ conf)

let fmt_source_directive indentation source formatter =
  let directive_pathstr =
    Hmc.Source.Slice.container source
    |> Hmc.Source.path
    |> Option.value_hlt
    |> Path.to_string_hlt
  in
  let base = Hmc.Source.Slice.base source in
  let pos = Hmc.Source.Cursor.pos base in
  let line = Text.Pos.line pos in
  let col = Text.Pos.col pos in
  formatter
  |> Fmt.fmt "[:" |> String.pp directive_pathstr
  |> Fmt.fmt ":" |> Uns.fmt line
  |> Fmt.fmt ":" |> Uns.fmt indentation |> Fmt.fmt "+" |> Uns.fmt (col - indentation)
  |> Fmt.fmt "]"

let expand ~template_indentation template expanders formatter =
  formatter
  |> (fun formatter ->
    let formatter, _first =
      String.C.Slice.lines_fold ~init:(formatter, true) ~f:(fun (formatter, first) line ->
        formatter
        |> (fun formatter ->
          match first with
          | true -> formatter
          | false -> formatter |> Fmt.fmt "\n"
        )
        |> (fun formatter ->
          let indentation = template_indentation + (line_raw_indentation line) in
          match macro_of_line line with
          | Some macro -> begin
              let expander = Map.get_hlt macro expanders in
              formatter |> expander ~indentation
            end
          | None -> begin
              formatter
              |> (fun formatter ->
                match first, String.C.Slice.length line with
                | true, _
                | _, 0L -> formatter
                | _, _ -> Fmt.fmt ~width:template_indentation "" formatter
              )
              |> Fmt.fmt (String.C.Slice.to_string line)
            end
        ),
        false
      ) (String.C.Slice.of_string template)
    in
    formatter
  )

let hmi_template = {|{
    Spec = {
        Algorithm = {
            type t: t =
              | Lr1 [@doc "LR(1) algorithm."]
              | Ielr1 [@doc "IELR(1) algorithm."]
              | Pgm1 [@doc "PGM(1) algorithm."]
              | Lalr1 [@doc "LALR(1) algorithm."]

            pp >e: t -> Fmt.Formatter e >e-> Fmt.Formatter e
          }

        algorithm: Algorithm.t
          [@@doc "Algorithm used to generate parser."]

        Assoc = {
            type t: t =
              | Left
              | Right

            pp >e: t -> Fmt.Formatter e >e-> Fmt.Formatter e
          }

        Prec = {
            type t: t = {
                index: uns # Index in `precs` array.
                name: string
                assoc: option Assoc.t
                doms: Ordset.t uns # Indices in `precs` array of dominator precedences.
              }

            pp >e: t -> Fmt.Formatter e >e-> Fmt.Formatter e
          }

        precs: array Prec.t
          [@@doc "Array of precedences, where each element's `index` field corresponds to the
          element's array index."]

        Prod = {
            type t: t = {
                index: uns # Index in `prods` array.
                lhs_index: uns
                rhs_indexes: array uns
                prec: option Prec.t
                reduction: uns # Index of corresponding reduction function in `reductions` array.
              }

            hash_fold: t -> Hash.State.t -> Hash.State.t
            cmp: t -> t -> Cmp.t
            pp >e: t -> Fmt.Formatter e >e-> Fmt.Formatter e
          }

        prods: array Prod.t
          [@@doc "Array of productions, where each element's `index` field corresponds to the
          element's array index."]

        Symbol = {
            type t: t = {
                index: uns # Index in `symbols` array.
                name: string
                prec: option Prec.t
                alias: option string
                start: bool
                prods: Ordset.t Prod.t Prod.cmper_witness # empty ≡ token
                first: Ordset.t uns Uns.cmper_witness
                follow: Ordset.t uns Uns.cmper_witness
              }

            hash_fold: t -> Hash.State.t -> Hash.State.t
            cmp: t -> t -> Cmp.t
            pp >e: t -> Fmt.Formatter e >e-> Fmt.Formatter e
          }

        symbols: array Symbol.t
          [@@doc "Array of symbols, where each element's `index` field corresponds to the element's
          array index."]

        Lr0Item = {
            type t: t = {
                prod: Prod.t
                dot: uns
              }

            hash_fold: t -> Hash.State.t -> Hash.State.t
            cmp: t -> t -> Cmp.t
            pp >e: t -> Fmt.Formatter e >e-> Fmt.Formatter e
          }

        Lr0Itemset = {
            type t: t = Ordset.t Lr0Item.t Lr0Item.cmper_witness

            hash_fold: t -> Hash.State.t -> Hash.State.t
            cmp: t -> t -> Cmp.t
            pp >e: t -> Fmt.Formatter e >e-> Fmt.Formatter e
          }

        Lr1Item = {
            type t: t = {
                lr0item: Lr0Item.t
                follow: Ordset.t uns Uns.cmper_witness
              }

            hash_fold: t -> Hash.State.t -> Hash.State.t
            cmp: t -> t -> Cmp.t
            pp >e: t -> Fmt.Formatter e >e-> Fmt.Formatter e
          }

        Lr1Itemset = {
            type t: t = Ordmap.t Lr0Item.t Lr1Item.t Lr0Item.cmper_witness

            hash_fold: t -> Hash.State.t -> Hash.State.t
            cmp: t -> t -> Cmp.t
            pp >e: t -> Fmt.Formatter e >e-> Fmt.Formatter e
          }

        Lr1ItemsetClosure = {
            type t: t = {
                index: uns # Index of corresponding `State.t` in `states` array.
                kernel: Lr1Itemset.t
                added: Lr1Itemset.t
              }

            hash_fold: t -> Hash.State.t -> Hash.State.t
            cmp: t -> t -> Cmp.t
            pp >e: t -> Fmt.Formatter e >e-> Fmt.Formatter e
          }

        Action = {
            type t: t =
              | ShiftPrefix of uns # `states` index.
              | ShiftAccept of uns # `states` index.
              | Reduce of uns # `prods` index.

            pp >e: t -> Fmt.Formatter e >e-> Fmt.Formatter e
          }

        State = {
            type t: t = {
                lr1ItemsetClosure: Lr1ItemsetClosure.t
                actions: Map.t uns Action.t Uns.cmper_witness
                gotos: Map.t uns uns Uns.cmper_witness
              }

            pp >e: t -> Fmt.Formatter e >e-> Fmt.Formatter e
          }

        states: array State.t
          [@@doc "Array of CFSM states, where each element's `lr1ItemsetClosure.index` field
          corresponds to the element's array index."]
      }

    Token = {
        type t: t =
          «tokens»

        pp >e: t -> Fmt.Formatter e >e-> Fmt.Formatter e

        spec: t -> Spec.Symbol.t
      }

    Nonterm = {
        type t: t =
          «nonterms»

        pp >e: t -> Fmt.Formatter e >e-> Fmt.Formatter e

        spec: t -> Spec.Symbol.t
      }

    Symbol = {
        type t: t =
          | Token of Token.t
          | Nonterm of Nonterm.t

        pp >e: t -> Fmt.Formatter e >e-> Fmt.Formatter e

        spec: t -> Spec.Symbol.t
      }

    State = {
        type t: t = uns

        pp >e: t -> Fmt.Formatter e >e-> Fmt.Formatter e

        spec: t -> Spec.State.t
      }

    type stack_elm: stack_elm = {
        symbol: Symbol.t
        state: State.t
      }
    type stack: stack = list stack_elm
    type reduction: reduction = stack -> stack

    reductions: array reduction
      [@@doc "Array of reductions, where each element's `index` field corresponds to the element's
      array index."]

    Status = {
        type t: t =
          # `feed`/`step` may produce these variants; `next` fast-forwards over them.
          | ShiftPrefix of (Token.t, State.t)
          | ShiftAccept of (Token.t, State.t)
          | Reduce of reduction
          # Common variants.
          | Prefix # Valid parse prefix; more input needed.
          | Accept of Nonterm.t # Successful parse result.
          | Reject of Token.t # Syntax error due to unexpected token.

        pp >e: t -> Fmt.Formatter e >e-> Fmt.Formatter e
      }

    type t: t = {
        stack: stack
        status: status
      }

    Start = {
        «starts»
      }

    feed: Token.t -> t -> t
      [@@doc "`feed token t` returns a result with status in {`ShiftPrefix`, `ShiftAccept`,
      `Reject`}. `t.status` must be `Prefix`."]

    step: t -> t
      [@@doc "`step t` returns the result of applying one state transition to `t`. `t.status` must
      be in {`ShiftPrefix`, `ShiftAccept`, `Reduce`}."]

    next: -> Token.t -> t -> t
      [@@doc "`next token t` calls `feed token t` and fast-forwards via `step` calls to return a
      result with status in {`Prefix`, `Accept`, `Reject`}. `t.status` must be `Prefix`."]
  }|}

let expand_hmi_template template_indentation template Spec.{symbols; _} formatter =
  let expand_tokens ~indentation formatter = begin
    let indent = fun formatter -> formatter |> Fmt.fmt ~width:indentation "" in
    let formatter, _first = Symbols.tokens_fold ~init:(formatter, true)
      ~f:(fun (formatter, first) {name; alias; qtype; _}->
        formatter
        |> (fun formatter ->
          match first with
          | true -> formatter
          | false -> formatter |> Fmt.fmt "\n"
        )
        |> (fun formatter ->
          match qtype with
          | {explicit_opt=None; _} -> begin
              formatter
              |> indent
              |> Fmt.fmt "| "
              |> Fmt.fmt name
            end
          | {explicit_opt=Some {module_; type_}; _} -> begin
              formatter
              |> indent
              |> Fmt.fmt "| "
              |> Fmt.fmt name
              |> Fmt.fmt " of "
              |> Fmt.fmt module_
              |> Fmt.fmt "."
              |> Fmt.fmt type_
            end
        )
        |> (fun formatter ->
          match alias with
          | None -> formatter
          | Some alias -> formatter |> Fmt.fmt " # " |> String.fmt ~pretty:true alias
        ),
        false
      ) symbols
    in
    formatter
  end in
  let expand_nonterms ~indentation formatter = begin
    let indent = fun formatter -> formatter |> Fmt.fmt ~width:indentation "" in
    let formatter, _first = Symbols.nonterms_fold ~init:(formatter, true)
      ~f:(fun (formatter, first) {name; qtype; _} ->
        formatter
        |> (fun formatter ->
          match first with
          | true -> formatter
          | false -> formatter |> Fmt.fmt "\n"
        )
        |> (fun formatter ->
          match qtype with
          | {explicit_opt=None; _} -> begin
              formatter
              |> indent
              |> Fmt.fmt "| "
              |> Fmt.fmt name
            end
          | {explicit_opt=Some {module_; type_}; _} -> begin
              formatter
              |> indent
              |> Fmt.fmt "| "
              |> Fmt.fmt name
              |> Fmt.fmt " of "
              |> Fmt.fmt module_
              |> Fmt.fmt "."
              |> Fmt.fmt type_
            end
        ),
        false
      ) symbols
    in
    formatter
  end in
  let expand_starts ~indentation formatter = begin
    let indent = fun formatter -> formatter |> Fmt.fmt ~width:indentation "" in
    let formatter, _first = Symbols.nonterms_fold ~init:(formatter, true)
      ~f:(fun (formatter, first) {name; qtype={synthetic; _}; start; _} ->
        (match start && (not synthetic) with
            | false -> formatter, first
            | true -> begin
                formatter
                |> (fun formatter ->
                  match first with
                  | true -> formatter
                  | false -> formatter |> Fmt.fmt "\n"
                )
                |> (fun formatter ->
                  formatter
                  |> indent |> String.fmt name |> Fmt.fmt " = {\n"
                  |> indent |> Fmt.fmt "    boi: t\n"
                  |> indent |> Fmt.fmt "  }"
                ),
                false
              end
        )
      ) symbols
    in
    formatter
  end in
  let expanders = Map.of_alist (module String) [
    ("«tokens»", expand_tokens);
    ("«nonterms»", expand_nonterms);
    ("«starts»", expand_starts)
  ] in
  formatter |> expand ~template_indentation template expanders

let generate_hmi conf Parse.(Hmhi {prelude; hocc; postlude; eoi=Eoi {eoi}}) io spec =
  assert (Spec.conflicts spec = 0L);
  let indentation = match hocc with
    | HmcToken _ -> not_reached ()
    | HoccToken {source; _} -> Hmc.Source.Slice.line_context source |> line_context_indentation
  in
  let module_name = module_name conf in
  let hmhi_name = module_name ^ ".hmhi" in
  let hmi_name = module_name ^ ".hmi" in
  let hmhi_path = Path.(join [Conf.srcdir conf; of_string hmhi_name] |> to_string_replace) in
  let directive_pathstr = String.(hmhi_path |> to_string ~pretty:true) in
  let io =
    io.hmi
    |> Fmt.fmt "# This file was generated by `hocc`; edit "
    |> Fmt.fmt String.(hmhi_name |> to_string ~pretty:true)
    |> Fmt.fmt " rather than "
    |> Fmt.fmt String.(hmi_name |> to_string ~pretty:true)
    |> Fmt.fmt "\n"
    |> Fmt.fmt "[:" |> Fmt.fmt directive_pathstr |> Fmt.fmt ":1]"
    |> (fun formatter ->
      match prelude with
      | Parse.Matter {token; _} -> begin
          let base = match token with
            | HmcToken {source; _} -> Hmc.Source.Slice.base source
            | HoccToken _ -> not_reached ()
          in
          let past = match hocc with
            | HmcToken _ -> not_reached ()
            | HoccToken {source; _} -> Hmc.Source.Slice.base source
          in
          let source = Hmc.Source.Slice.of_cursors ~base ~past in
          formatter |> Fmt.fmt (Hmc.Source.Slice.to_string source)
        end
      | MatterEpsilon -> formatter
    )
    |> Fmt.fmt "[:]"
    |> expand_hmi_template indentation hmi_template spec
    |> (fun formatter ->
      match postlude with
      | Parse.Matter _ -> begin
          let base = match hocc with
            | HmcToken _ -> not_reached ()
            | HoccToken {source; _} -> Hmc.Source.Slice.past source
          in
          let past = match eoi with
            | HmcToken {source; _} -> Hmc.Source.Slice.past source
            | HoccToken _ -> not_reached ()
          in
          let source = Hmc.Source.Slice.of_cursors ~base ~past in
          formatter
          |> fmt_source_directive indentation source
          |> Fmt.fmt (Hmc.Source.Slice.to_string source)
        end
      | MatterEpsilon -> formatter
    )
    |> Io.with_hmi io
  in
  io

let hm_template = {|{
    Spec = {
        Algorithm = {
            type t: t =
              | Lr1
              | Ielr1
              | Pgm1
              | Lalr1

            to_string = function
              | Lr1 -> "Lr1"
              | Ielr1 -> "Ielr1"
              | Pgm1 -> "Pgm1"
              | Lalr1 -> "Lalr1"

            pp t formatter =
                formatter |> Fmt.fmt (to_string t)
          }

        «algorithm»

        Assoc = {
            type t: t =
              | Left
              | Right

            to_string = function
              | Left -> "Left"
              | Right -> "Right"

            pp t formatter =
                formatter |> Fmt.fmt (to_string t)
          }

        Prec = {
            type t: t = {
                index: uns
                name: string
                assoc: option Assoc.t
                doms: Ordset.t uns
              }

            pp {index; name; assoc; doms} formatter =
                formatter
                  |> Fmt.fmt
                  "{%u=(^index
                  ^); %s=(^name
                  ^); %f(^Option.pp Assoc.pp^)=(^assoc
                  ^); %f(^Ordset.pp^)=(^doms
                  ^)}"

            init ~index ~name ~assoc ~doms =
                {index; name; assoc; doms}
          }

        precs = [|
            «precs»
          |]

        Prod = {
            type t: t = {
                index: uns
                lhs_index: uns
                rhs_indexes: array uns
                prec: option Prec.t
                reduction: uns
              }

            hash_fold {index; _} state =
                Uns.hash_fold index state

            cmp {index=i0; _} {index=i1; _} =
                Uns.cmp i0 i1

            pp {index; lhs_index; rhs_indexes; prec; reduction} formatter =
                formatter
                  |> Fmt.fmt
                  "{%u=(^index
                  ^); %u=(^lhs_index
                  ^); %f(^Array.pp Uns.pp^)=(^rhs_indexes
                  ^); %f(^Option.pp Prec.pp^)=(^prec
                  ^); %u=(^reduction
                  ^)}"

            init ~index ~lhs_index ~rhs_indexes ~prec ~reduction =
                {index; lhs_index; rhs_indexes; prec; reduction}
          }

        prods = [|
            «prods»
          |]

        Symbol = {
            type t: t = {
                index: uns
                name: string
                prec: option Prec.t
                alias: option string
                start: bool
                prods: Ordset.t Prod.t Prod.cmper_witness
                first: Ordset.t uns Uns.cmper_witness
                follow: Ordset.t uns Uns.cmper_witness
              }

            hash_fold {index; _} state =
                Uns.hash_fold index state

            cmp {index=i0; _} {index=i1; _} =
                Uns.cmp i0 i1

            pp {index; name; prec; alias; start; prods; first; follow} formatter =
                formatter
                  |> Fmt.fmt
                  "{%u=(^index
                  ^); %s=(^name
                  ^); %f(^Option.pp Prec.pp^)=(^prec
                  ^); %f(^Option.pp String.pp^)=(^alias
                  ^); %b=(^start
                  ^); %f(^Ordset.pp^)=(^prods
                  ^); %f(^Ordset.pp^)=(^first
                  ^); %f(^Ordset.pp^)=(^follow
                  ^)}"

            init ~index ~name ~prec ~alias ~start ~prods ~first ~follow =
                {index; name; prec; alias; start; prods; first; follow}
          }

        symbols = [|
            «symbols»
          |]

        Lr0Item = {
            type t: t = {
                prod: Prod.t
                dot: uns
              }

            hash_fold {prod; dot} state =
                state
                  |> Prod.hash_fold prod
                  |> Uns.hash_fold dot

            cmp {prod=p0; dot=d0} {prod=p1; dot=d1} =
                let open Cmp
                match Prod.cmp p0 p1 with
                  | Lt -> Lt
                  | Eq -> Uns.cmp d0 d1
                  | Gt -> Gt

            pp {prod; dot} formatter =
                formatter |> Fmt.fmt "{%f(^Prod.pp^)=(^prod^); %u=(^dot^)}"

            init ~prod ~dot =
                {prod; dot}
          }

        Lr0Itemset = {
            type t: t = Ordset.t Lr0Item.t Lr0Item.cmper_witness

            hash_fold = Ordset.hash_fold
            cmp = Ordset.cmp
            pp = Ordset.pp

            init = Ordset.of_alist Lr0Item
          }

        Lr1Item = {
            type t: t = {
                lr0item: Lr0Item.t
                follow: Ordset.t uns Uns.cmper_witness
              }

            hash_fold {lr0item; follow} state =
                state
                  |> Lr0Item.hash_fold lr0item
                  |> Ordset.hash_fold follow

            cmp {lr0item=l0; follow=f0} {lr0item=l1; follow=f1} =
                let open Cmp
                match Lr0Item.cmp l0 l1 with
                  | Lt -> Lt
                  | Eq -> Ordset.cmp f0 f1
                  | Gt -> Gt

            pp {lr0item; follow} formatter =
                formatter |> Fmt.fmt "{%f(^Lr0Item.pp^)=(^lr0item^); %f(^Ordset.pp^)=(^follow^)}"

            init ~lr0item ~follow =
                {lr0item; follow}
          }

        Lr1Itemset = {
            type t: t = Ordmap.t Lr0Item.t Lr1Item.t Lr0Item.cmper_witness

            hash_fold = Ordmap.hash_fold Lr1Item.hash_fold
            cmp = Ordmap.cmp Lr1Item.cmp
            pp = Ordmap.pp Lr1Item.pp

            init = Ordmap.of_alist Lr0Item
          }

        Lr1ItemsetClosure = {
            type t: t = {
                index: uns
                kernel: Lr1Itemset.t
                added: Lr1Itemset.t
              }

            hash_fold {index; _} state =
                state |> Uns.hash_fold index

            cmp {index=i0; _} {index=i1; _} =
                Uns.cmp i0 i1

            pp {index; kernel; added} formatter =
                formatter
                  |> Fmt.fmt
                  "{%u=(^index
                  ^); %f(^Lr1Itemset.pp^)=(^kernel
                  ^); %f(^Lr1Itemset.pp^)=(^added
                  ^)}"

            init ~index ~kernel ~added =
                {index; kernel; added}
          }

        Action = {
            type t: t =
              | ShiftPrefix of uns
              | ShiftAccept of uns
              | Reduce of uns

            to_string = function
              | ShiftPrefix state_index -> "ShiftPrefix %u(^state_index^)"
              | ShiftAccept state_index -> "ShiftAccept %u(^state_index^)"
              | Reduce prod_index -> "Reduce %u(^prod_index^)"

            pp t formatter =
                formatter |> Fmt.fmt (to_string t)
          }

        State = {
            type t: t = {
                lr1ItemsetClosure: Lr1ItemsetClosure.t
                actions: Map.t uns Action.t Uns.cmper_witness
                gotos: Map.t uns uns Uns.cmper_witness
              }

            pp {lr1ItemsetClosure; actions; gotos} formatter =
                formatter
                  |> Fmt.fmt
                  "{%f(^Lr1ItemsetClosure.pp^)=(^lr1ItemsetClosure
                  ^); %f(^Map.pp Action.pp^)=(^actions
                  ^); %f(^Map.pp Uns.pp^)=(^gotos
                  ^)}"

            init ~lr1ItemsetClosure ~actions ~gotos =
                {lr1ItemsetClosure; actions; gotos}
          }

        states = [|
            «states»
          |]
      }

    Token = {
        type t: t =
          «tokens»

        index = function
          «token_index»

        spec t =
            Array.get (index t) Spec.symbols

        pp t formatter =
            formatter
              |> Spec.Token.pp (spec t)
      }

    Nonterm = {
        type t: t =
          «nonterms»

        index = function
          «nonterm_index»

        spec t =
            Array.get (index t) Spec.symbols

        pp t formatter =
            formatter
              |> Spec.Token.pp (spec t)
      }

    Symbol = {
        type t: t =
          | Token of Token.t
          | Nonterm of Nonterm.t

        index = function
          | Token token -> Token.index token
          | Nonterm nonterm -> Nonterm.index nonterm

        spec = function
          | Token token -> Token.spec token
          | Nonterm nonterm -> Nonterm.spec nonterm

        pp t formatter =
            formatter
              |> Spec.Symbol.pp (spec t)
      }

    State = {
        type t: t = uns

        spec t =
            Array.get t Spec.states

        pp t formatter =
            formatter |> Uns.pp t

        init state_index =
            state_index
      }

    type stack_elm: stack_elm = {
        symbol: Symbol.t
        state: State.t
      }
    type stack: stack = list stack_elm
    type reduction: reduction = stack -> stack

    # goto: Symbol.t -> stack -> stack
    goto symbol stack =
        match stack with
          | [] -> not_reached ()
          | {state; _} :: _ ->
            let symbol_index = Symbol.index symbol
            let Spec.State.{goto; _} = Array.get state Spec.states
            let state' = Map.get_hlt symbol_index goto |> State.init
            {symbol; state=state'} :: stack

    reductions = [|
        «reductions»
      |]

    Status = {
        type t: t =
          | ShiftPrefix of (Token.t, State.t)
          | ShiftAccept of (Token.t, State.t)
          | Reduce of reduction
          | Prefix
          | Accept of Nonterm.t
          | Reject of Token.t

        pp t formatter =
            formatter
              |> fn formatter ->
                match t with
                  | ShiftPrefix (token, state) ->
                    formatter
                      |> Fmt.fmt "ShiftPrefix (%f(^Token.pp^)(^token^), %f(^State.pp^)(^state^))"
                  | ShiftAccept (token, state) ->
                    formatter
                      |> Fmt.fmt "ShiftAccept (%f(^Token.pp^)(^token^), %f(^State.pp^)(^state^))"
                  | Reduce reduction ->
                    formatter |> Fmt.fmt "Reduce (stack -> stack)"
                  | Prefix ->
                    formatter |> Fmt.fmt "Prefix"
                  | Accept nonterm ->
                    formatter |> Fmt.fmt "Accept %f(^Nonterm.pp^)(^nonterm^)"
                  | Reject token ->
                    formatter |> Fmt.fmt "Reject %f(^Token.pp^)(^token^)"
      }

    type t: t = {
        stack: stack
        status: status
      }

    Start = {
        «starts»
      }

    feed token = function
      | {stack={symbol; state} :: _; Prefix}
        let token_index = Token.index token
        let Spec.State.{actions; _} = Array.get state Spec.states
        let status = match Map.get token_index actions with
          | Some (Action.ShiftPrefix state') -> Status.ShiftPrefix (token, state')
          | Some (Action.ShiftAccept state') -> Status.ShiftAccept (token, state')
          | Some (Action.Reduce prod_index) ->
            let reduction = Array.get prod_index reductions
            Status.Reduce reduction
          | None -> Status.Reject token
        {t with status}
      | _ -> not_reached ()

    # shift: Symbol.t -> State.t -> stack -> stack
    shift token state stack =
        {symbol=token; state} :: stack

    # reduce: reduction -> stack -> stack
    reduce reduction stack =
        reduction stack

    step {stack; status} =
        let open Status
        match status with
          | ShiftPrefix (token, state) -> {stack=shift token state stack; status=Prefix}
          | ShiftAccept (token, state) ->
            # Shift, perform the ⊥ reduction, and extract the accepted symbol from the stack.
            let stack = shift token state stack
            let pseudo_end_index = Token.index Token.PSEUDO_END
            let Spec.State.{actions; _} = Array.get state Spec.states
            match Map.get_hlt pseudo_end_index actions with
              | Action.Reduce prod_index ->
                let reduction = Array.get prod_index reductions
                let stack = reduce reduction stack
                match stack with
                  | [] -> not_reached ()
                  | {symbol; _} :: _ -> {stack=[]; status=Accept symbol}
              | _ -> not_reached ()
          | Reduce reduction -> {stack=reduce reduction stack; Prefix}
          | _ -> not_reached ()

    # rec walk: t -> t
    rec walk ({status; _} as t) =
        let open Status
        match status with
          | ShiftPrefix _
          | ShiftAccept _
          | Reduce _ -> t |> step |> walk
          | Prefix
          | Accept _
          | Reject _ -> t

    next token ({status; _} as t) =
        match status with
          | Status.Prefix -> t |> feed token |> walk
          | _ -> not_reached ()
  }|}

let state_of_synthetic_start_symbol symbols states synthetic_start_symbol =
  assert (Symbol.is_synthetic synthetic_start_symbol);
  assert (synthetic_start_symbol.start);
  Array.find ~f:(fun state ->
    match State.is_start state with
    | false -> false
    | true -> begin
        let start_symbol_index = State.start_symbol_index state in
        let start_symbol = Symbols.symbol_of_symbol_index start_symbol_index symbols in
        Symbol.(start_symbol = synthetic_start_symbol)
      end
  ) states
  |> Option.value_hlt

let expand_hm_template template_indentation template hocc_block
    Spec.{algorithm; precs; symbols; prods; reductions; states} formatter =
  let expand_algorithm ~indentation formatter = begin
    let indent = fun formatter -> formatter |> Fmt.fmt ~width:indentation "" in
    formatter
    |> indent
    |> Fmt.fmt "algorithm = Algorithm."
    |> Conf.pp_algorithm algorithm
  end in
  let expand_precs ~indentation formatter = begin
    let indent = fun formatter -> formatter |> Fmt.fmt ~width:indentation "" in
    let formatter, _first = Precs.fold ~init:(formatter, true)
      ~f:(fun (formatter, first) Prec.{index; name; assoc; doms; _} ->
        formatter
        |> (fun formatter ->
          match first with
          | true -> formatter
          | false -> formatter |> Fmt.fmt "\n"
        )
        |> (fun formatter ->
          formatter
          |> indent
          |> Fmt.fmt "Prec.init"
          |> Fmt.fmt " ~index:" |> Prod.Index.pp index
          |> Fmt.fmt " ~name:" |> String.pp name
          |> Fmt.fmt " ~assoc:"
          |> (fun formatter ->
            match assoc with
            | None -> formatter |> Fmt.fmt "None"
            | Some assoc -> formatter |> Fmt.fmt "(Some " |> Assoc.pp assoc |> Fmt.fmt ")"
          )
          |> Fmt.fmt " ~doms:(Ordset."
          |> (fun formatter ->
            match Ordset.length doms with
            | 0L -> formatter |> Fmt.fmt "empty Uns"
            | 1L ->
              formatter |> Fmt.fmt "singleton Uns " |> (Ordset.choose_hlt doms |> Prec.Index.pp)
            | _ -> begin
                formatter
                |> Fmt.fmt "of_list "
                |> (Ordset.to_list doms |> List.fmt ~alt:true ~width:indentation Prec.Index.pp)
              end
          )
          |> Fmt.fmt ")"
        ),
        false
      ) precs
    in
    formatter
  end in
  let expand_prods ~indentation formatter = begin
    let indent = fun formatter -> formatter |> Fmt.fmt ~width:indentation "" in
    let formatter, _first = Prods.fold ~init:(formatter, true)
      ~f:(fun (formatter, first) Prod.{index; lhs_index; rhs_indexes; prec; reduction; _} ->
        formatter
        |> (fun formatter ->
          match first with
          | true -> formatter
          | false -> formatter |> Fmt.fmt "\n"
        )
        |> (fun formatter ->
          formatter
          |> indent
          |> Fmt.fmt "Prod.init"
          |> Fmt.fmt " ~index:" |> Prod.Index.pp index
          |> Fmt.fmt " ~lhs_index:" |> Symbol.Index.pp lhs_index
          |> Fmt.fmt " ~rhs_indexes:" |> Array.pp Symbol.Index.pp rhs_indexes
          |> Fmt.fmt "\n" |> indent |> Fmt.fmt "  ~prec:"
          |> (fun formatter ->
            match prec with
            | None -> formatter |> Fmt.fmt "None"
            | Some prec -> begin
                formatter
                |> Fmt.fmt "(Some (Array.get " |> Prec.Index.pp prec.index |> Fmt.fmt " precs))"
              end
          )
          |> Fmt.fmt " ~reduction:" |> Reduction.Index.pp reduction.index
        ),
        false
      ) prods
    in
    formatter
  end in
  let expand_symbols ~indentation formatter = begin
    let indent = fun formatter -> formatter |> Fmt.fmt ~width:indentation "" in
    let formatter, _first_line = Symbols.symbols_fold ~init:(formatter, true)
      ~f:(fun (formatter, first_line)
        Symbol.{index; name; prec; alias; start; prods; first; follow; _} ->
        formatter
        |> (fun formatter ->
          match first_line with
          | true -> formatter
          | false -> formatter |> Fmt.fmt "\n"
        )
        |> (fun formatter ->
          formatter
          |> indent
          |> Fmt.fmt "Symbol.init"
          |> Fmt.fmt " ~index:" |> Symbol.Index.pp index
          |> Fmt.fmt " ~name:" |> String.pp name
          |> Fmt.fmt "\n" |> indent |> Fmt.fmt "  ~prec:"
          |> (fun formatter ->
            match prec with
            | None -> formatter |> Fmt.fmt "None"
            | Some prec -> begin
                formatter
                |> Fmt.fmt "(Some (Array.get " |> Prec.Index.pp prec.index |> Fmt.fmt " precs))"
              end
          )
          |> Fmt.fmt " ~alias:"
          |> (fun formatter ->
            match alias with
            | None -> formatter |> Fmt.fmt "None"
            | Some alias -> formatter |> Fmt.fmt "(Some " |> String.pp alias |> Fmt.fmt ")"
          )
          |> Fmt.fmt " ~start:" |> Bool.pp start
          |> Fmt.fmt "\n" |> indent |> Fmt.fmt "  ~prods:("
          |> (fun formatter ->
            match Ordset.length prods with
            | 0L -> formatter |> Fmt.fmt "Ordset.empty Prod"
            | 1L -> begin
                let Prod.{index; _} = Ordset.choose_hlt prods in
                formatter |> Fmt.fmt "Ordset.singleton Prod " |> Prod.Index.pp index
              end
            | _ -> begin
                formatter
                |> Fmt.fmt "Ordset.of_list Prod "
                |> List.fmt ~alt:true ~width:indentation (fun Prod.{index; _} formatter ->
                  formatter
                  |> Fmt.fmt "Array.get " |> Prod.Index.pp index |> Fmt.fmt " prods"
                ) (Ordset.to_list prods)
              end
          )
          |> Fmt.fmt ")"
          |> Fmt.fmt " ~first:("
          |> (fun formatter ->
            match Ordset.length first with
            | 0L -> formatter |> Fmt.fmt "Ordset.empty Uns"
            | 1L -> begin
                let symbol_index = Ordset.choose_hlt first in
                formatter |> Fmt.fmt "Ordset.singleton Uns " |> Prod.Index.pp symbol_index
              end
            | _ -> begin
                formatter
                |> Fmt.fmt "Ordset.of_list Uns "
                |> List.fmt Symbol.Index.pp (Ordset.to_list first)
              end
          )
          |> Fmt.fmt ")"
          |> Fmt.fmt "\n" |> indent |> Fmt.fmt "  ~follow:("
          |> (fun formatter ->
            match Ordset.length follow with
            | 0L -> formatter |> Fmt.fmt "Ordset.empty Uns"
            | 1L -> begin
                let symbol_index = Ordset.choose_hlt follow in
                formatter |> Fmt.fmt "Ordset.singleton Uns " |> Prod.Index.pp symbol_index
              end
            | _ -> begin
                formatter
                |> Fmt.fmt "Ordset.of_list Uns "
                |> List.pp Symbol.Index.pp (Ordset.to_list follow)
              end
          )
          |> Fmt.fmt ")"
        ),
        false
      ) symbols
    in
    formatter
  end in
  let expand_lr1Itemset ~indentation lr1itemset formatter = begin
    let indent = fun formatter -> formatter |> Fmt.fmt ~width:indentation "" in
    match Lr1Itemset.is_empty lr1itemset with
    | false -> begin
        formatter
        |> indent |> Fmt.fmt "        Lr1Itemset.init [\n"
        |> (fun formatter ->
          Lr1Itemset.fold ~init:formatter
            ~f:(fun formatter {lr0item={prod={index=prod_index; _}; dot}; follow} ->
              formatter
              |> indent |> Fmt.fmt "            (\n"
              |> indent
              |> Fmt.fmt "                let lr0item = Lr0Item.init ~prod:"
              |> Fmt.fmt "(Array.get " |> Prod.Index.pp prod_index |> Fmt.fmt " prods)"
              |> Fmt.fmt " ~dot:" |> Uns.pp dot |> Fmt.fmt "\n"
              |> indent
              |> Fmt.fmt "                let lr1item = Lr1Item.init ~lr0item ~follow:\n"
              |> indent
              |> Fmt.fmt "                    Ordset.of_alist " |> Ordset.pp follow |> Fmt.fmt "\n"
              |> indent |> Fmt.fmt "                lr0item, lr1item\n"
              |> indent |> Fmt.fmt "              )\n"
            ) lr1itemset
        )
        |> indent |> Fmt.fmt "          ]\n"
      end
    | true -> formatter |> indent |> Fmt.fmt "        Lr1Itemset.empty\n"
  end in
  let expand_states ~indentation formatter = begin
    let indent = fun formatter -> formatter |> Fmt.fmt ~width:indentation "" in
    let formatter, _first = Array.fold ~init:(formatter, true)
      ~f:(fun (formatter, first)
        State.{statenub={lr1itemsetclosure={index; kernel; added}; _}; actions; gotos} ->
        formatter
        |> (fun formatter ->
          match first with
          | true -> formatter
          | false -> formatter |> Fmt.fmt "\n"
        )
        |> (fun formatter ->
          formatter
          |> indent |> Fmt.fmt "(* " |> Lr1ItemsetClosure.Index.pp index
          |> Fmt.fmt " *) State.init\n"
          |> indent |> Fmt.fmt "  ~lr1ItemsetClosure:\n"
          |> indent |> Fmt.fmt "    Lr1ItemsetClosure.init\n"
          |> indent |> Fmt.fmt "      ~index:"
          |> Lr1ItemsetClosure.Index.pp index |> Fmt.fmt "\n"
          |> indent |> Fmt.fmt "      ~kernel:\n"
          |> expand_lr1Itemset ~indentation kernel
          |> indent |> Fmt.fmt "      ~added:\n"
          |> expand_lr1Itemset ~indentation added
          |> indent |> Fmt.fmt "  ~actions:\n"
          |> indent |> Fmt.fmt "    Map.of_alist Action [\n"
          |> (fun formatter ->
            Ordmap.fold ~init:formatter ~f:(fun formatter (symbol_index, action_set) ->
              assert (Ordset.length action_set = 1L);
              let action = Ordset.choose_hlt action_set in
              formatter
              |> indent |> Fmt.fmt "        "
              |> Symbol.Index.pp symbol_index
              |> Fmt.fmt ", Action."
              |> State.Action.pp action
              |> Fmt.fmt "\n"
            ) actions
          )
          |> indent |> Fmt.fmt "      ]\n"
          |> indent |> Fmt.fmt "  ~gotos:\n"
          |> (fun formatter ->
            match Ordmap.is_empty gotos with
            | false -> begin
                formatter
                |> indent |> Fmt.fmt "    Map.of_alist Uns [\n"
                |> (fun formatter ->
                  Ordmap.fold ~init:formatter ~f:(fun formatter (symbol_index, state_index) ->
                    formatter
                    |> indent |> Fmt.fmt "        "
                    |> Symbol.Index.pp symbol_index
                    |> Fmt.fmt ", Action."
                    |> State.Index.pp state_index
                    |> Fmt.fmt "\n"
                  ) gotos
                )
                |> indent |> Fmt.fmt "      ]"
              end
            | true -> formatter |> indent |> Fmt.fmt "    Map.empty Uns"
          )
        ),
        false
      ) states
    in
    formatter
  end in
  let expand_tokens ~indentation formatter = begin
    let indent = fun formatter -> formatter |> Fmt.fmt ~width:indentation "" in
    let formatter, _first = Symbols.tokens_fold ~init:(formatter, true)
      ~f:(fun (formatter, first) {name; alias; qtype; _} ->
        formatter
        |> (fun formatter ->
          match first with
          | true -> formatter
          | false -> formatter |> Fmt.fmt "\n"
        )
        |> (fun formatter ->
          match qtype with
          | {explicit_opt=None; _} -> begin
              formatter
              |> indent
              |> Fmt.fmt "| "
              |> Fmt.fmt name
            end
          | {explicit_opt=Some {module_; type_}; _} -> begin
              formatter
              |> indent
              |> Fmt.fmt "| "
              |> Fmt.fmt name
              |> Fmt.fmt " of "
              |> Fmt.fmt module_
              |> Fmt.fmt "."
              |> Fmt.fmt type_
            end
        )
        |> (fun formatter ->
          match alias with
          | None -> formatter
          | Some alias -> formatter |> Fmt.fmt " # " |> String.fmt ~pretty:true alias
        ),
        false
      ) symbols
    in
    formatter
  end in
  let expand_token_index ~indentation formatter = begin
    let indent = fun formatter -> formatter |> Fmt.fmt ~width:indentation "" in
    let formatter, _first = Symbols.tokens_fold ~init:(formatter, true)
      ~f:(fun (formatter, first) {index; name; qtype; _} ->
        formatter
        |> (fun formatter ->
          match first with
          | true -> formatter
          | false -> formatter |> Fmt.fmt "\n"
        )
        |> (fun formatter ->
          formatter
          |> indent
          |> Fmt.fmt "| "
          |> Fmt.fmt name
          |> (fun formatter ->
            match qtype.explicit_opt with
            | None -> formatter
            | Some _ -> formatter |> Fmt.fmt " _"
          )
          |> Fmt.fmt " -> "
          |> Uns.pp index
        ),
        false
      ) symbols
    in
    formatter
  end in
  let expand_nonterms ~indentation formatter = begin
    let indent = fun formatter -> formatter |> Fmt.fmt ~width:indentation "" in
    let formatter, _first = Symbols.nonterms_fold ~init:(formatter, true)
      ~f:(fun (formatter, first) {name; qtype; _} ->
        formatter
        |> (fun formatter ->
          match first with
          | true -> formatter
          | false -> formatter |> Fmt.fmt "\n"
        )
        |> (fun formatter ->
          match qtype with
          | {explicit_opt=None; _} -> begin
              formatter
              |> indent
              |> Fmt.fmt "| "
              |> Fmt.fmt name
            end
          | {explicit_opt=Some {module_; type_}; _} -> begin
              formatter
              |> indent
              |> Fmt.fmt "| "
              |> Fmt.fmt name
              |> Fmt.fmt " of "
              |> Fmt.fmt module_
              |> Fmt.fmt "."
              |> Fmt.fmt type_
            end
        ),
        false
      ) symbols
    in
    formatter
  end in
  let expand_nonterm_index ~indentation formatter = begin
    let indent = fun formatter -> formatter |> Fmt.fmt ~width:indentation "" in
    let formatter, _first = Symbols.nonterms_fold ~init:(formatter, true)
      ~f:(fun (formatter, first) {index; name; _} ->
        formatter
        |> (fun formatter ->
          match first with
          | true -> formatter
          | false -> formatter |> Fmt.fmt "\n"
        )
        |> (fun formatter ->
          formatter
          |> indent
          |> Fmt.fmt "| "
          |> Fmt.fmt name
          |> Fmt.fmt " _ -> "
          |> Uns.pp index
        ),
        false
      ) symbols
    in
    formatter
  end in
  let expand_reductions ~indentation formatter = begin
    let indent = fun formatter -> formatter |> Fmt.fmt ~width:indentation "" in
    let formatter, _first = Reductions.fold ~init:(formatter, true)
      ~f:(fun (formatter, first) (Reduction.{index; lhs_name; rhs; code; _} as reduction) ->
        formatter
        |> (fun formatter ->
          match first with
          | true -> formatter
          | false -> formatter |> Fmt.fmt "\n"
        )
        |> (fun formatter ->
          formatter
          |> indent |> Fmt.fmt "(* " |> Reduction.Index.pp index
          |> Fmt.fmt " *) "
          |> (fun formatter ->
            match Reduction.is_epsilon reduction || Option.is_empty code with
            | false -> begin
                let underline = Codepoint.of_char '_' in
                let overline = Codepoint.kv 0x203eL (*'‾'*) in
                let code = Option.value_hlt code in
                let source = Parse.source_of_code hocc_block code in
                formatter
                |> Fmt.fmt "function\n"
                |> (fun formatter ->
                  let formatter, _first =
                    Reduction.Params.fold_right ~init:(formatter, true)
                      ~f:(fun (formatter, first)
                        Reduction.Param.{binding; symbol_name; qtype={explicit_opt; _}; _} ->
                        let symbol_constructor = match explicit_opt with
                          | None -> "Token"
                          | Some _ -> "Nonterm"
                        in
                        formatter
                        |> indent
                        |> Fmt.fmt (match first with
                          | true -> "  | "
                          | false -> "  :: "
                        )
                        |> (fun formatter ->
                          match binding with
                          | Some uname -> begin
                              formatter
                              |> Fmt.fmt "{symbol=Symbol."
                              |> Fmt.fmt symbol_constructor
                              |> Fmt.fmt " ("
                              |> Fmt.fmt symbol_name
                              |> Fmt.fmt " "
                              |> Fmt.fmt uname
                              |> Fmt.fmt "); _}"
                            end
                          | None -> formatter |> Fmt.fmt "_"
                        )
                        |> Fmt.fmt "\n"
                      , false
                      ) rhs
                  in
                  formatter
                )
                |> indent |> Fmt.fmt "  :: tl -> Symbol.Nonterm ("
                |> Fmt.fmt lhs_name |> Fmt.fmt " (\n"
                |> indent
                |> String.fmt ~pad:underline ~just:Fmt.Left ~width:(100L - indentation) "  # "
                |> Fmt.fmt "\n"
                |> indent |> Fmt.fmt "  "
                |> fmt_source_directive (Parse.indentation_of_code hocc_block code) source
                |> Fmt.fmt (Hmc.Source.Slice.to_string source)
                |> Fmt.fmt "[:]\n"
                |> indent
                |> String.fmt ~pad:overline ~just:Fmt.Left ~width:(100L - indentation) "  # "
                |> Fmt.fmt "\n"
                |> indent |> Fmt.fmt "  )) :: tl\n"
                |> indent |> Fmt.fmt "  | _ -> not_reached ()"
              end
            | true -> formatter |> Fmt.fmt "fn stack -> stack"
          )
        ),
        false
      ) reductions
    in
    formatter
  end in
  let expand_starts ~indentation formatter = begin
    let indent = fun formatter -> formatter |> Fmt.fmt ~width:indentation "" in
    let formatter, _first = Symbols.nonterms_fold ~init:(formatter, true)
      ~f:(fun (formatter, first) {name; qtype={synthetic; _}; start; _} ->
        match (start && (not synthetic)) with
        | false -> formatter, first
        | true -> begin
            let synthetic_name = Spec.synthetic_name_of_start_name name in
            let synthetic_start_symbol =
              Symbols.symbol_of_name synthetic_name symbols |> Option.value_hlt in
            let state =
              state_of_synthetic_start_symbol symbols states synthetic_start_symbol in
            formatter
            |> (fun formatter ->
              match first with
              | true -> formatter
              | false -> formatter |> Fmt.fmt "\n"
            )
            |> (fun formatter ->
              formatter
              |> indent |> String.fmt name |> Fmt.fmt " = {\n"
              |> indent |> Fmt.fmt "    boi = {\n"
              |> indent |> Fmt.fmt "        stack=[{\n"
              |> indent |> Fmt.fmt "            symbol=Token.EPSILON\n"
              |> indent |> Fmt.fmt "            state_index="
              |> State.(index state |> Index.pp) |> Fmt.fmt "\n"
              |> indent |> Fmt.fmt "          }]\n"
              |> indent |> Fmt.fmt "        status=Prefix\n"
              |> indent |> Fmt.fmt "      }\n"
              |> indent |> Fmt.fmt "  }"
            ),
            false
          end
      ) symbols
    in
    formatter
  end in
  let expanders = Map.of_alist (module String) [
    ("«algorithm»", expand_algorithm);
    ("«precs»", expand_precs);
    ("«prods»", expand_prods);
    ("«symbols»", expand_symbols);
    ("«states»", expand_states);
    ("«tokens»", expand_tokens);
    ("«token_index»", expand_token_index);
    ("«nonterms»", expand_nonterms);
    ("«nonterm_index»", expand_nonterm_index);
    ("«reductions»", expand_reductions);
    ("«starts»", expand_starts)
  ] in
  formatter |> expand ~template_indentation template expanders

let generate_hm conf Parse.(Hmh {prelude; hocc=(Hocc {hocc; _} as hocc_block); postlude; eoi=Eoi {eoi}} as _XXX_hmh) io spec =
(*
  File.Fmt.stderr
  |> Parse.fmt_hmh ~alt:true _XXX_hmh
  |> ignore;
*)
  assert (Spec.conflicts spec = 0L);
  let indentation = match hocc with
    | HmcToken _ -> not_reached ()
    | HoccToken {source; _} -> Hmc.Source.Slice.line_context source |> line_context_indentation
  in
  let module_name = module_name conf in
  let hmh_name = module_name ^ ".hmh" in
  let hm_name = module_name ^ ".hm" in
  let hmh_path = Path.(join [Conf.srcdir conf; of_string hmh_name] |> to_string_replace) in
  let directive_pathstr = String.(hmh_path |> to_string ~pretty:true) in
  let io =
    io.hm
    |> Fmt.fmt "# This file was generated by `hocc`; edit "
    |> Fmt.fmt (String.to_string ~pretty:true hmh_name)
    |> Fmt.fmt " rather than "
    |> Fmt.fmt (String.to_string ~pretty:true hm_name)
    |> Fmt.fmt "\n"
    |> Fmt.fmt "[:" |> Fmt.fmt directive_pathstr |> Fmt.fmt ":1]"
    |> (fun formatter ->
      match prelude with
      | Parse.Matter {token; _} -> begin
          let base = match token with
            | HmcToken {source; _} -> Hmc.Source.Slice.base source
            | HoccToken _ -> not_reached ()
          in
          let past = match hocc with
            | HmcToken _ -> not_reached ()
            | HoccToken {source; _} -> Hmc.Source.Slice.base source
          in
          let source = Hmc.Source.Slice.of_cursors ~base ~past in
          formatter |> Fmt.fmt (Hmc.Source.Slice.to_string source)
        end
      | MatterEpsilon -> formatter
    )
    |> Fmt.fmt "[:]"
    |> expand_hm_template indentation hm_template hocc_block spec
    |> (fun formatter ->
      match postlude with
      | Parse.Matter _ -> begin
          let base = Parse.postlude_base_of_hocc hocc_block in
          let past = match eoi with
            | HmcToken {source; _} -> Hmc.Source.Slice.past source
            | HoccToken _ -> not_reached ()
          in
          let source = Hmc.Source.Slice.of_cursors ~base ~past in
          formatter
          |> fmt_source_directive indentation source
          |> Fmt.fmt (Hmc.Source.Slice.to_string source)
        end
      | MatterEpsilon -> formatter
    )
    |> Io.with_hm io
  in
  io

let generate_mli _conf _hmhi _io spec =
  assert (Spec.conflicts spec = 0L);
  not_implemented "XXX"

let generate_ml _conf _hmh _io spec =
  assert (Spec.conflicts spec = 0L);
  not_implemented "XXX"
