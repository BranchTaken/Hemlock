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

let indentation_of_hocc = function
  | Scan.Token.HmcToken _ -> not_reached ()
  | HoccToken {source; _} -> Hmc.Source.Slice.line_context source |> line_context_indentation

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

let mk_indent indentation =
  fun formatter -> formatter |> Fmt.fmt ~width:indentation ""

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

            include IdentifiableIntf.S with type t := t
          }

        algorithm: Algorithm.t
          [@@doc "Algorithm used to generate parser."]

        Assoc = {
            type t: t =
              | Left
              | Right

            include IdentifiableIntf.S with type t := t
          }

        Prec = {
            type t: t = {
                index: uns # Index in `precs` array.
                name: string
                assoc: option Assoc.t
                doms: Ordset.t uns Uns.cmper_witness # Indices in `precs` array of dominator
                                                     # precedences.
              }

            include IdentifiableIntf.S with type t := t
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
                callback: uns # Index of reduction callback in `Stack.Reduction.callbacks`.
              }

            include IdentifiableIntf.S with type t := t
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

            include IdentifiableIntf.S with type t := t
          }

        symbols: array Symbol.t
          [@@doc "Array of symbols, where each element's `index` field corresponds to the element's
          array index."]

        Lr0Item = {
            type t: t = {
                prod: Prod.t
                dot: uns
              }

            include IdentifiableIntf.S with type t := t
          }

        Lr1Item = {
            type t: t = {
                lr0item: Lr0Item.t
                follow: Ordset.t uns Uns.cmper_witness
              }

            include IdentifiableIntf.S with type t := t
          }

        Lr1Itemset = {
            type t: t = Ordmap.t Lr0Item.t Lr1Item.t Lr0Item.cmper_witness

            include IdentifiableIntf.S with type t := t
          }

        Lr1ItemsetClosure = {
            type t: t = {
                index: uns # Index of corresponding `State.t` in `states` array.
                kernel: Lr1Itemset.t
                added: Lr1Itemset.t
              }

            include IdentifiableIntf.S with type t := t
          }

        Action = {
            type t: t =
              | ShiftPrefix of uns # `states` index.
              | ShiftAccept of uns # `states` index.
              | Reduce of uns # `prods` index.

            include IdentifiableIntf.S with type t := t
          }

        State = {
            type t: t = {
                lr1ItemsetClosure: Lr1ItemsetClosure.t
                actions: Map.t uns Action.t Uns.cmper_witness
                gotos: Map.t uns uns Uns.cmper_witness
              }

            include IdentifiableIntf.S with type t := t
          }

        states: array State.t
          [@@doc "Array of CFSM states, where each element's `lr1ItemsetClosure.index` field
          corresponds to the element's array index."]
      }

    Token = {
        «tokens»

        include IdentifiableIntf.S with type t := t

        spec: t -> Spec.Symbol.t
      }

    Nonterm = {
        «nonterms»

        include IdentifiableIntf.S with type t := t

        spec: t -> Spec.Symbol.t
      }

    Symbol = {
        type t: t =
          | Token of Token.t
          | Nonterm of Nonterm.t

        include IdentifiableIntf.S with type t := t

        spec: t -> Spec.Symbol.t
      }

    State = {
        type t: t = uns

        include IdentifiableIntf.S with type t := t

        spec: t -> Spec.State.t
      }

    Stack = {
        module Elm : sig
            type t: t = {
                symbol: Symbol.t;
                state: State.t;
              }

            include IdentifiableIntf.S with type t := t
          end

        type t: t = Elm.t list

        pp >e: t -> Fmt.Formatter e >e-> Fmt.Formatter e
        fmt >e: ?alt:bool -> ?width:uns -> t -> Fmt.Formatter e >e-> Fmt.Formatter e

        Reduction = {
            type stack: stack = t
            type t: t
            type callback: callback = stack -> Symbol.t * stack

            include IdentifiableIntf.S with type t := t

            callbacks: array callback
              [@@doc "Array of reduction callback functions containing embedded parser code."]

            callback: t -> callback
          }

        shift: symbol:Symbol.t -> state:State.t -> t -> t
          [@@doc "Perform a shift."]

        reduce: reduction:Reduction.t -> t -> t
          [@@doc "Perform a reduction."]
      }

    Status = {
        type t: t =
          # `feed`/`step` may produce these variants; `next` fast-forwards over them.
          | ShiftPrefix of Token.t * State.t
          | ShiftAccept of Token.t * State.t
          | Reduce of Token.t * Stack.Reduction.t
          # Common variants.
          | Prefix # Valid parse prefix; more input needed.
          | Accept of Nonterm.t # Successful parse result.
          | Reject of Token.t # Syntax error due to unexpected token.

        include IdentifiableIntf.S with type t := t
      }

    type t: t = {
        stack: Stack.t
        status: Status.t
      }

    Start = {
        «starts»
      }

    feed: Token.t -> t -> t
      [@@doc "`feed token t` returns a result with status in {`ShiftPrefix`, `ShiftAccept`,
      `Reduce`, `Reject`}. `t.status` must be `Prefix`."]

    step: t -> t
      [@@doc "`step t` returns the result of applying one state transition to `t`. `t.status` must
      be in {`ShiftPrefix`, `ShiftAccept`, `Reduce`}."]

    next: Token.t -> t -> t
      [@@doc "`next token t` calls `feed token t` and fast-forwards via `step` calls to return a
      result with status in {`Prefix`, `Accept`, `Reject`}. `t.status` must be `Prefix`."]
  }|}

let expand_hmi_tokens symbols ~indentation formatter =
  let indent = mk_indent indentation in
  let fmt_tokens formatter = begin
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
              |> Fmt.fmt "  | "
              |> Fmt.fmt name
            end
          | {explicit_opt=Some {module_; type_}; _} -> begin
              formatter
              |> indent
              |> Fmt.fmt "  | "
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
  formatter
  |> indent |> Fmt.fmt "type t: t =\n"
  |> fmt_tokens

let expand_hmi_nonterms symbols ~indentation formatter =
  let indent = mk_indent indentation in
  let fmt_nonterms formatter = begin
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
              |> Fmt.fmt "  | "
              |> Fmt.fmt name
            end
          | {explicit_opt=Some {module_; type_}; _} -> begin
              formatter
              |> indent
              |> Fmt.fmt "  | "
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
  formatter
  |> indent |> Fmt.fmt "type t: t =\n"
  |> fmt_nonterms

let expand_hmi_starts symbols ~indentation formatter =
  let indent = mk_indent indentation in
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
                |> (fun formatter ->
                  let indentation = indentation + 4L in
                  let indent = mk_indent indentation in
                  formatter
                  |> indent |> Fmt.fmt "boi: t\n"
                )
                |> indent |> Fmt.fmt "  }"
              ),
              false
            end
      )
    ) symbols
  in
  formatter

let expand_hmi_template template_indentation template Spec.{symbols; _} formatter =
  let expanders = Map.of_alist (module String) [
    ("«tokens»", expand_hmi_tokens symbols);
    ("«nonterms»", expand_hmi_nonterms symbols);
    ("«starts»", expand_hmi_starts symbols)
  ] in
  formatter |> expand ~template_indentation template expanders

let generate_hmi conf Parse.(Hmhi {prelude; hocc_; postlude; eoi}) io spec =
  assert (Spec.conflicts spec = 0L);
  let indentation = indentation_of_hocc hocc_ in
  let module_name = module_name conf in
  let hmhi_name = module_name ^ ".hmhi" in
  let hmhi_path = Path.(join [Conf.srcdir conf; of_string hmhi_name] |> to_string_replace) in
  let directive_pathstr = String.(hmhi_path |> to_string ~pretty:true) in
  let io =
    io.hmi
    |> Fmt.fmt "# This file was generated by `hocc` based on "
    |> Fmt.fmt (String.to_string ~pretty:true hmhi_name)
    |> Fmt.fmt "\n"
    |> Fmt.fmt "[:" |> Fmt.fmt directive_pathstr |> Fmt.fmt ":1]"
    |> (fun formatter ->
      match prelude with
      | Parse.Matter {token_; _} -> begin
          let base = match token_ with
            | HmcToken {source; _} -> Hmc.Source.Slice.base source
            | HoccToken _ -> not_reached ()
          in
          let past = match hocc_ with
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
          let base = match hocc_ with
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
            T = {
                type t: t =
                  | Lr1
                  | Ielr1
                  | Pgm1
                  | Lalr1

                index = function
                  | Lr1 -> 0
                  | Ielr1 -> 1
                  | Pgm1 -> 2
                  | Lalr1 -> 3

                hash_fold t state =
                    state |> Uns.hash_fold (index t)

                cmp t0 t1 =
                    Uns.cmp (index t0) (index t1)

                to_string = function
                  | Lr1 -> "Lr1"
                  | Ielr1 -> "Ielr1"
                  | Pgm1 -> "Pgm1"
                  | Lalr1 -> "Lalr1"

                pp t formatter =
                    formatter |> Fmt.fmt (to_string t)
              }
            include T
            include Identifiable.Make(T)
          }

        «algorithm»

        Assoc = {
            T = {
                type t: t =
                  | Left
                  | Right

                index = function
                  | Left -> 0
                  | Right -> 1

                hash_fold t state =
                    state |> Uns.hash_fold (index t)

                cmp t0 t1 =
                    Uns.cmp (index t0) (index t1)

                to_string = function
                  | Left -> "Left"
                  | Right -> "Right"

                pp t formatter =
                    formatter |> Fmt.fmt (to_string t)
              }
            include T
            include Identifiable.Make(T)
          }

        Prec = {
            T = {
                type t: t = {
                    index: uns
                    name: string
                    assoc: option Assoc.t
                    doms: Ordset.t uns Uns.cmper_witness
                  }

                index {index; _} =
                    index

                hash_fold t state =
                    state |> Uns.hash_fold (index t)

                cmp t0 t1 =
                    Uns.cmp (index t0) (index t1)

                pp {index; name; assoc; doms} formatter =
                    formatter
                      |> Fmt.fmt
                      "{%u=(^index
                      ^); %s=(^name
                      ^); %f(^Option.pp Assoc.pp^)=(^assoc
                      ^); %f(^Ordset.pp^)=(^doms
                      ^)}"
              }
            include T
            include Identifiable.Make(T)

            init ~index ~name ~assoc ~doms =
                {index; name; assoc; doms}
          }

        «precs»

        Prod = {
            T = {
                type t: t = {
                    index: uns
                    lhs_index: uns
                    rhs_indexes: array uns
                    prec: option Prec.t
                    callback: uns
                  }

                hash_fold {index; _} state =
                    Uns.hash_fold index state

                cmp {index=i0; _} {index=i1; _} =
                    Uns.cmp i0 i1

                pp {index; lhs_index; rhs_indexes; prec; callback} formatter =
                    formatter
                      |> Fmt.fmt
                      "{%u=(^index
                      ^); %u=(^lhs_index
                      ^); %f(^Array.pp Uns.pp^)=(^rhs_indexes
                      ^); %f(^Option.pp Prec.pp^)=(^prec
                      ^); %u=(^callback
                      ^)}"
              }
            include T
            include Identifiable.Make(T)

            init ~index ~lhs_index ~rhs_indexes ~prec ~callback =
                {index; lhs_index; rhs_indexes; prec; callback}
          }

        «prods»

        Symbol = {
            T = {
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
              }
            include T
            include Identifiable.Make(T)

            init ~index ~name ~prec ~alias ~start ~prods ~first ~follow =
                {index; name; prec; alias; start; prods; first; follow}
          }

        «symbols»

        Lr0Item = {
            T = {
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
              }
            include T
            include Identifiable.Make(T)

            init ~prod ~dot =
                {prod; dot}
          }

        Lr1Item = {
            T = {
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
                    formatter
                      |> Fmt.fmt "{%f(^Lr0Item.pp^)=(^lr0item^); %f(^Ordset.pp^)=(^follow^)}"
              }
            include T
            include Identifiable.Make(T)

            init ~lr0item ~follow =
                {lr0item; follow}
          }

        Lr1Itemset = {
            T = {
                type t: t = Ordmap.t Lr0Item.t Lr1Item.t Lr0Item.cmper_witness

                hash_fold = Ordmap.hash_fold Lr1Item.hash_fold
                cmp = Ordmap.cmp Lr1Item.cmp
                pp = Ordmap.pp Lr1Item.pp
              }
            include T
            include Identifiable.Make(T)

            empty = Ordmap.empty Lr0Item

            init = Ordmap.of_alist Lr0Item
          }

        Lr1ItemsetClosure = {
            T = {
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
              }
            include T
            include Identifiable.Make(T)

            init ~index ~kernel ~added =
                {index; kernel; added}
          }

        Action = {
            T = {
                type t: t =
                  | ShiftPrefix of uns
                  | ShiftAccept of uns
                  | Reduce of uns

                constructor_index = function
                  | ShiftPrefix _ -> 0
                  | ShiftAccept _ -> 1
                  | Reduce _ -> 2

                arg_index = function
                  | ShiftPrefix arg_index
                  | ShiftAccept arg_index
                  | Reduce arg_index -> arg_index

                hash_fold t state =
                    state
                      |> Uns.hash_fold (constructor_index t)
                      |> Uns.hash_fold (arg_index t)

                cmp t0 t1 =
                    let open Cmp
                    match Uns.cmp (constructor_index t0) (constructor_index t1) with
                      | Lt -> Lt
                      | Eq -> Uns.cmp (arg_index t0) (arg_index t1)
                      | Gt -> Gt

                to_string = function
                  | ShiftPrefix state_index -> "ShiftPrefix %u(^state_index^)"
                  | ShiftAccept state_index -> "ShiftAccept %u(^state_index^)"
                  | Reduce prod_index -> "Reduce %u(^prod_index^)"

                pp t formatter =
                    formatter |> Fmt.fmt (to_string t)
              }
            include T
            include Identifiable.Make(T)
          }

        State = {
            T = {
                type t: t = {
                    lr1ItemsetClosure: Lr1ItemsetClosure.t
                    actions: Map.t uns Action.t Uns.cmper_witness
                    gotos: Map.t uns uns Uns.cmper_witness
                  }

                hash_fold {lr1ItemsetClosure; _} state =
                    state |> Lr1ItemsetClosure.hash_fold lr1ItemsetClosure

                cmp {lr1ItemsetClosure=c0; _} {lr1ItemsetClosure=c1; _} =
                    Lr1ItemsetClosure.cmp c0 c1

                pp {lr1ItemsetClosure; actions; gotos} formatter =
                    formatter
                      |> Fmt.fmt
                      "{%f(^Lr1ItemsetClosure.pp^)=(^lr1ItemsetClosure
                      ^); %f(^Map.pp Action.pp^)=(^actions
                      ^); %f(^Map.pp Uns.pp^)=(^gotos
                      ^)}"
              }
            include T
            include Identifiable.Make(T)

            init ~lr1ItemsetClosure ~actions ~gotos =
                {lr1ItemsetClosure; actions; gotos}
          }

        «states»
      }

    Token = {
        T = {
            «tokens»

            hash_fold t state =
                state |> Uns.hash_fold (index t)

            cmp t0 t1 =
                Uns.cmp (index t0) (index t1)

            spec t =
                Array.get (index t) Spec.symbols

            pp t formatter =
                formatter
                  |> Spec.Symbol.pp (spec t)
          }
        include T
        include Identifiable.Make(T)
      }

    Nonterm = {
        T = {
            «nonterms»

            hash_fold t state =
                state |> Uns.hash_fold (index t)

            cmp t0 t1 =
                Uns.cmp (index t0) (index t1)

            spec t =
                Array.get (index t) Spec.symbols

            pp t formatter =
                formatter
                  |> Spec.Symbol.pp (spec t)
          }
        include T
        include Identifiable.Make(T)
      }

    Symbol = {
        T = {
            type t: t =
              | Token of Token.t
              | Nonterm of Nonterm.t

            index = function
              | Token token -> Token.index token
              | Nonterm nonterm -> Nonterm.index nonterm

            hash_fold t state =
                state |> Uns.hash_fold (index t)

            cmp t0 t1 =
                Uns.cmp (index t0) (index t1)

            spec = function
              | Token token -> Token.spec token
              | Nonterm nonterm -> Nonterm.spec nonterm

            pp t formatter =
                formatter
                  |> Spec.Symbol.pp (spec t)
          }
        include T
        include Identifiable.Make(T)
      }

    State = {
        T = {
            type t: t = uns

            hash_fold t state =
                state |> Uns.hash_fold t

            cmp t0 t1 =
                Uns.cmp t0 t1

            spec t =
                Array.get t Spec.states

            pp t formatter =
                formatter |> Uns.pp t
          }
        include T
        include Identifiable.Make(T)

        init state_index =
            state_index
      }

    Stack = {
        Elm = {
            T = {
                type t: t = {
                    symbol: Symbol.t;
                    state: State.t;
                  }

                hash_fold {symbol; state} hash_state =
                    hash_state
                      |> Symbol.hash_fold symbol
                      |> State.hash_fold state

                cmp {symbol=symbol0; state=state0} {symbol=symbol1; state=state1} =
                    let open Cmp
                    match State.cmp state0 state1 with
                      | Lt -> Lt
                      | Eq -> Symbol.cmp symbol0 symbol1
                      | Gt -> Gt

                let pp {symbol; state} formatter =
                    formatter |> Fmt.fmt "{%f(^Symbol.pp^)=(^symbol^); %f(^State.pp^)=(^state^)}"
              }
            include T
            include Identifiable.Make(T)

            init ~symbol ~state =
                {symbol; state}
          }

        type t: t = list Elm.t

        fmt ?(alt=false) ?(width=0) t formatter =
            formatter |> List.fmt ~alt ~width Elm.pp t

        pp t formatter =
            formatter |> fmt t

        Reduction = {
            T = {
                type stack: stack = t
                type t: t = uns
                type callback: callback = stack -> Symbol.t * stack

                hash_fold t state =
                    state |> Uns.hash_fold t

                cmp t0 t1 =
                    Uns.cmp t0 t1

                pp t formatter =
                    formatter |> Uns.pp t
              }
            include T
            include Identifiable.Make(T)

            «callbacks»

            callback t =
                Array.get t callbacks

            init callback_index =
                callback_index
          }

        shift ~symbol ~state t =
            (Elm.init ~symbol ~state) :: t

        # goto: Symbol.t -> t -> t
        goto symbol t =
            match t with
              | [] -> not_reached ()
              | Elm.{state; _} :: _ ->
                let symbol_index = Symbol.index symbol
                let Spec.State.{gotos; _} = Array.get state Spec.states
                let state' = Map.get_hlt symbol_index gotos |> State.init
                shift ~symbol ~state:state' t

        reduce ~reduction t =
            let callback = Reduction.callback reduction
            let symbol, t' = callback t
            goto symbol t'
      }

    Status = {
        T = {
            type t: t =
              | ShiftPrefix of Token.t * State.t
              | ShiftAccept of Token.t * State.t
              | Reduce of Token.t * Stack.Reduction.t
              | Prefix
              | Accept of Nonterm.t
              | Reject of Token.t

            let constructor_index = function
              | ShiftPrefix _ -> 0
              | ShiftAccept _ -> 1
              | Reduce _ -> 2
              | Prefix -> 3
              | Accept _ -> 4
              | Reject _ -> 5

            let hash_fold t state =
                state
                  |> Uns.hash_fold (constructor_index t)
                  |> fn hash_state ->
                    match t with
                      | ShiftPrefix (token, state)
                      | ShiftAccept (token, state) ->
                        hash_state |> State.hash_fold state |> Token.hash_fold token
                      | Reduce (token, reduction) ->
                        hash_state |> Stack.Reduction.hash_fold reduction |> Token.hash_fold token
                      | Prefix -> hash_state
                      | Accept nonterm -> hash_state |> Nonterm.hash_fold nonterm
                      | Reject token -> hash_state |> Token.hash_fold token

            let cmp t0 t1 =
                let open Cmp
                match Uns.cmp (constructor_index t0) (constructor_index t1) with
                  | Lt -> Lt
                  | Eq ->
                    match t0, t1 with
                      | ShiftPrefix (token0, state0), ShiftPrefix (token1, state1)
                      | ShiftAccept (token0, state0), ShiftAccept (token1, state1) ->
                        match State.cmp state0 state1 with
                          | Lt -> Lt
                          | Eq -> Token.cmp token0 token1
                          | Gt -> Gt
                      | Reduce (token0, reduction0), Reduce (token1, reduction1) ->
                        match Stack.Reduction.cmp reduction0 reduction1 with
                          | Lt -> Lt
                          | Eq -> Token.cmp token0 token1
                          | Gt -> Gt
                      | Prefix, Prefix -> Eq
                      | Accept nonterm0, Accept nonterm1 -> Nonterm.cmp nonterm0 nonterm1
                      | Reject token0, Reject token1 -> Token.cmp token0 token1
                      | _, _ -> not_reached ()
                  | Gt -> Gt

            pp t formatter =
                formatter
                  |> fn formatter ->
                    match t with
                      | ShiftPrefix (token, state) ->
                        formatter
                          |> Fmt.fmt "ShiftPrefix (%f(^Token.pp^)(^token^), %f(^State.pp^)(^state
                          ^))"
                      | ShiftAccept (token, state) ->
                        formatter
                          |> Fmt.fmt "ShiftAccept (%f(^Token.pp^)(^token^), %f(^State.pp^)(^state
                          ^))"
                      | Reduce reduction ->
                        formatter
                          |> Fmt.fmt "Reduce (%f(^Token.pp^)(^token^), %f(^Stack.Reduction.pp
                          ^)(^reduction^))"
                      | Prefix -> formatter |> Fmt.fmt "Prefix"
                      | Accept nonterm -> formatter |> Fmt.fmt "Accept %f(^Nonterm.pp^)(^nonterm^)"
                      | Reject token -> formatter |> Fmt.fmt "Reject %f(^Token.pp^)(^token^)"
          }
        include T
        include Identifiable.Make(T)
      }

    type t: t = {
        stack: Stack.t
        status: Status.t
      }

    Start = {
        «starts»
      }

    feed token = function
      | {stack={state; _} :: _; status=Prefix} as t ->
        let token_index = Token.index token
        let Spec.State.{actions; _} = Array.get state Spec.states
        let status = match Map.get token_index actions with
          | Some (Spec.Action.ShiftPrefix state') -> Status.ShiftPrefix (token, state')
          | Some (Spec.Action.ShiftAccept state') -> Status.ShiftAccept (token, state')
          | Some (Spec.Action.Reduce prod_index) ->
            let Spec.Prod.{callback=callback_index; _} = Array.get prod_index Spec.prods
            let reduction = Stack.Reduction.init callback_index
            Status.Reduce (token, reduction)
          | None -> Status.Reject token
        {t with status}
      | _ -> not_reached ()

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
              | Spec.Action.Reduce prod_index ->
                let Spec.Prod.{callback=callback_index; _} = Array.get prod_index Spec.prods
                let reduction = Stack.Reduction.init callback_index
                let stack = Stack.reduce ~reduction stack
                match stack with
                  | [] -> not_reached ()
                  | {symbol=Token _; _} :: _ -> not_reached ()
                  | {symbol=Nonterm nonterm; _} :: _ -> {stack=[]; status=Accept nonterm}
              | _ -> not_reached ()
          | Reduce (token, reduction) ->
            feed token {stack=Stack.reduce ~reduction stack; status=Prefix}
          | _ -> not_reached ()

    # walk: t -> t
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

let expand_hm_algorithm algorithm ~indentation formatter =
  let indent = mk_indent indentation in
  formatter
  |> indent |> Fmt.fmt "algorithm = Algorithm." |> Conf.pp_algorithm algorithm

let expand_hm_precs precs ~indentation formatter =
  let fmt_precs ~indentation formatter = begin
    let indent = mk_indent indentation in
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
                |> Fmt.fmt "of_list Uns "
                |> (Ordset.to_list doms |> List.pp Prec.Index.pp)
              end
          )
          |> Fmt.fmt ")"
        ),
        false
      ) precs
    in
    formatter
  end in
  let indent = mk_indent indentation in
  formatter
  |> indent |> Fmt.fmt "precs = [|\n"
  |> fmt_precs ~indentation:(indentation+4L) |> Fmt.fmt "\n"
  |> indent |> Fmt.fmt "  |]"

let expand_hm_prods prods ~indentation formatter =
  let fmt_prods ~indentation formatter = begin
    let indent = mk_indent indentation in
    let formatter, _first = Prods.fold ~init:(formatter, true)
      ~f:(fun (formatter, first) Prod.{index; lhs_index; rhs_indexes; prec; callback; _} ->
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
          |> Fmt.fmt " ~callback:" |> Callback.Index.pp callback.index
        ),
        false
      ) prods
    in
    formatter
  end in
  let indent = mk_indent indentation in
  formatter
  |> indent |> Fmt.fmt "prods = [|\n"
  |> fmt_prods ~indentation:(indentation+4L) |> Fmt.fmt "\n"
  |> indent |> Fmt.fmt "  |]"

let expand_hm_symbols symbols ~indentation formatter =
  let fmt_symbols ~indentation formatter = begin
    let indent = mk_indent indentation in
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
                formatter
                |> Fmt.fmt "Ordset.singleton Prod (Array.get "
                |> Prod.Index.pp index
                |> Fmt.fmt " prods)"
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
  let indent = mk_indent indentation in
  formatter
  |> indent |> Fmt.fmt "symbols = [|\n"
  |> fmt_symbols ~indentation:(indentation+4L) |> Fmt.fmt "\n"
  |> indent |> Fmt.fmt "  |]"

let expand_hm_lr1Itemset lr1itemset ~indentation formatter =
  let indent = mk_indent indentation in
  match Lr1Itemset.is_empty lr1itemset with
  | false -> begin
      formatter
      |> indent |> Fmt.fmt "Lr1Itemset.init [\n"
      |> (fun formatter ->
        let indentation = indentation + 4L in
        let indent = mk_indent indentation in
        Lr1Itemset.fold ~init:formatter
          ~f:(fun formatter {lr0item={prod={index=prod_index; _}; dot}; follow} ->
            formatter
            |> indent |> Fmt.fmt "(\n"
            |> (fun formatter ->
              let indentation = indentation + 4L in
              let indent = mk_indent indentation in
              formatter
              |> indent |> Fmt.fmt "let lr0item = Lr0Item.init ~prod:"
              |> Fmt.fmt "(Array.get " |> Prod.Index.pp prod_index |> Fmt.fmt " prods)"
              |> Fmt.fmt " ~dot:" |> Uns.pp dot |> Fmt.fmt "\n"
              |> indent |> Fmt.fmt "let lr1item = Lr1Item.init ~lr0item ~follow:\n"
              |> (fun formatter ->
                let indentation = indentation + 4L in
                let indent = mk_indent indentation in
                formatter
                |> indent |> Fmt.fmt "Ordset.of_list Uns " |> Ordset.pp follow |> Fmt.fmt "\n"
              )
              |> indent |> Fmt.fmt "lr0item, lr1item\n"
            )
            |> indent |> Fmt.fmt "  )\n"
          ) lr1itemset
      )
      |> indent |> Fmt.fmt "  ]\n"
    end
  | true -> formatter |> indent |> Fmt.fmt "Lr1Itemset.empty\n"

let expand_hm_states states ~indentation formatter =
  let fmt_states ~indentation formatter = begin
    let indent = mk_indent indentation in
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
          |> (fun formatter ->
            let indentation = indentation + 4L in
            let indent = mk_indent indentation in
            formatter
            |> indent |> Fmt.fmt "Lr1ItemsetClosure.init\n"
            |> indent |> Fmt.fmt "  ~index:"
            |> Lr1ItemsetClosure.Index.pp index |> Fmt.fmt "\n"
            |> indent |> Fmt.fmt "  ~kernel:\n"
            |> expand_hm_lr1Itemset kernel ~indentation:(indentation+4L)
            |> indent |> Fmt.fmt "  ~added:\n"
            |> expand_hm_lr1Itemset added ~indentation:(indentation+4L)
          )
          |> indent |> Fmt.fmt "  ~actions:\n"
          |> (fun formatter ->
            let indentation = indentation + 4L in
            let indent = mk_indent indentation in
            formatter
            |> indent |> Fmt.fmt "Map.of_alist Uns [\n"
            |> (fun formatter ->
              let indentation = indentation + 4L in
              let indent = mk_indent indentation in
              Ordmap.fold ~init:formatter ~f:(fun formatter (symbol_index, action_set) ->
                assert (Ordset.length action_set = 1L);
                let action = Ordset.choose_hlt action_set in
                formatter
                |> indent
                |> Symbol.Index.pp symbol_index
                |> Fmt.fmt ", Action."
                |> State.Action.pp action
                |> Fmt.fmt "\n"
              ) actions
            )
            |> indent |> Fmt.fmt "  ]\n"
          )
          |> indent |> Fmt.fmt "  ~gotos:\n"
          |> (fun formatter ->
            let indentation = indentation + 4L in
            let indent = mk_indent indentation in
            match Ordmap.is_empty gotos with
            | false -> begin
                formatter
                |> indent |> Fmt.fmt "Map.of_alist Uns [\n"
                |> (fun formatter ->
                  let indentation = indentation + 4L in
                  let indent = mk_indent indentation in
                  Ordmap.fold ~init:formatter ~f:(fun formatter (symbol_index, state_index) ->
                    formatter
                    |> indent
                    |> Symbol.Index.pp symbol_index
                    |> Fmt.fmt ", "
                    |> State.Index.pp state_index
                    |> Fmt.fmt "\n"
                  ) gotos
                )
                |> indent |> Fmt.fmt "  ]"
              end
            | true -> formatter |> indent |> Fmt.fmt "Map.empty Uns"
          )
        ),
        false
      ) states
    in
    formatter
  end in
  let indent = mk_indent indentation in
  formatter
  |> indent |> Fmt.fmt "states = [|\n"
  |> fmt_states ~indentation:(indentation+4L) |> Fmt.fmt "\n"
  |> indent |> Fmt.fmt "  |]"

let expand_hm_token_type symbols ~indentation formatter =
  let indent = mk_indent indentation in
  let fmt_tokens formatter = begin
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
              |> Fmt.fmt "  | "
              |> Fmt.fmt name
            end
          | {explicit_opt=Some {module_; type_}; _} -> begin
              formatter
              |> indent
              |> Fmt.fmt "  | "
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
  formatter
  |> indent |> Fmt.fmt "type t: t =\n"
  |> fmt_tokens

let expand_hm_token_index symbols ~indentation formatter =
  let indent = mk_indent indentation in
  let fmt_token_indexes formatter = begin
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
          |> Fmt.fmt "  | "
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
  formatter
  |> indent |> Fmt.fmt "index = function\n"
  |> fmt_token_indexes

let expand_hm_tokens symbols ~indentation formatter =
  formatter
  |> expand_hm_token_type symbols ~indentation |> Fmt.fmt "\n"
  |> Fmt.fmt "\n"
  |> expand_hm_token_index symbols ~indentation

let expand_hm_nonterm_type symbols ~indentation formatter =
  let indent = mk_indent indentation in
  let fmt_nonterms formatter = begin
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
              |> Fmt.fmt "  | "
              |> Fmt.fmt name
            end
          | {explicit_opt=Some {module_; type_}; _} -> begin
              formatter
              |> indent
              |> Fmt.fmt "  | "
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
  formatter
  |> indent |> Fmt.fmt "type t: t =\n"
  |> fmt_nonterms

let expand_hm_nonterm_index symbols ~indentation formatter =
  let indent = mk_indent indentation in
  let fmt_nonterm_indexes formatter = begin
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
          |> Fmt.fmt "  | "
          |> Fmt.fmt name
          |> Fmt.fmt " _ -> "
          |> Uns.pp index
        ),
        false
      ) symbols
    in
    formatter
  end in
  formatter
  |> indent |> Fmt.fmt "index = function\n"
  |> fmt_nonterm_indexes

let expand_hm_nonterms symbols ~indentation formatter =
  formatter
  |> expand_hm_nonterm_type symbols ~indentation |> Fmt.fmt "\n"
  |> Fmt.fmt "\n"
  |> expand_hm_nonterm_index symbols ~indentation

let expand_hm_callbacks hocc_block symbols callbacks ~indentation formatter =
  let fmt_callbacks ~indentation formatter = begin
    let indent = mk_indent indentation in
    let formatter, _first = Callbacks.fold ~init:(formatter, true)
      ~f:(fun (formatter, first) (Callback.{index; lhs_name; rhs; code; _} as callback) ->
        formatter
        |> (fun formatter ->
          match first with
          | true -> formatter
          | false -> formatter |> Fmt.fmt "\n"
        )
        |> (fun formatter ->
          formatter
          |> indent |> Fmt.fmt "(* " |> Callback.Index.pp index
          |> Fmt.fmt " *) "
          |> (fun formatter ->
            match Option.is_empty code with
            | false -> begin
                let underline = Codepoint.of_char '_' in
                let overline = Codepoint.kv 0x203eL (*'‾'*) in
                let code = Option.value_hlt code in
                let source = Parse.source_of_code hocc_block code in
                formatter
                |> Fmt.fmt "function\n"
                |> (fun formatter ->
                  let formatter, _first =
                    Callback.Params.fold_right ~init:(formatter, true)
                      ~f:(fun (formatter, first)
                        Callback.Param.{binding; symbol_name; _} ->
                        let is_token =
                          Symbols.symbol_of_name symbol_name symbols
                          |> Option.value_hlt
                          |> Symbol.is_token
                        in
                        let symbol_constructor = match is_token with
                          | true -> "Token"
                          | false -> "Nonterm"
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
                |> indent |> Fmt.fmt "  "
                |> Fmt.fmt (match Callback.is_epsilon callback with false -> ":: " | true -> "")
                |> Fmt.fmt "tl -> Symbol.Nonterm ("
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
                |> indent |> Fmt.fmt "  )), tl"
                |> (fun formatter ->
                  match Callback.is_epsilon callback with
                  | false ->
                    formatter |> Fmt.fmt "\n" |> indent |> Fmt.fmt "  | _ -> not_reached ()"
                  | true -> formatter
                )
              end
            | true -> formatter |> Fmt.fmt "fn _stack -> not_reached ()"
          )
        ),
        false
      ) callbacks
    in
    formatter
  end in
  let indent = mk_indent indentation in
  formatter
  |> indent |> Fmt.fmt "callbacks = [|\n"
  |> fmt_callbacks ~indentation:(indentation+4L) |> Fmt.fmt "\n"
  |> indent |> Fmt.fmt "  |]"

let expand_hm_starts symbols states ~indentation formatter =
  let indent = mk_indent indentation in
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
            |> (fun formatter ->
              let indentation = indentation + 4L in
              let indent = mk_indent indentation in
              formatter
              |> indent |> Fmt.fmt "boi = {\n"
              |> (fun formatter ->
                let indentation = indentation + 4L in
                let indent = mk_indent indentation in
                formatter
                |> indent |> Fmt.fmt "stack=[{\n"
                |> (fun formatter ->
                  let indentation = indentation + 4L in
                  let indent = mk_indent indentation in
                  formatter
                  |> indent |> Fmt.fmt "symbol=Token Token.EPSILON\n"
                  |> indent |> Fmt.fmt "state=State.init " |> State.(index state |> Index.pp)
                  |> Fmt.fmt "\n"
                )
                |> indent |> Fmt.fmt "  }]\n"
                |> indent |> Fmt.fmt "status=Prefix\n"
              )
              |> indent |> Fmt.fmt "  }\n"
            )
            |> indent |> Fmt.fmt "  }"
          ),
          false
        end
    ) symbols
  in
  formatter

let expand_hm_template template_indentation template hocc_block
    Spec.{algorithm; precs; symbols; prods; callbacks; states} formatter =
  let expanders = Map.of_alist (module String) [
    ("«algorithm»", expand_hm_algorithm algorithm);
    ("«precs»", expand_hm_precs precs);
    ("«prods»", expand_hm_prods prods);
    ("«symbols»", expand_hm_symbols symbols);
    ("«states»", expand_hm_states states);
    ("«tokens»", expand_hm_tokens symbols);
    ("«nonterms»", expand_hm_nonterms symbols);
    ("«callbacks»", expand_hm_callbacks hocc_block symbols callbacks);
    ("«starts»", expand_hm_starts symbols states)
  ] in
  formatter |> expand ~template_indentation template expanders

let generate_hm conf
    Parse.(Hmh {prelude; hocc_=(Hocc {hocc_; _} as hocc_block); postlude; eoi}) io spec =
  assert (Spec.conflicts spec = 0L);
  let indentation = indentation_of_hocc hocc_ in
  let module_name = module_name conf in
  let hmh_name = module_name ^ ".hmh" in
  let hmh_path = Path.(join [Conf.srcdir conf; of_string hmh_name] |> to_string_replace) in
  let directive_pathstr = String.(hmh_path |> to_string ~pretty:true) in
  let io =
    io.hm
    |> Fmt.fmt "# This file was generated by `hocc` based on "
    |> Fmt.fmt (String.to_string ~pretty:true hmh_name)
    |> Fmt.fmt "\n"
    |> Fmt.fmt "[:" |> Fmt.fmt directive_pathstr |> Fmt.fmt ":1]"
    |> (fun formatter ->
      match prelude with
      | Parse.Matter {token_; _} -> begin
          let base = match token_ with
            | HmcToken {source; _} -> Hmc.Source.Slice.base source
            | HoccToken _ -> not_reached ()
          in
          let past = match hocc_ with
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

(***************************************************************************************************
 * OCaml code generation. *)

(* Source directives thwart debugging generated code. This is only an issue for `hocc` development,
 * hence this unexposed knob. *)
let ml_source_comments = false

let fmt_ml_source_directive source formatter =
  (* The line directive (both its value and whether to output a terminating '\n') needs to be
   * adjusted according to whether the `hocc` keyword is immediately followed by a newline. *)
  let nl_termination = match (Hmc.Source.Slice.base source |> Hmc.Source.Cursor.next_opt) with
    | None -> true
    | Some (cp, _) -> Codepoint.(cp <> of_char '\n')
  in
  let directive_pathstr =
    Hmc.Source.Slice.container source
    |> Hmc.Source.path
    |> Option.value_hlt
    |> Path.to_string_hlt
  in
  let base = Hmc.Source.Slice.base source in
  let pos = Hmc.Source.Cursor.pos base in
  let line = Text.Pos.line pos in
  formatter
  |> Fmt.fmt "\n"
  |> Fmt.fmt (match ml_source_comments with false -> "" | true -> "(* ")
  |> Fmt.fmt "#" |> Uns.fmt (line + (Bool.to_uns (not nl_termination)))
  |> Fmt.fmt " " |> String.pp directive_pathstr
  |> Fmt.fmt (match ml_source_comments with false -> "" | true -> " *)")
  |> (fun formatter -> match nl_termination with
    | false -> formatter
    | true -> formatter |> Fmt.fmt "\n"
  )

let ml_uns_pp u formatter =
  formatter
  |> Uns.fmt ~alt:false u
  |> Fmt.fmt "L"

let mli_template = {|sig
    module Spec : sig
        module Algorithm : sig
            type t =
              | Lr1 (** LR(1) algorithm. *)
              | Ielr1 (** IELR(1) algorithm. *)
              | Pgm1 (** PGM(1) algorithm. *)
              | Lalr1 (** LALR(1) algorithm. *)

            include IdentifiableIntf.S with type t := t
          end

        val algorithm: Algorithm.t
          (** Algorithm used to generate parser. *)

        module Assoc : sig
            type t =
              | Left
              | Right

            include IdentifiableIntf.S with type t := t
          end

        module Prec : sig
            type t = {
                index: uns; (* Index in `precs` array. *)
                name: string;
                assoc: Assoc.t option;
                doms: (uns, Uns.cmper_witness) Ordset.t; (* Indices in `precs` array of dominator
                                                          * precedences. *)
              }

            include IdentifiableIntf.S with type t := t
          end

        val precs: Prec.t array
          (** Array of precedences, where each element's `index` field corresponds to the element's
              array index. *)

        module Prod : sig
            type t = {
                index: uns; (* Index in `prods` array. *)
                lhs_index: uns;
                rhs_indexes: uns array;
                prec: Prec.t option;
                callback: uns; (* Index of reduction callback in `Stack.Reduction.callbacks`. *)
              }

            include IdentifiableIntf.S with type t := t
          end

        val prods: Prod.t array
          (** Array of productions, where each element's `index` field corresponds to the element's
              array index. *)

        module Symbol : sig
            type t = {
                index: uns; (* Index in `symbols` array. *)
                name: string;
                prec: Prec.t option;
                alias: string option;
                start: bool;
                prods: (Prod.t, Prod.cmper_witness) Ordset.t; (* empty ≡ token *)
                first: (uns, Uns.cmper_witness) Ordset.t;
                follow: (uns, Uns.cmper_witness) Ordset.t;
              }

            include IdentifiableIntf.S with type t := t
          end

        val symbols: Symbol.t array
          (** Array of symbols, where each element's `index` field corresponds to the element's
              array index. *)

        module Lr0Item : sig
            type t = {
                prod: Prod.t;
                dot: uns;
              }

            include IdentifiableIntf.S with type t := t
          end

        module Lr1Item : sig
            type t = {
                lr0item: Lr0Item.t;
                follow: (uns, Uns.cmper_witness) Ordset.t;
              }

            include IdentifiableIntf.S with type t := t
          end

        module Lr1Itemset : sig
            type t = (Lr0Item.t, Lr1Item.t, Lr0Item.cmper_witness) Ordmap.t

            include IdentifiableIntf.S with type t := t
          end

        module Lr1ItemsetClosure : sig
            type t = {
                index: uns; (* Index of corresponding `State.t` in `states` array. *)
                kernel: Lr1Itemset.t;
                added: Lr1Itemset.t;
              }

            include IdentifiableIntf.S with type t := t
          end

        module Action : sig
            type t =
              | ShiftPrefix of uns (* `states` index. *)
              | ShiftAccept of uns (* `states` index. *)
              | Reduce of uns (* `prods` index. *)

            include IdentifiableIntf.S with type t := t
          end

        module State : sig
            type t = {
                lr1ItemsetClosure: Lr1ItemsetClosure.t;
                actions: (uns, Action.t, Uns.cmper_witness) Map.t;
                gotos: (uns, uns, Uns.cmper_witness) Map.t;
              }

            include IdentifiableIntf.S with type t := t
          end

        val states: State.t array
          (** Array of CFSM states, where each element's `lr1ItemsetClosure.index` field corresponds
              to the element's array index. *)
      end

    module Token : sig
        «tokens»

        include IdentifiableIntf.S with type t := t

        val spec: t -> Spec.Symbol.t
      end

    module Nonterm : sig
        «nonterms»

        include IdentifiableIntf.S with type t := t

        val spec: t -> Spec.Symbol.t
      end

    module Symbol : sig
        type t =
          | Token of Token.t
          | Nonterm of Nonterm.t

        include IdentifiableIntf.S with type t := t

        val spec: t -> Spec.Symbol.t
      end

    module State : sig
        type t = uns

        include IdentifiableIntf.S with type t := t

        val spec: t -> Spec.State.t
      end

    module Stack : sig
        module Elm : sig
            type t = {
                symbol: Symbol.t;
                state: State.t;
              }

            include IdentifiableIntf.S with type t := t
          end

        type t = Elm.t list

        val pp: t -> (module Fmt.Formatter) -> (module Fmt.Formatter)
        val fmt: ?alt:bool -> ?width:uns -> t -> (module Fmt.Formatter) -> (module Fmt.Formatter)

        module Reduction : sig
            type stack = t
            type t
            type callback = stack -> Symbol.t * stack

            include IdentifiableIntf.S with type t := t

            val callbacks: callback array
              (** Array of reduction callback functions containing embedded parser code. *)

            val callback: t -> callback
          end

        val shift: symbol:Symbol.t -> state:State.t -> t -> t
          (** Perform a shift. *)

        val reduce: reduction:Reduction.t -> t -> t
          (** Perform a reduction. *)
      end

    module Status : sig
        type t =
          (* `feed`/`step` may produce these variants; `next` fast-forwards over them. *)
          | ShiftPrefix of Token.t * State.t
          | ShiftAccept of Token.t * State.t
          | Reduce of Token.t * Stack.Reduction.t
          (* Common variants. *)
          | Prefix (** Valid parse prefix; more input needed. *)
          | Accept of Nonterm.t (** Successful parse result. *)
          | Reject of Token.t (** Syntax error due to unexpected token. *)

        include IdentifiableIntf.S with type t := t
      end

    type t = {
        stack: Stack.t;
        status: Status.t;
      }

    module Start : sig
        «starts»
      end

    val feed: Token.t -> t -> t
      (** `feed token t` returns a result with status in {`ShiftPrefix`, `ShiftAccept`, `Reduce`,
          `Reject`}. `t.status` must be `Prefix`. *)

    val step: t -> t
      (** `step t` returns the result of applying one state transition to `t`. `t.status` must be in
          {`ShiftPrefix`, `ShiftAccept`, `Reduce`}. *)

    val next: Token.t -> t -> t
      (** `next token t` calls `feed token t` and fast-forwards via `step` calls to return a result
          with status in {`Prefix`, `Accept`, `Reject`}. `t.status` must be `Prefix`. *)
  end|}

let expand_mli_tokens symbols ~indentation formatter =
  let indent = mk_indent indentation in
  let fmt_tokens formatter = begin
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
              |> Fmt.fmt "  | "
              |> Fmt.fmt name
            end
          | {explicit_opt=Some {module_; type_}; _} -> begin
              formatter
              |> indent
              |> Fmt.fmt "  | "
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
          | Some alias ->
            formatter |> Fmt.fmt " (* " |> String.fmt ~pretty:true alias |> Fmt.fmt " *)"
        ),
        false
      ) symbols
    in
    formatter
  end in
  formatter
  |> indent |> Fmt.fmt "type t =\n"
  |> fmt_tokens

let expand_mli_nonterms symbols ~indentation formatter =
  let indent = mk_indent indentation in
  let fmt_nonterms formatter = begin
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
              |> Fmt.fmt "  | "
              |> Fmt.fmt name
            end
          | {explicit_opt=Some {module_; type_}; _} -> begin
              formatter
              |> indent
              |> Fmt.fmt "  | "
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
  formatter
  |> indent |> Fmt.fmt "type t =\n"
  |> fmt_nonterms

let expand_mli_starts symbols ~indentation formatter =
  let indent = mk_indent indentation in
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
                |> indent |> Fmt.fmt "module " |> String.fmt name |> Fmt.fmt " : sig\n"
                |> (fun formatter ->
                  let indentation = indentation + 4L in
                  let indent = mk_indent indentation in
                  formatter
                  |> indent |> Fmt.fmt "val boi: t\n"
                )
                |> indent |> Fmt.fmt "  end"
              ),
              false
            end
      )
    ) symbols
  in
  formatter

let expand_mli_template template_indentation template Spec.{symbols; _} formatter =
  let expanders = Map.of_alist (module String) [
    ("«tokens»", expand_mli_tokens symbols);
    ("«nonterms»", expand_mli_nonterms symbols);
    ("«starts»", expand_mli_starts symbols)
  ] in
  formatter |> expand ~template_indentation template expanders

let generate_mli conf Parse.(Hmhi {prelude; hocc_; postlude; eoi}) io spec =
  assert (Spec.conflicts spec = 0L);
  let indentation = indentation_of_hocc hocc_ in
  let module_name = module_name conf in
  let hmhi_name = module_name ^ ".hmhi" in
  let io =
    io.mli
    |> Fmt.fmt "(* This file was generated by `hocc` based on "
    |> Fmt.fmt (String.to_string ~pretty:true hmhi_name)
    |> Fmt.fmt " *)\n"
    |> (fun formatter ->
      match prelude with
      | Parse.Matter {token_; _} -> begin
          let base = match token_ with
            | HmcToken {source; _} -> Hmc.Source.Slice.base source
            | HoccToken _ -> not_reached ()
          in
          let past = match hocc_ with
            | HmcToken _ -> not_reached ()
            | HoccToken {source; _} -> Hmc.Source.Slice.base source
          in
          let source = Hmc.Source.Slice.of_cursors ~base ~past in
          formatter |> Fmt.fmt (Hmc.Source.Slice.to_string source)
        end
      | MatterEpsilon -> formatter
    )
    |> expand_mli_template indentation mli_template spec
    |> (fun formatter ->
      match postlude with
      | Parse.Matter _ -> begin
          let base = match hocc_ with
            | HmcToken _ -> not_reached ()
            | HoccToken {source; _} -> Hmc.Source.Slice.past source
          in
          let past = match eoi with
            | HmcToken {source; _} -> Hmc.Source.Slice.past source
            | HoccToken _ -> not_reached ()
          in
          let source = Hmc.Source.Slice.of_cursors ~base ~past in
          formatter
          |> fmt_ml_source_directive source
          |> Fmt.fmt (Hmc.Source.Slice.to_string source)
        end
      | MatterEpsilon -> formatter
    )
    |> Io.with_mli io
  in
  io

let ml_template = {|struct
    module Spec = struct
        module Algorithm = struct
            module T = struct
                type t =
                  | Lr1
                  | Ielr1
                  | Pgm1
                  | Lalr1

                let index = function
                  | Lr1 -> 0L
                  | Ielr1 -> 1L
                  | Pgm1 -> 2L
                  | Lalr1 -> 3L

                let hash_fold t state =
                    state |> Uns.hash_fold (index t)

                let cmp t0 t1 =
                    Uns.cmp (index t0) (index t1)

                let to_string = function
                  | Lr1 -> "Lr1"
                  | Ielr1 -> "Ielr1"
                  | Pgm1 -> "Pgm1"
                  | Lalr1 -> "Lalr1"

                let pp t formatter =
                    formatter |> Fmt.fmt (to_string t)
              end
            include T
            include Identifiable.Make(T)
          end

        «algorithm»

        module Assoc = struct
            module T = struct
                type t =
                  | Left
                  | Right

                let index = function
                  | Left -> 0L
                  | Right -> 1L

                let hash_fold t state =
                    state |> Uns.hash_fold (index t)

                let cmp t0 t1 =
                    Uns.cmp (index t0) (index t1)

                let to_string = function
                  | Left -> "Left"
                  | Right -> "Right"

                let pp t formatter =
                    formatter |> Fmt.fmt (to_string t)
              end
            include T
            include Identifiable.Make(T)
          end

        module Prec = struct
            module T = struct
                type t = {
                    index: uns;
                    name: string;
                    assoc: Assoc.t option;
                    doms: (uns, Uns.cmper_witness) Ordset.t;
                  }

                let index {index; _} =
                    index

                let hash_fold t state =
                    state |> Uns.hash_fold (index t)

                let cmp t0 t1 =
                    Uns.cmp (index t0) (index t1)

                let pp {index; name; assoc; doms} formatter =
                    formatter
                      |> Fmt.fmt "{index=" |> Uns.pp index
                      |> Fmt.fmt "; name=" |> String.pp name
                      |> Fmt.fmt "; assoc=" |> Option.pp Assoc.pp assoc
                      |> Fmt.fmt "; doms=" |> Ordset.pp doms
                      |> Fmt.fmt "}"
              end
            include T
            include Identifiable.Make(T)

            let init ~index ~name ~assoc ~doms =
                {index; name; assoc; doms}
          end

        «precs»

        module Prod = struct
            module T = struct
                type t = {
                    index: uns;
                    lhs_index: uns;
                    rhs_indexes: uns array;
                    prec: Prec.t option;
                    callback: uns;
                  }

                let hash_fold {index; _} state =
                    Uns.hash_fold index state

                let cmp {index=i0; _} {index=i1; _} =
                    Uns.cmp i0 i1

                let pp {index; lhs_index; rhs_indexes; prec; callback} formatter =
                    formatter
                      |> Fmt.fmt "{index=" |> Uns.pp index
                      |> Fmt.fmt "; lhs_index=" |> Uns.pp lhs_index
                      |> Fmt.fmt "; rhs_indexes=" |> Array.pp Uns.pp rhs_indexes
                      |> Fmt.fmt "; prec=" |> Option.pp Prec.pp prec
                      |> Fmt.fmt "; callback=" |> Uns.pp callback
                      |> Fmt.fmt "}"
            end
            include T
            include Identifiable.Make(T)

            let init ~index ~lhs_index ~rhs_indexes ~prec ~callback =
                {index; lhs_index; rhs_indexes; prec; callback}
          end

        «prods»

        module Symbol = struct
            module T = struct
                type t = {
                    index: uns;
                    name: string;
                    prec: Prec.t option;
                    alias: string option;
                    start: bool;
                    prods: (Prod.t, Prod.cmper_witness) Ordset.t;
                    first: (uns, Uns.cmper_witness) Ordset.t;
                    follow: (uns, Uns.cmper_witness) Ordset.t;
                  }

                let hash_fold {index; _} state =
                    Uns.hash_fold index state

                let cmp {index=i0; _} {index=i1; _} =
                    Uns.cmp i0 i1

                let pp {index; name; prec; alias; start; prods; first; follow} formatter =
                    formatter
                      |> Fmt.fmt "{index=" |> Uns.pp index
                      |> Fmt.fmt "; name=" |> String.pp name
                      |> Fmt.fmt "; prec=" |> Option.pp Prec.pp prec
                      |> Fmt.fmt "; alias=" |> Option.pp String.pp alias
                      |> Fmt.fmt "; start=" |> Bool.pp start
                      |> Fmt.fmt "; prods=" |> Ordset.pp prods
                      |> Fmt.fmt "; first=" |> Ordset.pp first
                      |> Fmt.fmt "; follow=" |> Ordset.pp follow
                      |> Fmt.fmt "}"
              end
            include T
            include Identifiable.Make(T)

            let init ~index ~name ~prec ~alias ~start ~prods ~first ~follow =
                {index; name; prec; alias; start; prods; first; follow}
          end

        «symbols»

        module Lr0Item = struct
            module T = struct
                type t = {
                    prod: Prod.t;
                    dot: uns;
                  }

                let hash_fold {prod; dot} state =
                    state
                      |> Prod.hash_fold prod
                      |> Uns.hash_fold dot

                let cmp {prod=p0; dot=d0} {prod=p1; dot=d1} =
                    let open Cmp in
                    match Prod.cmp p0 p1 with
                      | Lt -> Lt
                      | Eq -> Uns.cmp d0 d1
                      | Gt -> Gt

                let pp {prod; dot} formatter =
                    formatter
                      |> Fmt.fmt "{prod=" |> Prod.pp prod
                      |> Fmt.fmt "; dot=" |> Uns.pp dot
                      |> Fmt.fmt "}"
            end
            include T
            include Identifiable.Make(T)

            let init ~prod ~dot =
                {prod; dot}
          end

        module Lr1Item = struct
            module T = struct
                type t = {
                    lr0item: Lr0Item.t;
                    follow: (uns, Uns.cmper_witness) Ordset.t;
                  }

                let hash_fold {lr0item; follow} state =
                    state
                      |> Lr0Item.hash_fold lr0item
                      |> Ordset.hash_fold follow

                let cmp {lr0item=l0; follow=f0} {lr0item=l1; follow=f1} =
                    let open Cmp in
                    match Lr0Item.cmp l0 l1 with
                      | Lt -> Lt
                      | Eq -> Ordset.cmp f0 f1
                      | Gt -> Gt

                let pp {lr0item; follow} formatter =
                    formatter
                      |> Fmt.fmt "{lr0item=" |> Lr0Item.pp lr0item
                      |> Fmt.fmt "; follow=" |> Ordset.pp follow
                      |> Fmt.fmt "}"
              end
            include T
            include Identifiable.Make(T)

            let init ~lr0item ~follow =
                {lr0item; follow}
          end

        module Lr1Itemset = struct
            module T = struct
                type t = (Lr0Item.t, Lr1Item.t, Lr0Item.cmper_witness) Ordmap.t

                let hash_fold t =
                    Ordmap.hash_fold Lr1Item.hash_fold t

                let cmp t0 t1 =
                    Ordmap.cmp Lr1Item.cmp t0 t1

                let pp = Ordmap.pp Lr1Item.pp
              end
            include T
            include Identifiable.Make(T)

            let empty = Ordmap.empty (module Lr0Item)

            let init = Ordmap.of_alist (module Lr0Item)
          end

        module Lr1ItemsetClosure = struct
            module T = struct
                type t = {
                    index: uns;
                    kernel: Lr1Itemset.t;
                    added: Lr1Itemset.t;
                  }

                let hash_fold {index; _} state =
                    state |> Uns.hash_fold index

                let cmp {index=i0; _} {index=i1; _} =
                    Uns.cmp i0 i1

                let pp {index; kernel; added} formatter =
                    formatter
                      |> Fmt.fmt "{index=" |> Uns.pp index
                      |> Fmt.fmt "; kernel=" |> Lr1Itemset.pp kernel
                      |> Fmt.fmt "; added=" |> Lr1Itemset.pp added
                      |> Fmt.fmt "}"
              end
            include T
            include Identifiable.Make(T)

            let init ~index ~kernel ~added =
                {index; kernel; added}
          end

        module Action = struct
            module T = struct
                type t =
                  | ShiftPrefix of uns
                  | ShiftAccept of uns
                  | Reduce of uns

                let constructor_index = function
                  | ShiftPrefix _ -> 0L
                  | ShiftAccept _ -> 1L
                  | Reduce _ -> 2L

                let arg_index = function
                  | ShiftPrefix arg_index
                  | ShiftAccept arg_index
                  | Reduce arg_index -> arg_index

                let hash_fold t state =
                    state
                      |> Uns.hash_fold (constructor_index t)
                      |> Uns.hash_fold (arg_index t)

                let cmp t0 t1 =
                    let open Cmp in
                    match Uns.cmp (constructor_index t0) (constructor_index t1) with
                      | Lt -> Lt
                      | Eq -> Uns.cmp (arg_index t0) (arg_index t1)
                      | Gt -> Gt

                let to_string = function
                  | ShiftPrefix state_index -> begin
                    String.Fmt.empty
                    |> Fmt.fmt "ShiftPrefix " |> Uns.pp state_index
                    |> Fmt.to_string
                  end
                  | ShiftAccept state_index -> begin
                    String.Fmt.empty
                    |> Fmt.fmt "ShiftAccept " |> Uns.pp state_index
                    |> Fmt.to_string
                  end
                  | Reduce prod_index -> begin
                    String.Fmt.empty
                    |> Fmt.fmt "Reduce " |> Uns.pp prod_index
                    |> Fmt.to_string
                  end

                let pp t formatter =
                    formatter |> Fmt.fmt (to_string t)
              end
            include T
            include Identifiable.Make(T)
          end

        module State = struct
            module T = struct
                type t = {
                    lr1ItemsetClosure: Lr1ItemsetClosure.t;
                    actions: (uns, Action.t, Uns.cmper_witness) Map.t;
                    gotos: (uns, uns, Uns.cmper_witness) Map.t;
                  }

                let hash_fold {lr1ItemsetClosure; _} state =
                    state |> Lr1ItemsetClosure.hash_fold lr1ItemsetClosure

                let cmp {lr1ItemsetClosure=c0; _} {lr1ItemsetClosure=c1; _} =
                    Lr1ItemsetClosure.cmp c0 c1

                let pp {lr1ItemsetClosure; actions; gotos} formatter =
                    formatter
                      |> Fmt.fmt "{lr1ItemsetClosure=" |> Lr1ItemsetClosure.pp lr1ItemsetClosure
                      |> Fmt.fmt "; actions=" |> Map.pp Action.pp actions
                      |> Fmt.fmt "; gotos=" |> Map.pp Uns.pp gotos
                      |> Fmt.fmt "}"
              end
            include T
            include Identifiable.Make(T)

            let init ~lr1ItemsetClosure ~actions ~gotos =
                {lr1ItemsetClosure; actions; gotos}
          end

        «states»
      end

    module Token = struct
        module T = struct
            «tokens»

            let hash_fold t state =
                state |> Uns.hash_fold (index t)

            let cmp t0 t1 =
                Uns.cmp (index t0) (index t1)

            let spec t =
                Array.get (index t) Spec.symbols

            let pp t formatter =
                formatter
                  |> Spec.Symbol.pp (spec t)
          end
        include T
        include Identifiable.Make(T)
      end

    module Nonterm = struct
        module T = struct
            «nonterms»

            let hash_fold t state =
                state |> Uns.hash_fold (index t)

            let cmp t0 t1 =
                Uns.cmp (index t0) (index t1)

            let spec t =
                Array.get (index t) Spec.symbols

            let pp t formatter =
                formatter
                  |> Spec.Symbol.pp (spec t)
          end
        include T
        include Identifiable.Make(T)
      end

    module Symbol = struct
        module T = struct
            type t =
              | Token of Token.t
              | Nonterm of Nonterm.t

            let index = function
              | Token token -> Token.index token
              | Nonterm nonterm -> Nonterm.index nonterm

            let hash_fold t state =
                state |> Uns.hash_fold (index t)

            let cmp t0 t1 =
                Uns.cmp (index t0) (index t1)

            let spec = function
              | Token token -> Token.spec token
              | Nonterm nonterm -> Nonterm.spec nonterm

            let pp t formatter =
                formatter
                  |> Spec.Symbol.pp (spec t)
          end
        include T
        include Identifiable.Make(T)
      end

    module State = struct
        module T = struct
            type t = uns

            let hash_fold t state =
                state |> Uns.hash_fold t

            let cmp t0 t1 =
                Uns.cmp t0 t1

            let spec t =
                Array.get t Spec.states

            let pp t formatter =
                formatter |> Uns.pp t
          end
        include T
        include Identifiable.Make(T)

        let init state_index =
            state_index
      end

    module Stack = struct
        module Elm = struct
            module T = struct
                type t = {
                    symbol: Symbol.t;
                    state: State.t;
                  }

                let hash_fold {symbol; state} hash_state =
                    hash_state
                      |> Symbol.hash_fold symbol
                      |> State.hash_fold state

                let cmp {symbol=symbol0; state=state0} {symbol=symbol1; state=state1} =
                    let open Cmp in
                    match State.cmp state0 state1 with
                      | Lt -> Lt
                      | Eq -> Symbol.cmp symbol0 symbol1
                      | Gt -> Gt

                let pp {symbol; state} formatter =
                    formatter
                      |> Fmt.fmt "{symbol=" |> Symbol.pp symbol
                      |> Fmt.fmt "; state=" |> State.pp state
                      |> Fmt.fmt "}"
              end
            include T
            include Identifiable.Make(T)

            let init ~symbol ~state =
                {symbol; state}
          end

        type t = Elm.t list

        let fmt ?(alt=false) ?(width=0L) t formatter =
            formatter |> List.fmt ~alt ~width Elm.pp t

        let pp t formatter =
            formatter |> fmt t

        module Reduction = struct
            module T = struct
                type stack = t
                type t = uns
                type callback = stack -> Symbol.t * stack

                let hash_fold t state =
                    state |> Uns.hash_fold t

                let cmp t0 t1 =
                    Uns.cmp t0 t1

                let pp t formatter =
                    formatter |> Uns.pp t
              end
            include T
            include Identifiable.Make(T)

            «callbacks»

            let callback t =
                Array.get t callbacks

            let init callback_index =
                callback_index
          end

        let shift ~symbol ~state t =
            (Elm.init ~symbol ~state) :: t

        (* val goto: Symbol.t -> t -> t *)
        let goto symbol t =
            match t with
              | [] -> not_reached ()
              | Elm.{state; _} :: _ ->
                let symbol_index = Symbol.index symbol in
                let Spec.State.{gotos; _} = Array.get state Spec.states in
                let state' = Map.get_hlt symbol_index gotos |> State.init in
                shift ~symbol ~state:state' t

        let reduce ~reduction t =
            let callback = Reduction.callback reduction in
            let symbol, t' = callback t in
            goto symbol t'
      end

    module Status = struct
        module T = struct
            type t =
              | ShiftPrefix of Token.t * State.t
              | ShiftAccept of Token.t * State.t
              | Reduce of Token.t * Stack.Reduction.t
              | Prefix
              | Accept of Nonterm.t
              | Reject of Token.t

            let constructor_index = function
              | ShiftPrefix _ -> 0L
              | ShiftAccept _ -> 1L
              | Reduce _ -> 2L
              | Prefix -> 3L
              | Accept _ -> 4L
              | Reject _ -> 5L

            let hash_fold t state =
                state
                  |> Uns.hash_fold (constructor_index t)
                  |> (fun hash_state ->
                    match t with
                      | ShiftPrefix (token, state)
                      | ShiftAccept (token, state) ->
                        hash_state |> State.hash_fold state |> Token.hash_fold token
                      | Reduce (token, reduction) ->
                        hash_state |> Stack.Reduction.hash_fold reduction |> Token.hash_fold token
                      | Prefix -> hash_state
                      | Accept nonterm -> hash_state |> Nonterm.hash_fold nonterm
                      | Reject token -> hash_state |> Token.hash_fold token
                  )

            let cmp t0 t1 =
                let open Cmp in
                match Uns.cmp (constructor_index t0) (constructor_index t1) with
                  | Lt -> Lt
                  | Eq -> begin
                    match t0, t1 with
                      | ShiftPrefix (token0, state0), ShiftPrefix (token1, state1)
                      | ShiftAccept (token0, state0), ShiftAccept (token1, state1) -> begin
                        match State.cmp state0 state1 with
                          | Lt -> Lt
                          | Eq -> Token.cmp token0 token1
                          | Gt -> Gt
                      end
                      | Reduce (token0, reduction0), Reduce (token1, reduction1)
                      -> begin
                        match Stack.Reduction.cmp reduction0 reduction1 with
                          | Lt -> Lt
                          | Eq -> Token.cmp token0 token1
                          | Gt -> Gt
                      end
                      | Prefix, Prefix -> Eq
                      | Accept nonterm0, Accept nonterm1 -> Nonterm.cmp nonterm0 nonterm1
                      | Reject token0, Reject token1 -> Token.cmp token0 token1
                      | _, _ -> not_reached ()
                  end
                  | Gt -> Gt

            let pp t formatter =
                formatter
                  |> (fun formatter ->
                    match t with
                      | ShiftPrefix (token, state) -> begin
                        formatter
                          |> Fmt.fmt "ShiftPrefix (" |> Token.pp token
                          |> Fmt.fmt ", " |> State.pp state
                          |> Fmt.fmt ")"
                      end
                      | ShiftAccept (token, state) -> begin
                        formatter
                          |> Fmt.fmt "ShiftAccept (" |> Token.pp token
                          |> Fmt.fmt ", " |> State.pp state
                          |> Fmt.fmt ")"
                      end
                      | Reduce (token, reduction) -> begin
                        formatter
                          |> Fmt.fmt "Reduce (" |> Token.pp token
                          |> Fmt.fmt ", " |> Stack.Reduction.pp reduction
                          |> Fmt.fmt ")"
                      end
                      | Prefix -> formatter |> Fmt.fmt "Prefix"
                      | Accept nonterm -> formatter |> Fmt.fmt "Accept " |> Nonterm.pp nonterm
                      | Reject token -> formatter |> Fmt.fmt "Reject " |> Token.pp token
                  )
          end
        include T
        include Identifiable.Make(T)
      end

    type t = {
        stack: Stack.t;
        status: Status.t;
      }

    module Start = struct
        «starts»
      end

    let feed token = function
      | {stack={state; _} :: _; status=Prefix} as t -> begin
        let token_index = Token.index token in
        let Spec.State.{actions; _} = Array.get state Spec.states in
        let status = match Map.get token_index actions with
          | Some (Spec.Action.ShiftPrefix state') -> Status.ShiftPrefix (token, state')
          | Some (Spec.Action.ShiftAccept state') -> Status.ShiftAccept (token, state')
          | Some (Spec.Action.Reduce prod_index) -> begin
            let Spec.Prod.{callback=callback_index; _} = Array.get prod_index Spec.prods in
            let reduction = Stack.Reduction.init callback_index in
            Status.Reduce (token, reduction)
          end
          | None -> Status.Reject token
        in
        {t with status}
      end
      | _ -> not_reached ()

    let step {stack; status} =
        let open Status in
        match status with
          | ShiftPrefix (token, state) ->
            {stack=Stack.shift ~symbol:(Token token) ~state stack; status=Prefix}
          | ShiftAccept (token, state) -> begin
            (* Shift, perform the ⊥ reduction, and extract the accepted symbol from the stack. *)
            let stack = Stack.shift ~symbol:(Token token) ~state stack in
            let pseudo_end_index = Token.index Token.PSEUDO_END in
            let Spec.State.{actions; _} = Array.get state Spec.states in
            match Map.get_hlt pseudo_end_index actions with
              | Spec.Action.Reduce prod_index -> begin
                let Spec.Prod.{callback=callback_index; _} = Array.get prod_index Spec.prods in
                let reduction = Stack.Reduction.init callback_index in
                let stack = Stack.reduce ~reduction stack in
                match stack with
                  | [] -> not_reached ()
                  | {symbol=Token _; _} :: _ -> not_reached ()
                  | {symbol=Nonterm nonterm; _} :: _ -> {stack=[]; status=Accept nonterm}
              end
              | _ -> not_reached ()
          end
          | Reduce (token, reduction) -> begin
            feed token {stack=Stack.reduce ~reduction stack; status=Prefix}
          end
          | _ -> not_reached ()

    (* val walk: t -> t *)
    let rec walk ({status; _} as t) =
        let open Status in
        match status with
          | ShiftPrefix _
          | ShiftAccept _
          | Reduce _ -> t |> step |> walk
          | Prefix
          | Accept _
          | Reject _ -> t

    let next token ({status; _} as t) =
        match status with
          | Status.Prefix -> t |> feed token |> walk
          | _ -> not_reached ()
  end|}

let expand_ml_algorithm algorithm ~indentation formatter =
  let indent = mk_indent indentation in
  formatter
  |> indent |> Fmt.fmt "let algorithm = Algorithm." |> Conf.pp_algorithm algorithm

let expand_ml_precs precs ~indentation formatter =
  let fmt_precs ~indentation formatter = begin
    let indent = mk_indent indentation in
    let formatter, _first = Precs.fold ~init:(formatter, true)
      ~f:(fun (formatter, first) Prec.{index; name; assoc; doms; _} ->
        formatter
        |> (fun formatter ->
          match first with
          | true -> formatter
          | false -> formatter |> Fmt.fmt ";\n"
        )
        |> (fun formatter ->
          formatter
          |> indent
          |> Fmt.fmt "Prec.init"
          |> Fmt.fmt " ~index:" |> ml_uns_pp index
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
            | 0L -> formatter |> Fmt.fmt "empty (module Uns)"
            | 1L -> begin
                formatter
                |> Fmt.fmt "singleton (module Uns) "
                |> (Ordset.choose_hlt doms |> ml_uns_pp)
              end
            | _ -> begin
                formatter
                |> Fmt.fmt "of_list (module Uns) "
                |> (Ordset.to_list doms |> List.pp ml_uns_pp)
              end
          )
          |> Fmt.fmt ")"
        ),
        false
      ) precs
    in
    formatter
  end in
  let indent = mk_indent indentation in
  formatter
  |> indent |> Fmt.fmt "let precs = [|\n"
  |> fmt_precs ~indentation:(indentation+4L) |> Fmt.fmt "\n"
  |> indent |> Fmt.fmt "  |]"

let expand_ml_prods prods ~indentation formatter =
  let fmt_prods ~indentation formatter = begin
    let indent = mk_indent indentation in
    let formatter, _first = Prods.fold ~init:(formatter, true)
      ~f:(fun (formatter, first) Prod.{index; lhs_index; rhs_indexes; prec; callback; _} ->
        formatter
        |> (fun formatter ->
          match first with
          | true -> formatter
          | false -> formatter |> Fmt.fmt ";\n"
        )
        |> (fun formatter ->
          formatter
          |> indent
          |> Fmt.fmt "Prod.init"
          |> Fmt.fmt " ~index:" |> ml_uns_pp index
          |> Fmt.fmt " ~lhs_index:" |> ml_uns_pp lhs_index
          |> Fmt.fmt " ~rhs_indexes:" |> Array.pp ml_uns_pp rhs_indexes
          |> Fmt.fmt "\n" |> indent |> Fmt.fmt "  ~prec:"
          |> (fun formatter ->
            match prec with
            | None -> formatter |> Fmt.fmt "None"
            | Some prec -> begin
                formatter
                |> Fmt.fmt "(Some (Array.get " |> ml_uns_pp prec.index |> Fmt.fmt " precs))"
              end
          )
          |> Fmt.fmt " ~callback:" |> ml_uns_pp callback.index
        ),
        false
      ) prods
    in
    formatter
  end in
  let indent = mk_indent indentation in
  formatter
  |> indent |> Fmt.fmt "let prods = [|\n"
  |> fmt_prods ~indentation:(indentation+4L) |> Fmt.fmt "\n"
  |> indent |> Fmt.fmt "  |]"

let expand_ml_symbols symbols ~indentation formatter =
  let fmt_symbols ~indentation formatter = begin
    let indent = mk_indent indentation in
    let formatter, _first_line = Symbols.symbols_fold ~init:(formatter, true)
      ~f:(fun (formatter, first_line)
        Symbol.{index; name; prec; alias; start; prods; first; follow; _} ->
        formatter
        |> (fun formatter ->
          match first_line with
          | true -> formatter
          | false -> formatter |> Fmt.fmt ";\n"
        )
        |> (fun formatter ->
          formatter
          |> indent
          |> Fmt.fmt "Symbol.init"
          |> Fmt.fmt " ~index:" |> ml_uns_pp index
          |> Fmt.fmt " ~name:" |> String.pp name
          |> Fmt.fmt "\n" |> indent |> Fmt.fmt "  ~prec:"
          |> (fun formatter ->
            match prec with
            | None -> formatter |> Fmt.fmt "None"
            | Some prec -> begin
                formatter
                |> Fmt.fmt "(Some (Array.get " |> ml_uns_pp prec.index |> Fmt.fmt " precs))"
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
            | 0L -> formatter |> Fmt.fmt "Ordset.empty (module Prod)"
            | 1L -> begin
                let Prod.{index; _} = Ordset.choose_hlt prods in
                formatter
                |> Fmt.fmt "Ordset.singleton (module Prod) (Array.get "
                |> ml_uns_pp index
                |> Fmt.fmt " prods)"
              end
            | _ -> begin
                formatter
                |> Fmt.fmt "Ordset.of_list (module Prod) "
                |> List.fmt ~alt:true ~width:indentation (fun Prod.{index; _} formatter ->
                  formatter
                  |> Fmt.fmt "Array.get " |> ml_uns_pp index |> Fmt.fmt " prods;"
                ) (Ordset.to_list prods)
              end
          )
          |> Fmt.fmt ")"
          |> Fmt.fmt " ~first:("
          |> (fun formatter ->
            match Ordset.length first with
            | 0L -> formatter |> Fmt.fmt "Ordset.empty (module Uns)"
            | 1L -> begin
                let symbol_index = Ordset.choose_hlt first in
                formatter
                |> Fmt.fmt "Ordset.singleton (module Uns) " |> ml_uns_pp symbol_index
              end
            | _ -> begin
                formatter
                |> Fmt.fmt "Ordset.of_list (module Uns) "
                |> List.fmt ml_uns_pp (Ordset.to_list first)
              end
          )
          |> Fmt.fmt ")"
          |> Fmt.fmt "\n" |> indent |> Fmt.fmt "  ~follow:("
          |> (fun formatter ->
            match Ordset.length follow with
            | 0L -> formatter |> Fmt.fmt "Ordset.empty (module Uns)"
            | 1L -> begin
                let symbol_index = Ordset.choose_hlt follow in
                formatter |> Fmt.fmt "Ordset.singleton (module Uns) " |> ml_uns_pp symbol_index
              end
            | _ -> begin
                formatter
                |> Fmt.fmt "Ordset.of_list (module Uns) "
                |> List.pp ml_uns_pp (Ordset.to_list follow)
              end
          )
          |> Fmt.fmt ")"
        ),
        false
      ) symbols
    in
    formatter
  end in
  let indent = mk_indent indentation in
  formatter
  |> indent |> Fmt.fmt "let symbols = [|\n"
  |> fmt_symbols ~indentation:(indentation+4L) |> Fmt.fmt "\n"
  |> indent |> Fmt.fmt "  |]"

let expand_ml_lr1Itemset lr1itemset ~indentation formatter =
  let indent = mk_indent indentation in
  match Lr1Itemset.is_empty lr1itemset with
  | false -> begin
      formatter
      |> indent |> Fmt.fmt "Lr1Itemset.init [\n"
      |> (fun formatter ->
        let indentation = indentation + 4L in
        let indent = mk_indent indentation in
        Lr1Itemset.fold ~init:formatter
          ~f:(fun formatter {lr0item={prod={index=prod_index; _}; dot}; follow} ->
            formatter
            |> indent |> Fmt.fmt "(\n"
            |> (fun formatter ->
              let indentation = indentation + 4L in
              let indent = mk_indent indentation in
              formatter
              |> indent |> Fmt.fmt "let lr0item = Lr0Item.init ~prod:"
              |> Fmt.fmt "(Array.get " |> ml_uns_pp prod_index |> Fmt.fmt " prods)"
              |> Fmt.fmt " ~dot:" |> ml_uns_pp dot |> Fmt.fmt " in\n"
              |> indent |> Fmt.fmt "let lr1item = Lr1Item.init ~lr0item ~follow:(\n"
              |> (fun formatter ->
                let indentation = indentation + 4L in
                let indent = mk_indent indentation in
                formatter
                |> indent |> Fmt.fmt "Ordset.of_list (module Uns) "
                |> (Ordset.to_list follow |> List.pp ml_uns_pp)
                |> Fmt.fmt "\n"
              )
              |> indent |> Fmt.fmt "  ) in\n"
              |> indent |> Fmt.fmt "lr0item, lr1item\n"
            )
            |> indent |> Fmt.fmt "  );\n"
          ) lr1itemset
      )
      |> indent |> Fmt.fmt "  ]\n"
    end
  | true -> formatter |> indent |> Fmt.fmt "Lr1Itemset.empty\n"

let expand_ml_states states ~indentation formatter =
  let fmt_states ~indentation formatter = begin
    let indent = mk_indent indentation in
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
          |> indent |> Fmt.fmt "(* " |> State.Index.pp index
          |> Fmt.fmt " *) State.init\n"
          |> indent |> Fmt.fmt "  ~lr1ItemsetClosure:(\n"
          |> (fun formatter ->
            let indentation = indentation + 4L in
            let indent = mk_indent indentation in
            formatter
            |> indent |> Fmt.fmt "Lr1ItemsetClosure.init\n"
            |> indent |> Fmt.fmt "  ~index:"
            |> ml_uns_pp index |> Fmt.fmt "\n"
            |> indent |> Fmt.fmt "  ~kernel:(\n"
            |> expand_ml_lr1Itemset kernel ~indentation:(indentation+4L)
            |> indent |> Fmt.fmt "  )\n"
            |> indent |> Fmt.fmt "  ~added:(\n"
            |> expand_ml_lr1Itemset added ~indentation:(indentation+4L)
            |> indent |> Fmt.fmt "  )\n"
          )
          |> indent |> Fmt.fmt "  )\n"
          |> indent |> Fmt.fmt "  ~actions:(\n"
          |> (fun formatter ->
            let indentation = indentation + 4L in
            let indent = mk_indent indentation in
            formatter
            |> indent |> Fmt.fmt "Map.of_alist (module Uns) [\n"
            |> (fun formatter ->
              let indentation = indentation + 4L in
              let indent = mk_indent indentation in
              Ordmap.fold ~init:formatter ~f:(fun formatter (symbol_index, action_set) ->
                assert (Ordset.length action_set = 1L);
                let action = Ordset.choose_hlt action_set in
                formatter
                |> indent
                |> Fmt.fmt "("
                |> ml_uns_pp symbol_index
                |> Fmt.fmt ", Action."
                |> State.Action.pp action
                |> Fmt.fmt "L);\n"
              ) actions
            )
            |> indent |> Fmt.fmt "  ]\n"
          )
          |> indent |> Fmt.fmt "  )\n"
          |> indent |> Fmt.fmt "  ~gotos:(\n"
          |> (fun formatter ->
            let indentation = indentation + 4L in
            let indent = mk_indent indentation in
            match Ordmap.is_empty gotos with
            | false -> begin
                formatter
                |> indent |> Fmt.fmt "Map.of_alist (module Uns) [\n"
                |> (fun formatter ->
                  let indentation = indentation + 4L in
                  let indent = mk_indent indentation in
                  Ordmap.fold ~init:formatter ~f:(fun formatter (symbol_index, state_index) ->
                    formatter
                    |> indent
                    |> Fmt.fmt "("
                    |> ml_uns_pp symbol_index
                    |> Fmt.fmt ", "
                    |> ml_uns_pp state_index
                    |> Fmt.fmt ");\n"
                  ) gotos
                )
                |> indent |> Fmt.fmt "  ]"
              end
            | true -> formatter |> indent |> Fmt.fmt "Map.empty (module Uns)"
          )
          |> Fmt.fmt "\n"
          |> indent |> Fmt.fmt "  );"
        ),
        false
      ) states
    in
    formatter
  end in
  let indent = mk_indent indentation in
  formatter
  |> indent |> Fmt.fmt "let states = [|\n"
  |> fmt_states ~indentation:(indentation+4L) |> Fmt.fmt "\n"
  |> indent |> Fmt.fmt "  |]"

let expand_ml_token_type symbols ~indentation formatter =
  let indent = mk_indent indentation in
  let fmt_tokens formatter = begin
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
              |> Fmt.fmt "  | "
              |> Fmt.fmt name
            end
          | {explicit_opt=Some {module_; type_}; _} -> begin
              formatter
              |> indent
              |> Fmt.fmt "  | "
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
          | Some alias ->
            formatter |> Fmt.fmt " (* " |> String.fmt ~pretty:true alias |> Fmt.fmt " *)"
        ),
        false
      ) symbols
    in
    formatter
  end in
  formatter
  |> indent |> Fmt.fmt "type t =\n"
  |> fmt_tokens

let expand_ml_token_index symbols ~indentation formatter =
  let indent = mk_indent indentation in
  let fmt_token_indexes formatter = begin
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
          |> Fmt.fmt "  | "
          |> Fmt.fmt name
          |> (fun formatter ->
            match qtype.explicit_opt with
            | None -> formatter
            | Some _ -> formatter |> Fmt.fmt " _"
          )
          |> Fmt.fmt " -> "
          |> ml_uns_pp index
        ),
        false
      ) symbols
    in
    formatter
  end in
  formatter
  |> indent |> Fmt.fmt "let index = function\n"
  |> fmt_token_indexes

let expand_ml_tokens symbols ~indentation formatter =
  formatter
  |> expand_ml_token_type symbols ~indentation |> Fmt.fmt "\n"
  |> Fmt.fmt "\n"
  |> expand_ml_token_index symbols ~indentation

let expand_ml_nonterm_type symbols ~indentation formatter =
  let indent = mk_indent indentation in
  let fmt_nonterms formatter = begin
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
              |> Fmt.fmt "  | "
              |> Fmt.fmt name
            end
          | {explicit_opt=Some {module_; type_}; _} -> begin
              formatter
              |> indent
              |> Fmt.fmt "  | "
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
  formatter
  |> indent |> Fmt.fmt "type t =\n"
  |> fmt_nonterms

let expand_ml_nonterm_index symbols ~indentation formatter =
  let indent = mk_indent indentation in
  let fmt_nonterm_indexes formatter = begin
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
          |> Fmt.fmt "  | "
          |> Fmt.fmt name
          |> Fmt.fmt " _ -> "
          |> ml_uns_pp index
        ),
        false
      ) symbols
    in
    formatter
  end in
  formatter
  |> indent |> Fmt.fmt "let index = function\n"
  |> fmt_nonterm_indexes

let expand_ml_nonterms symbols ~indentation formatter =
  formatter
  |> expand_ml_nonterm_type symbols ~indentation |> Fmt.fmt "\n"
  |> Fmt.fmt "\n"
  |> expand_ml_nonterm_index symbols ~indentation

let expand_ml_callbacks hocc_block symbols callbacks ~indentation formatter =
  let fmt_callbacks ~indentation formatter = begin
    let indent = mk_indent indentation in
    let formatter, _first = Callbacks.fold ~init:(formatter, true)
      ~f:(fun (formatter, first) (Callback.{index; lhs_name; rhs; code; _} as callback) ->
        formatter
        |> (fun formatter ->
          match first with
          | true -> formatter
          | false -> formatter |> Fmt.fmt ";\n"
        )
        |> (fun formatter ->
          formatter
          |> indent |> Fmt.fmt "(* " |> Callback.Index.pp index
          |> Fmt.fmt " *) "
          |> (fun formatter ->
            match Option.is_empty code with
            | false -> begin
                let underline = Codepoint.of_char '_' in
                let overline = Codepoint.kv 0x203eL (*'‾'*) in
                let code = Option.value_hlt code in
                let source = Parse.source_of_code hocc_block code in
                formatter
                |> Fmt.fmt "(function\n"
                |> (fun formatter ->
                  let formatter, _first =
                    Callback.Params.fold_right ~init:(formatter, true)
                      ~f:(fun (formatter, first)
                        Callback.Param.{binding; symbol_name; _} ->
                        let is_token =
                          Symbols.symbol_of_name symbol_name symbols
                          |> Option.value_hlt
                          |> Symbol.is_token
                        in
                        let symbol_constructor = match is_token with
                          | true -> "Token"
                          | false -> "Nonterm"
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
                              |> Fmt.fmt "Elm.{symbol=Symbol."
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
                |> indent |> Fmt.fmt "  "
                |> Fmt.fmt (match Callback.is_epsilon callback with false -> ":: " | true -> "")
                |> Fmt.fmt "tl -> Symbol.Nonterm ("
                |> Fmt.fmt lhs_name |> Fmt.fmt " (\n"
                |> indent
                |> String.fmt ~pad:underline ~just:Fmt.Left ~width:(98L - indentation) "  (*"
                |> Fmt.fmt "*)"
                |> fmt_ml_source_directive source
                |> Fmt.fmt (Hmc.Source.Slice.to_string source)
                |> Fmt.fmt "\n"
                |> indent
                |> String.fmt ~pad:overline ~just:Fmt.Left ~width:(98L - indentation) "  (*"
                |> Fmt.fmt "*)\n"
                |> indent |> Fmt.fmt "  )), tl\n"
                |> (fun formatter ->
                  match Callback.is_epsilon callback with
                  | false -> formatter |> indent |> Fmt.fmt "  | _ -> not_reached ()\n"
                  | true -> formatter
                )
                |> indent |> Fmt.fmt ")"
              end
            | true -> formatter |> Fmt.fmt "(fun _stack -> not_reached ())"
          )
        ),
        false
      ) callbacks
    in
    formatter
  end in
  let indent = mk_indent indentation in
  formatter
  |> indent |> Fmt.fmt "let callbacks = [|\n"
  |> fmt_callbacks ~indentation:(indentation+4L) |> Fmt.fmt "\n"
  |> indent |> Fmt.fmt "  |]"

let expand_ml_starts symbols states ~indentation formatter =
  let indent = mk_indent indentation in
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
            |> indent |> Fmt.fmt "module " |> String.fmt name |> Fmt.fmt " = struct\n"
            |> (fun formatter ->
              let indentation = indentation + 4L in
              let indent = mk_indent indentation in
              formatter
              |> indent |> Fmt.fmt "let boi = {\n"
              |> (fun formatter ->
                let indentation = indentation + 4L in
                let indent = mk_indent indentation in
                formatter
                |> indent |> Fmt.fmt "stack=[{\n"
                |> (fun formatter ->
                  let indentation = indentation + 4L in
                  let indent = mk_indent indentation in
                  formatter
                  |> indent |> Fmt.fmt "symbol=Token Token.EPSILON;\n"
                  |> indent |> Fmt.fmt "state=State.init " |> State.(index state |> ml_uns_pp)
                  |> Fmt.fmt ";\n"
                )
                |> indent |> Fmt.fmt "  }];\n"
                |> indent |> Fmt.fmt "status=Prefix;\n"
              )
              |> indent |> Fmt.fmt "  }\n"
            )
            |> indent |> Fmt.fmt "  end"
          ),
          false
        end
    ) symbols
  in
  formatter

let expand_ml_template template_indentation template hocc_block
    Spec.{algorithm; precs; symbols; prods; callbacks; states} formatter =
  let expanders = Map.of_alist (module String) [
    ("«algorithm»", expand_ml_algorithm algorithm);
    ("«precs»", expand_ml_precs precs);
    ("«prods»", expand_ml_prods prods);
    ("«symbols»", expand_ml_symbols symbols);
    ("«states»", expand_ml_states states);
    ("«tokens»", expand_ml_tokens symbols);
    ("«nonterms»", expand_ml_nonterms symbols);
    ("«callbacks»", expand_ml_callbacks hocc_block symbols callbacks);
    ("«starts»", expand_ml_starts symbols states)
  ] in
  formatter |> expand ~template_indentation template expanders

let generate_ml conf
    Parse.(Hmh {prelude; hocc_=(Hocc {hocc_; _} as hocc_block); postlude; eoi}) io spec =
  assert (Spec.conflicts spec = 0L);
  let indentation = indentation_of_hocc hocc_ in
  let module_name = module_name conf in
  let hmh_name = module_name ^ ".hmh" in
  let io =
    io.ml
    |> Fmt.fmt "(* This file was generated by `hocc` based on "
    |> Fmt.fmt (String.to_string ~pretty:true hmh_name)
    |> Fmt.fmt " *)\n"

    |> (fun formatter ->
      match prelude with
      | Parse.Matter {token_; _} -> begin
          let base = match token_ with
            | HmcToken {source; _} -> Hmc.Source.Slice.base source
            | HoccToken _ -> not_reached ()
          in
          let past = match hocc_ with
            | HmcToken _ -> not_reached ()
            | HoccToken {source; _} -> Hmc.Source.Slice.base source
          in
          let source = Hmc.Source.Slice.of_cursors ~base ~past in
          formatter |> Fmt.fmt (Hmc.Source.Slice.to_string source)
        end
      | MatterEpsilon -> formatter
    )
    |> expand_ml_template indentation ml_template hocc_block spec
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
          |> fmt_ml_source_directive source
          |> Fmt.fmt (Hmc.Source.Slice.to_string source)
        end
      | MatterEpsilon -> formatter
    )
    |> Io.with_ml io
  in
  io
