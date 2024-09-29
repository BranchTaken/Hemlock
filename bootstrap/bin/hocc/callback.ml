open Basis
open! Basis.Rudiments

module T = struct
  module Param = struct
    module U = struct
      type t = {
        pattern: string option;
        symbol_name: string;
        stype: SymbolType.t;
        prod_param: Parse.nonterm_prod_param option;
      }

      let hash_fold {pattern; symbol_name; _} state =
        state
        |> Option.hash_fold String.hash_fold pattern
        |> String.hash_fold symbol_name

      let cmp {pattern=p0; symbol_name=s0; _} {pattern=p1; symbol_name=s1; _} =
        let open Cmp in
        match Option.cmp String.cmp p0 p1 with
        | Lt -> Lt
        | Eq -> String.cmp s0 s1
        | Gt -> Gt

      let pp {pattern; symbol_name; stype; prod_param} formatter =
        formatter
        |> Fmt.fmt "{pattern=" |> (Option.pp String.pp) pattern
        |> Fmt.fmt "; symbol_name=" |> String.pp symbol_name
        |> Fmt.fmt "; stype=" |> SymbolType.pp stype
        |> Fmt.fmt "; prod_param=" |> (Option.pp Parse.fmt_prod_param) prod_param
        |> Fmt.fmt "}"
    end
    include U
    include Identifiable.Make(U)

    let init ~pattern ~symbol_name ~stype ~prod_param =
      {pattern; symbol_name; stype; prod_param}

    let fold_bindings ~init ~f {prod_param; _} =
      let rec fold_binding ~init ~f (Parse.Uident {token}) = begin
        f init token
      end and fold_field ~init ~f field = begin
      match field with
      | Parse.PatternFieldBinding {binding} -> fold_binding ~init ~f binding
      | PatternFieldPattern {pattern} -> fold_pattern ~init ~f pattern
    end and fold_fields ~init ~f fields = begin
      match fields with
      | Parse.PatternFieldsOne {field} -> fold_field ~init ~f field
      | PatternFieldsMulti {field; fields} -> begin
          let init = fold_field ~init ~f field in
          fold_fields ~init ~f fields
        end
    end and fold_pattern ~init ~f pattern = begin
      match pattern with
      | Parse.PatternUscore -> init
      | PatternBinding {binding} -> fold_binding ~init ~f binding
      | PatternPattern {pattern} -> fold_pattern ~init ~f pattern
      | PatternComma {pattern_a; pattern_b} -> begin
          let init = fold_pattern ~init ~f pattern_a in
          fold_pattern ~init ~f pattern_b
        end
      | PatternFields {fields} -> fold_fields ~init ~f fields
    end in
      match prod_param with
      | Some prod_param -> begin
          match prod_param with
          | ProdParamBinding {binding; _} -> fold_binding ~init ~f binding
          | ProdParamPattern {pattern; _} -> fold_pattern ~init ~f pattern
          | ProdParamFields {fields; _} -> fold_fields ~init ~f fields
          | ProdParam _ -> init
        end
      | None -> init

    let bindings t =
      fold_bindings ~init:(Set.empty (module String))
        ~f:(fun bindings token ->
          let binding = Scan.Token.source token |> Hmc.Source.Slice.to_string in
          Set.insert binding bindings
        ) t
  end

  module Params = struct
    module U = struct
      type t = Param.t array
      type elm = Param.t

      let hash_fold t state =
        state |> Array.hash_fold Param.hash_fold t

      let cmp t0 t1 =
        Array.cmp Param.cmp t0 t1

      let pp t formatter =
        formatter |> (Array.pp Param.pp) t

      let init io params =
        let merge_binding io token bindings = begin
          let token_source = Scan.Token.source token in
          let binding = token_source |> Hmc.Source.Slice.to_string in
          match Set.mem binding bindings with
          | true -> begin
              let io =
                io.err
                |> Fmt.fmt "hocc: At "
                |> Hmc.Source.Slice.pp token_source
                |> Fmt.fmt ": Duplicate parameter binding: "
                |> Fmt.fmt binding
                |> Fmt.fmt "\n"
                |> Io.with_err io
              in
              Io.fatal io
            end
          | false -> io, Set.insert binding bindings
        end in
        Array.fold ~init:(io, Set.empty (module String))
          ~f:(fun (io, bindings) param ->
            Param.fold_bindings ~init:(io, bindings) ~f:(fun (io, bindings) binding ->
              merge_binding io binding bindings) param
          ) params |> ignore;
        io, params

      module Cursor = struct
        module V = struct
          type t = Param.t Array.Cursor.t

          let cmp = Array.Cursor.cmp
          let hd = Array.Cursor.hd
          let tl = Array.Cursor.tl
          let pred = Array.Cursor.pred
          let succ = Array.Cursor.succ
          let lget = Array.Cursor.lget
          let rget = Array.Cursor.rget
          let prev = Array.Cursor.prev
          let next = Array.Cursor.next
        end
        include V
        include Cmpable.Make(V)
      end
      let length = Array.length
    end
    include U
    include Identifiable.Make(U)
    include Container.MakeMonoIndex(U)

    let to_array t = t
    let length = Array.length
    let range = Array.range
    let get = Array.get
    let map = Array.map

    let bindings t =
      Array.fold ~init:(Set.empty (module String)) ~f:(fun bindings param ->
        Set.union (Param.bindings param) bindings
      ) t
  end

  module Index = Uns
  type t = {
    index: Index.t;
    lhs_name: string;
    lhs_stype: SymbolType.t;
    rhs: Params.t;
    code: Parse.nonterm_code option;
  }

  let hash_fold {index; _} state =
    Uns.hash_fold index state

  let cmp {index=index0; _} {index=index1; _} =
    Index.cmp index0 index1

  let pp {index; lhs_name; lhs_stype; rhs; code} formatter =
    formatter
    |> Fmt.fmt "{index=" |> Index.pp index
    |> Fmt.fmt "; lhs_name=" |> String.pp lhs_name
    |> Fmt.fmt "; lhs_stype=" |> SymbolType.pp lhs_stype
    |> Fmt.fmt "; rhs=" |> Params.pp rhs
    |> Fmt.fmt "; code=" |> (Option.pp Parse.fmt_code) code
    |> Fmt.fmt "}"
end
include T
include Identifiable.Make(T)

let init ~index ~lhs_name ~lhs_stype ~rhs ~code =
  {index; lhs_name; lhs_stype; rhs; code}

let is_epsilon {rhs; _} =
  Params.is_empty rhs
