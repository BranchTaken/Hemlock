open Basis
open Basis.Rudiments

module T = struct
  module Param = struct
    module U = struct
      type t = {
        binding: string option;
        symbol_name: string;
        qtype: QualifiedType.t;
        prod_param: Parse.prod_param option;
      }

      let hash_fold {binding; symbol_name; _} state =
        state
        |> Option.hash_fold String.hash_fold binding
        |> String.hash_fold symbol_name

      let cmp {binding=b0; symbol_name=s0; _} {binding=b1; symbol_name=s1; _} =
        let open Cmp in
        match Option.cmp String.cmp b0 b1 with
        | Lt -> Lt
        | Eq -> String.cmp s0 s1
        | Gt -> Gt

      let pp {binding; symbol_name; qtype; prod_param} formatter =
        formatter
        |> Fmt.fmt "{binding=" |> (Option.pp String.pp) binding
        |> Fmt.fmt "; symbol_name=" |> String.pp symbol_name
        |> Fmt.fmt "; qtype=" |> QualifiedType.pp qtype
        |> Fmt.fmt "; prod_param=" |> (Option.pp Parse.fmt_prod_param) prod_param
        |> Fmt.fmt "}"
    end
    include U
    include Identifiable.Make(U)

    let init ~binding ~symbol_name ~qtype ~prod_param =
      {binding; symbol_name; qtype; prod_param}
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
        Array.fold ~init:(Set.empty (module String))
          ~f:(fun bindings Param.{binding; prod_param; _} ->
            match binding with
            | None -> bindings
            | Some binding -> begin
                match Set.mem binding bindings with
                | true -> begin
                    match prod_param with
                    | Some ProdParamBinding {
                      ident=((IdentUident {uident=Uident {uident=binding_token}}) |
                             (IdentCident {cident=Cident {cident=binding_token}})); _} -> begin
                        let io =
                          io.err
                          |> Fmt.fmt "hocc: At "
                          |> Hmc.Source.Slice.pp (Scan.Token.source binding_token)
                          |> Fmt.fmt ": Duplicate parameter binding: "
                          |> Fmt.fmt (Hmc.Source.Slice.to_string (Scan.Token.source binding_token))
                          |> Fmt.fmt "\n"
                          |> Io.with_err io
                        in
                        Io.fatal io
                      end
                    | _ -> not_reached ()
                  end
                | false -> Set.insert binding bindings
              end
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
  end

  module Index = Uns
  type t = {
    index: Index.t;
    lhs: QualifiedType.t;
    rhs: Params.t;
    code: Parse.code option;
  }

  let hash_fold {index; _} state =
    Uns.hash_fold index state

  let cmp {index=index0; _} {index=index1; _} =
    Index.cmp index0 index1

  let pp {index; lhs; rhs; code} formatter =
    formatter
    |> Fmt.fmt "{index=" |> Index.pp index
    |> Fmt.fmt "; lhs=" |> QualifiedType.pp lhs
    |> Fmt.fmt "; rhs=" |> Params.pp rhs
    |> Fmt.fmt "; code=" |> (Option.pp Parse.fmt_code) code
    |> Fmt.fmt "}"
end
include T
include Identifiable.Make(T)

let init ~index ~lhs ~rhs ~code =
  {index; lhs; rhs; code}
