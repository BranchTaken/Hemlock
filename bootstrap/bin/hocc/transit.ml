open Basis
open! Basis.Rudiments

module T = struct
  type t = {
    src: StateNub.Index.t;
    dst: StateNub.Index.t;
  }

  let hash_fold {src; dst} state =
    state
    |> StateNub.Index.hash_fold src
    |> StateNub.Index.hash_fold dst

  let cmp {src=s0; dst=d0} {src=s1; dst=d1} =
    let open Cmp in
    match StateNub.Index.cmp s0 s1 with
    | Lt -> Lt
    | Eq -> StateNub.Index.cmp d0 d1
    | Gt -> Gt

  let pp {src; dst} formatter =
    formatter
    |> Fmt.fmt "{src=" |> StateNub.Index.pp src
    |> Fmt.fmt "; dst=" |> StateNub.Index.pp dst
    |> Fmt.fmt "}"

end
include T
include Identifiable.Make(T)

let init ~src ~dst =
  {src; dst}

let cyclic {src; dst} =
  Uns.(src = dst)
