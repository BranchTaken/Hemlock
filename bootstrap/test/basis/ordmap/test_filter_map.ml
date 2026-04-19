open! Basis.Rudiments
open! Basis
open! OrdmapTest
open Ordmap

let test () =
  let test arr = begin
    let ordmap = of_karray arr in
    let ordmap' = filter_map ordmap ~f:(fun (_k, v) ->
      match (Bitset.choose_hlt v) % 2L = 0L with
      | true -> Some (Bitset.to_nat v)
      | false -> None
    ) in
    let arr' = to_array ordmap' in
    File.Fmt.stdout
    |> (Array.pp Uns.pp) arr
    |> Fmt.fmt " -> "
    |> (Array.pp (pp_kv_pair (Nat.fmt ~alt:true ~radix:Hex))) arr'
    |> Fmt.fmt "\n"
    |> ignore
  end in
  Range.Uns.iter (0L =:< 7L) ~f:(fun n ->
    let arr = Array.init (0L =:< n) ~f:(fun i -> i) in
    test arr
  )

let _ = test ()
