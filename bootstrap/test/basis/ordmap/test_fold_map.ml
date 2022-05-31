open! Basis.Rudiments
open! Basis
open! OrdmapTest
open Ordmap

let test () =
  let test arr = begin
    let ordmap = of_karray arr in
    let sum, ordmap' = fold_map ordmap ~init:0L ~f:(fun accum (_k, v) ->
      (accum + v), Uns.to_string v)
    in
    let arr' = to_array ordmap' in
    File.Fmt.stdout
    |> (Array.pp Uns.pp) arr
    |> Fmt.fmt " -> "
    |> Uns.pp sum
    |> Fmt.fmt ", "
    |> (Array.pp (pp_kv_pair String.pp)) arr'
    |> Fmt.fmt "\n"
    |> ignore
  end in
  Range.Uns.iter (0L =:< 7L) ~f:(fun n ->
    let arr = Array.init (0L =:< n) ~f:(fun i -> i) in
    test arr
  )

let _ = test ()
