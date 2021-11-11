open! Basis.Rudiments
open! Basis
open! OrdmapTest
open Ordmap

let test () =
  let test arr = begin
    let ordmap = of_karray arr in
    let ordmap' = filter_map ordmap ~f:(fun (k, v) ->
      match k % 2L = 0L with
      | true -> Some (Uns.to_string v)
      | false -> None
    ) in
    let arr' = to_array ordmap' in
    File.Fmt.stdout
    |> (Array.pp Uns.pp) arr
    |> Fmt.fmt " -> "
    |> (Array.pp (pp_kv_pair String.pp)) arr'
    |> Fmt.fmt "\n"
    |> ignore
  end in
  Range.iter (0L =:< 7L) ~f:(fun n ->
    let arr = Array.init (0L =:< n) ~f:(fun i -> i) in
    test arr
  )

let _ = test ()
