open! Basis.Rudiments
open! Basis
open! OrdmapTest
open Ordmap

let test () =
  let test arr = begin
    let ordmap = of_karray arr in
    let t_ordmap, f_ordmap = partitioni_tf ordmap ~f:(fun i _kv -> i % 2L = 0L) in
    let t_arr = to_array t_ordmap in
    let f_arr = to_array f_ordmap in
    File.Fmt.stdout
    |> (Array.pp Uns.pp) arr
    |> Fmt.fmt " -> "
    |> (Array.pp (pp_kv_pair Uns.pp)) t_arr
    |> Fmt.fmt " / "
    |> (Array.pp (pp_kv_pair Uns.pp)) f_arr
    |> Fmt.fmt "\n"
    |> ignore
  end in
  Range.Uns.iter (0L =:< 7L) ~f:(fun n ->
    let arr = Array.init (0L =:< n) ~f:(fun i -> i * 10L) in
    test arr
  )

let _ = test ()
