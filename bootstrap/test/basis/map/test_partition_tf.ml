open! Basis.Rudiments
open! Basis
open MapTest
open Map

let test () =
  let test ks = begin
    let map = of_klist ks in
    let t_map, f_map = partition_tf map ~f:(fun (k, _) -> k % 2L = 0L) in
    let t_kvs = to_alist t_map in
    let f_kvs = to_alist f_map in
    File.Fmt.stdout
    |> (List.pp Uns.pp) ks
    |> Fmt.fmt " -> "
    |> (List.pp (pp_kv Uns.pp)) t_kvs
    |> Fmt.fmt " / "
    |> (List.pp (pp_kv Uns.pp)) f_kvs
    |> Fmt.fmt "\n"
    |> ignore
  end in
  Range.Uns.iter (0L =:< 7L) ~f:(fun n ->
    let ks = Array.(to_list (init (0L =:< n) ~f:(fun i -> i))) in
    test ks
  )

let _ = test ()
