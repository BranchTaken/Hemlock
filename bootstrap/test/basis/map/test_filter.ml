open! Basis.Rudiments
open! Basis
open MapTest
open Map

let test () =
  let test ks = begin
    let map = of_klist ks in
    let map' = filter map ~f:(fun (k, _) -> k % 2L = 0L) in
    let kvs = to_alist map' in
    File.Fmt.stdout
    |> (List.pp Uns.pp) ks
    |> Fmt.fmt " -> "
    |> (List.pp (pp_kv Uns.pp)) kvs
    |> Fmt.fmt "\n"
    |> ignore
  end in
  Range.Uns.iter (0L =:< 7L) ~f:(fun n ->
    let ks = Array.(to_list (init (0L =:< n) ~f:(fun i -> i))) in
    test ks
  )

let _ = test ()
