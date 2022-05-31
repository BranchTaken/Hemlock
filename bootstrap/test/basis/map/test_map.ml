open! Basis.Rudiments
open! Basis
open MapTest
open Map

let test () =
  let test ks = begin
    let m = of_klist ks in
    let m' = map m ~f:(fun (_k, v) -> Uns.to_string v) in
    let kvs = to_alist m' in
    File.Fmt.stdout
    |> (List.pp Uns.pp) ks
    |> Fmt.fmt " -> "
    |> (List.pp (pp_kv String.pp)) kvs
    |> Fmt.fmt "\n"
    |> ignore
  end in
  Range.Uns.iter (0L =:< 7L) ~f:(fun n ->
    let ks = Array.(to_list (init (0L =:< n) ~f:(fun i -> i))) in
    test ks
  )

let _ = test ()
