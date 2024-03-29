open! Basis.Rudiments
open! Basis
open MapTest
open Map

let test () =
  let test ks = begin
    let map = of_klist ks in
    let a_map, b_map = partition_map map ~f:(fun (k, v) ->
      match k % 2L = 0L with
      | true -> First (Uns.to_string v)
      | false -> Second (Uns.bits_to_sint v)
    ) in
    let a_kvs = to_alist a_map in
    let b_kvs = to_alist b_map in
    File.Fmt.stdout
    |> (List.pp Uns.pp) ks
    |> Fmt.fmt " -> "
    |> (List.pp (pp_kv String.pp)) a_kvs
    |> Fmt.fmt " / "
    |> (List.pp (pp_kv Sint.pp)) b_kvs
    |> Fmt.fmt "\n"
    |> ignore
  end in
  Range.Uns.iter (0L =:< 7L) ~f:(fun n ->
    let ks = Array.(to_list (init (0L =:< n) ~f:(fun i -> i))) in
    test ks
  )

let _ = test ()
