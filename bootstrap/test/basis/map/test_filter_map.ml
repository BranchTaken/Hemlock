open! Basis.Rudiments
open! Basis
open MapTest
open Map

let test () =
  let test ks = begin
    let map = of_klist ks in
    let map' = filter_map map ~f:(fun (_k, v) ->
      match (Bitset.choose_hlt v) % 2L = 0L with
      | true -> Some (Bitset.to_nat v)
      | false -> None
    ) in
    let kvs = to_alist map' in
    File.Fmt.stdout
    |> (List.pp Uns.pp) ks
    |> Fmt.fmt " -> "
    |> (List.pp (pp_kv (Nat.fmt ~alt:true ~radix:Hex))) kvs
    |> Fmt.fmt "\n"
    |> ignore
  end in
  Range.Uns.iter (0L =:< 7L) ~f:(fun n ->
    let ks = Array.(to_list (init (0L =:< n) ~f:(fun i -> i))) in
    test ks
  )

let _ = test ()
