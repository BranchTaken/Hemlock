open! Basis.Rudiments
open! Basis
open Hash

let u128_of_tup = RudimentsInt0.u128_of_tup

let test () =
  let hash_fold u128s t = begin
    State.Gen.init t
    |> State.Gen.fold_u128 Stdlib.(Int64.of_int (Array.length u128s)) ~f:(fun i ->
      Stdlib.(Array.get u128s Int64.(to_int i))
    )
    |> State.Gen.fini
  end in
  let rec test_hash_fold u128s_list = begin
    match u128s_list with
    | [] -> ()
    | u128s :: u128s_list' -> begin
        File.Fmt.stdout
        |> Fmt.fmt "hash_fold "
        |> (Array.pp pp) u128s
        |> Fmt.fmt " -> "
        |> pp (t_of_state State.(hash_fold u128s empty))
        |> Fmt.fmt "\n"
        |> ignore;
        test_hash_fold u128s_list'
      end
  end in
  (* These test inputs were manually verified against the reference MurmurHash3 implementation. *)
  let u128s_list = [
    [||];

    [|u128_of_tup (0x0123456789abcdefL, 0xfedcba9876543210L)|];

    [|U128.zero|];

    [|u128_of_tup (0xfedcba9876543210L, 0x0123456789abcdefL)|];

    [|u128_of_tup (0x0123456789abcdefL, 0xfedcba9876543210L);
      U128.zero;
      u128_of_tup (0xfedcba9876543210L, 0x0123456789abcdefL)|]
  ] in
  test_hash_fold u128s_list

let _ = test ()
