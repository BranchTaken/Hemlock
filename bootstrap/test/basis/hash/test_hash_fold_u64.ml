open! Basis.Rudiments
open! Basis
open Hash

let test () =
  let hash_fold u64s t = begin
    State.Gen.init t
    |> State.Gen.fold_u64 Stdlib.(Int64.of_int (Array.length u64s)) ~f:(fun i ->
      Stdlib.(Array.get u64s Int64.(to_int i))
    )
    |> State.Gen.fini
  end in
  let rec test_hash_fold u64s_list = begin
    match u64s_list with
    | [] -> ()
    | u64s :: u64s_list' -> begin
        File.Fmt.stdout
        |> Fmt.fmt "hash_fold "
        |> (Array.pp (Uns.fmt ~alt:true ~zpad:true ~width:16L ~radix:Radix.Hex)) u64s
        |> Fmt.fmt " -> "
        |> pp (t_of_state State.(hash_fold u64s empty))
        |> Fmt.fmt "\n"
        |> ignore;
        test_hash_fold u64s_list'
      end
  end in
  (* These test inputs were manually verified against the reference MurmurHash3 implementation. *)
  let u64s_list = [
    [||];

    [|0x0123456789abcdefL; 0xfedcba9876543210L|];

    [|0L; 0L|];

    [|0xfedcba9876543210L; 0x0123456789abcdefL|];

    [|0x0123456789abcdefL; 0xfedcba9876543210L;
      0L; 0L;
      0xfedcba9876543210L; 0x0123456789abcdefL|]
  ] in
  test_hash_fold u64s_list

let _ = test ()
