open! Basis.Rudiments
open! Basis
open Hash

let test () =
  let hash_fold u8s t = begin
    State.Gen.init t
    |> State.Gen.fold_u8 Stdlib.(Int64.of_int (Array.length u8s)) ~f:(fun i ->
      Stdlib.(Array.get u8s Int64.(to_int i))
    )
    |> State.Gen.fini
  end in
  let rec test_hash_fold u8s_list = begin
    match u8s_list with
    | [] -> ()
    | u8s :: u8s_list' -> begin
        File.Fmt.stdout
        |> Fmt.fmt "hash_fold "
        |> (Array.pp (Uns.fmt ~alt:true ~zpad:true ~width:2L ~radix:Radix.Hex)) u8s
        |> Fmt.fmt " -> "
        |> pp (t_of_state State.(hash_fold u8s empty))
        |> Fmt.fmt "\n"
        |> ignore;
        test_hash_fold u8s_list'
      end
  end in
  (* These test inputs were manually verified against the reference MurmurHash3 implementation. *)
  let u8s_list = [
    [||];

    [|0L|];

    [|0xefL; 0xcdL; 0xabL; 0x89L; 0x67L; 0x45L; 0x23L; 0x01L;
      0x10L; 0x32L; 0x54L; 0x76L; 0x98L; 0xbaL; 0xdcL; 0xfeL|];

    [|0L; 0L; 0L; 0L; 0L; 0L; 0L; 0L;
      0L; 0L; 0L; 0L; 0L; 0L; 0L; 0L|];

    [|0x10L; 0x32L; 0x54L; 0x76L; 0x98L; 0xbaL; 0xdcL; 0xfeL;
      0xefL; 0xcdL; 0xabL; 0x89L; 0x67L; 0x45L; 0x23L; 0x01L|];

    [|0xefL; 0xcdL; 0xabL; 0x89L; 0x67L; 0x45L; 0x23L; 0x01L;
      0x10L; 0x32L; 0x54L; 0x76L; 0x98L; 0xbaL; 0xdcL; 0xfeL;

      0L; 0L; 0L; 0L; 0L; 0L; 0L; 0L;
      0L; 0L; 0L; 0L; 0L; 0L; 0L; 0L;

      0x10L; 0x32L; 0x54L; 0x76L; 0x98L; 0xbaL; 0xdcL; 0xfeL;
      0xefL; 0xcdL; 0xabL; 0x89L; 0x67L; 0x45L; 0x23L; 0x01L|]
  ] in
  test_hash_fold u8s_list

let _ = test ()
