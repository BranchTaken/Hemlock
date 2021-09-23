open! Basis.Rudiments
open! Basis
open Hash
open Format

let test () =
  let hash_fold u8s t = begin
    State.Gen.init t
    |> State.Gen.fold_u8 Stdlib.(Int64.of_int (Array.length u8s)) ~f:(fun i ->
      Stdlib.(Array.get u8s Int64.(to_int i))
    )
    |> State.Gen.fini
  end in
  let pp_u8 ppf u = Format.fprintf ppf "0x%02Lx" u in
  let pp_arr pp_elm ppf arr = begin
    let rec fn arr i len = begin
      match i = len with
      | true -> ()
      | false -> begin
          if i > 0L then fprintf ppf ";@ ";
          fprintf ppf "%a" pp_elm Stdlib.(Array.get arr Int64.(to_int i));
          fn arr (succ i) len
        end
    end in
    fprintf ppf "@[<h>[|";
    fn arr 0L Stdlib.(Int64.of_int (Array.length arr));
    fprintf ppf "|]@]"
  end in
  printf "@[<h>";
  let rec test_hash_fold u8s_list = begin
    match u8s_list with
    | [] -> ()
    | u8s :: u8s_list' -> begin
        printf "hash_fold %a -> %a\n"
          (pp_arr pp_u8) u8s pp (t_of_state State.(hash_fold u8s empty));
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
  test_hash_fold u8s_list;
  printf "@]"

let _ = test ()
