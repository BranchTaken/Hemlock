open! Basis.Rudiments
open! Basis
open Hash
open Format

let test () =
  let hash_fold u64s t = begin
    State.Gen.init t
    |> State.Gen.fold_u64 Stdlib.(Int64.of_int (Array.length u64s)) ~f:(fun i ->
      Stdlib.(Array.get u64s Int64.(to_int i))
    )
    |> State.Gen.fini
  end in
  let xpp_arr xpp_elm xppf arr = begin
    let rec fn arr i len = begin
      match i = len with
      | true -> ()
      | false -> begin
          if i > 0L then fprintf xppf ";@ ";
          fprintf xppf "%a" xpp_elm Stdlib.(Array.get arr (Int64.to_int i));
          fn arr (succ i) len
        end
    end in
    fprintf xppf "@[<h>[|";
    fn arr 0L Stdlib.(Int64.of_int (Array.length arr));
    fprintf xppf "|]@]"
  end in
  let xpp_u64 xppf u = begin
    Format.fprintf xppf "0x%016Lx" u
  end in
  printf "@[<h>";
  let rec test_hash_fold u64s_list = begin
    match u64s_list with
    | [] -> ()
    | u64s :: u64s_list' -> begin
        printf "hash_fold %a -> %a\n"
          (xpp_arr xpp_u64) u64s xpp (t_of_state State.(hash_fold u64s empty));
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
  test_hash_fold u64s_list;
  printf "@]"

let _ = test ()
