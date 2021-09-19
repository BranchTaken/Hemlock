open! Basis.Rudiments
open! Basis
open Hash
open Format

let test () =
  let hash_fold u8s t = begin
    State.Gen.init t
    |> State.Gen.fold_u8 (Stdlib.Array.length u8s) ~f:(fun i ->
      Stdlib.Array.get u8s i
    )
    |> State.Gen.fini
  end in
  let pp_u8 ppf u = Format.fprintf ppf "0x%02x" u in
  let pp_arr pp_elm ppf arr = begin
    let rec fn arr i len = begin
      match i = len with
      | true -> ()
      | false -> begin
          if i > 0 then fprintf ppf ";@ ";
          fprintf ppf "%a" pp_elm (Stdlib.Array.get arr i);
          fn arr (succ i) len
        end
    end in
    fprintf ppf "@[<h>[|";
    fn arr 0 (Stdlib.Array.length arr);
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

    [|0|];

    [|0xef; 0xcd; 0xab; 0x89; 0x67; 0x45; 0x23; 0x01;
      0x10; 0x32; 0x54; 0x76; 0x98; 0xba; 0xdc; 0xfe|];

    [|0; 0; 0; 0; 0; 0; 0; 0;
      0; 0; 0; 0; 0; 0; 0; 0|];

    [|0x10; 0x32; 0x54; 0x76; 0x98; 0xba; 0xdc; 0xfe;
      0xef; 0xcd; 0xab; 0x89; 0x67; 0x45; 0x23; 0x01|];

    [|0xef; 0xcd; 0xab; 0x89; 0x67; 0x45; 0x23; 0x01;
      0x10; 0x32; 0x54; 0x76; 0x98; 0xba; 0xdc; 0xfe;

      0; 0; 0; 0; 0; 0; 0; 0;
      0; 0; 0; 0; 0; 0; 0; 0;

      0x10; 0x32; 0x54; 0x76; 0x98; 0xba; 0xdc; 0xfe;
      0xef; 0xcd; 0xab; 0x89; 0x67; 0x45; 0x23; 0x01|]
  ] in
  test_hash_fold u8s_list;
  printf "@]"

let _ = test ()
