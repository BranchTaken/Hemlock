open! Basis.Rudiments
open! Basis
open Bytes
open Format

let test () =
  printf "@[<h>";
  let rec fn strs = begin
    match strs with
    | [] -> ()
    | s :: strs' -> begin
        let bytes = of_string_slice (String.C.Slice.of_string s) in
        printf "hash_fold %a (%a) -> %a\n"
          pp bytes
          String.pp s
          Hash.pp (Hash.t_of_state
            (hash_fold bytes Hash.State.empty));
        fn strs'
      end
  end in
  let strs = [""; "hello"; "<"; "«"; "‡"; "𐆗"] in
  fn strs;
  printf "@]"

let _ = test ()
