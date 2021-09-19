open! Basis.Rudiments
open! Basis
open String
open Format

let test () =
  let s = "abcde\n" in
  printf "%s" s;
  printf "%s" (concat_map s ~f:(fun cp -> of_codepoint cp));
  printf "%s" (concat_map s ~f:(fun cp ->
    match cp with
    | cp when Codepoint.(cp = (kv 0x61)) -> "hello "
    | cp when Codepoint.(cp = (kv 0x64)) -> " there "
    | _ -> of_codepoint cp
  ))

let _ = test ()
