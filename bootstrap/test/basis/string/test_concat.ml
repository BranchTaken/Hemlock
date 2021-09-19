open! Basis.Rudiments
open! Basis
open String
open Format

let test () =
  printf "%s\n" (concat [""]);
  printf "%s\n" (concat [""; ""]);
  printf "%s\n" (concat [""; ""; ""]);

  printf "%s\n" (concat ~sep:":" [""]);
  printf "%s\n" (concat ~sep:":" [""; ""]);
  printf "%s\n" (concat ~sep:":" [""; ""; ""]);

  printf "%s\n" (concat ["a"]);
  printf "%s\n" (concat ["a"; ""]);
  printf "%s\n" (concat ["a"; "b"]);
  printf "%s\n" (concat ["a"; "b"; "c"]);

  printf "%s\n" (concat ~sep:":" ["a"; "b"; "c"]);
  printf "%s\n" (concat ~sep:".." ["a"; "b"; "c"]);
  printf "%s\n" (concat ~sep:":" ["ab"; "cd"; "ef"]);

  printf "%s\n" (concat ~sep:":" ["a"; ""; ""]);
  printf "%s\n" (concat ~sep:":" ["a"; "b"; ""]);
  printf "%s\n" (concat ~sep:":" ["a"; ""; "c"]);
  printf "%s\n" (concat ~sep:":" [""; "b"; "c"]);
  printf "%s\n" (concat ~sep:":" [""; ""; "c"]);

  printf "%s\n" (concat_rev ~sep:":" ["a"; "b"; "c"]);
  printf "%s\n" ("a" ^ "b" ^ "c")

let _ = test ()
