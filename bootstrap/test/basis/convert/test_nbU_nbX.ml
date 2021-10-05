open! Basis.Rudiments
open! Basis
open Format

let test () =
  printf "@[<h>";
  List.iter U8.([kv 0L; kv 1L; kv 127L; kv 128L; kv 255L]) ~f:(fun u ->
    printf "extend_to_i32 %a -> %a\n"
      U8.pp u
      I32.pp (U8.extend_to_i32 u)
  );
  printf "\n";
  List.iter I32.([kv (-1L); kv 0L; kv 1L; kv 255L; kv 256L]) ~f:(fun u ->
    printf "trunc_of_i32/narrow_of_i32_opt %a -> %a/%a\n"
      I32.pp u
      U8.pp (U8.trunc_of_i32 u)
      (Option.pp U8.pp) (U8.narrow_of_i32_opt u)
  );
  printf "@]"

let _ = test ()
