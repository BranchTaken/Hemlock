open! Basis.Rudiments
open! Basis
open Format

let test () =
  printf "@[<h>";
  List.iter U8.([kv 0L; kv 1L; kv 127L; kv 128L; kv 255L]) ~f:(fun u ->
    printf "bits_to_i8/like_to_i8_opt %a -> %a/%a\n"
      U8.pp u
      I8.pp (U8.bits_to_i8 u)
      (Option.pp I8.pp) (U8.like_to_i8_opt u)
  );
  printf "\n";
  List.iter I8.([kv (-128L); kv (-1L); kv 0L; kv 1L; kv 127L]) ~f:(fun i ->
    printf "bits_of_i8/like_of_i8_opt %a -> %a/%a\n"
      I8.pp i
      U8.pp (U8.bits_of_i8 i)
      (Option.pp U8.pp) (U8.like_of_i8_opt i)
  );
  printf "@]"

let _ = test ()
