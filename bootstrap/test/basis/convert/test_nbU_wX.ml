open! Basis.Rudiments
open! Basis
open Format

let test () =
  printf "@[<h>";
  List.iter U8.([kv 0L; kv 1L; kv 127L; kv 128L; kv 255L]) ~f:(fun u ->
    printf "extend_to_i512 %a -> %a\n"
      U8.pp u
      I512.pp (U8.extend_to_i512 u)
  );
  printf "\n";
  List.iter I512.([of_i64 (-1L); of_i64 0L; of_i64 1L; of_i64 255L; of_i64 256L]) ~f:(fun u ->
    printf "trunc_of_i512/narrow_of_i512_opt %a -> %a/%a\n"
      I512.pp u
      U8.pp (U8.trunc_of_i512 u)
      (Option.pp U8.pp) (U8.narrow_of_i512_opt u)
  );
  printf "@]"

let _ = test ()
