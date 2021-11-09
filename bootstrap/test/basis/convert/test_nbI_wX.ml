open! Basis.Rudiments
open! Basis
open Format

let test () =
  printf "@[<h>";
  List.iter I16.([kv (-32768L); kv (-1L); kv 0L; kv 1L; kv 32767L]) ~f:(fun u ->
    printf "extend_to_i512 %a -> %a\n"
      I16.xpp u
      I512.xpp (I16.extend_to_i512 u)
  );
  printf "\n";
  List.iter I512.([of_i64 (-32769L); of_i64 (-32768L); of_i64 (-1L); of_i64 0L; of_i64 1L;
    of_i64 32767L; of_i64 32768L]) ~f:(fun u ->
    printf "trunc_of_i512/narrow_of_i512_opt %a -> %a/%a\n"
      I512.xpp u
      I16.xpp (I16.trunc_of_i512 u)
      (Option.xpp I16.xpp) (I16.narrow_of_i512_opt u)
  );
  printf "@]"

let _ = test ()
