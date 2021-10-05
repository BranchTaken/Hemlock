open! Basis.Rudiments
open! Basis
open Format

let test () =
  printf "@[<h>";
  List.iter I16.([kv (-32768L); kv (-1L); kv 0L; kv 1L; kv 32767L]) ~f:(fun u ->
    printf "widen_to_u512_opt %a -> %a\n"
      I16.pp u
      (Option.pp U512.pp) (I16.widen_to_u512_opt u)
  );
  printf "\n";
  List.iter U512.([of_u64 0L; of_u64 1L; of_u64 32767L; of_u64 32768L; of_u64 65535L; of_u64 65536L;
    of_u64 131071L]) ~f:(fun u ->
    printf "trunc_of_u512/narrow_of_u512_opt %a -> %a/%a\n"
      U512.pp u
      I16.pp_x (I16.trunc_of_u512 u)
      (Option.pp I16.pp) (I16.narrow_of_u512_opt u)
  );
  printf "@]"

let _ = test ()
