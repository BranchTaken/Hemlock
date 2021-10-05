open! Basis.Rudiments
open! Basis
open Format

let test () =
  printf "@[<h>";
  List.iter I16.([kv (-32768L); kv (-1L); kv 0L; kv 1L; kv 32767L]) ~f:(fun u ->
    printf "widen_to_u32_opt %a -> %a\n"
      I16.pp u
      (Option.pp U32.pp) (I16.widen_to_u32_opt u)
  );
  printf "\n";
  List.iter U32.([kv (-32769L); kv (-32768L); kv (-1L); kv 0L; kv 1L; kv 32767L; kv 32768L;
    kv 65535L; kv 65536L; kv 131071L]) ~f:(fun u ->
    printf "trunc_of_u32/narrow_of_u32_opt %a -> %a/%a\n"
      U32.pp_x u
      I16.pp_x (I16.trunc_of_u32 u)
      (Option.pp I16.pp) (I16.narrow_of_u32_opt u)
  );
  printf "@]"

let _ = test ()
