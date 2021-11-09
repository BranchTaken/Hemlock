open! Basis.Rudiments
open! Basis
open Format

let test () =
  printf "@[<h>";
  List.iter U8.([kv 0L; kv 1L; kv 127L; kv 128L; kv 255L]) ~f:(fun u ->
    printf "extend_to_u32 %a -> %a\n"
      U8.xpp u
      U32.xpp (U8.extend_to_u32 u)
  );
  printf "\n";
  List.iter U32.([kv 0L; kv 1L; kv 255L; kv 256L; kv 511L]) ~f:(fun u ->
    printf "trunc_of_u32/narrow_of_u32_opt %a -> %a/%a\n"
      U32.xpp u
      U8.xpp (U8.trunc_of_u32 u)
      (Option.xpp U8.xpp) (U8.narrow_of_u32_opt u)
  );
  printf "@]"

let _ = test ()
