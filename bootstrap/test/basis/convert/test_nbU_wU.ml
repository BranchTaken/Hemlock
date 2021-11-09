open! Basis.Rudiments
open! Basis
open Format

let test () =
  printf "@[<h>";
  List.iter U8.([kv 0L; kv 1L; kv 127L; kv 128L; kv 255L]) ~f:(fun u ->
    printf "extend_to_u512 %a -> %a\n"
      U8.xpp u
      U512.xpp (U8.extend_to_u512 u)
  );
  printf "\n";
  List.iter U512.([of_u64 0L; of_u64 1L; of_u64 255L; of_u64 256L; of_u64 511L]) ~f:(fun u ->
    printf "trunc_of_u512/narrow_of_u512_opt %a -> %a/%a\n"
      U512.xpp u
      U8.xpp (U8.trunc_of_u512 u)
      (Option.xpp U8.xpp) (U8.narrow_of_u512_opt u)
  );
  printf "@]"

let _ = test ()
