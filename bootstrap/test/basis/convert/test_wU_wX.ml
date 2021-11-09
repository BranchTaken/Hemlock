open! Basis.Rudiments
open! Basis
open Format

let test () =
  printf "@[<h>";
  List.iter U128.([of_u64 0L; of_u64 1L; of_u64 127L; of_u64 128L; of_u64 255L]) ~f:(fun u ->
    printf "extend_to_i512 %a -> %a\n"
      U128.xpp u
      I512.xpp (U128.extend_to_i512 u)
  );
  printf "\n";
  List.iter I512.([of_i64 (-1L); of_i64 0L; of_i64 1L;
    of_string "0xffff_ffff_ffff_ffff_ffff_ffff_ffff_ffffi512";
    of_string "0x1_0000_0000_0000_0000_0000_0000_0000_0000i512"]) ~f:(fun u ->
    printf "trunc_of_i512/narrow_of_i512_opt %a -> %a/%a\n"
      I512.xpp_x u
      U128.xpp_x (U128.trunc_of_i512 u)
      (Option.xpp U128.xpp) (U128.narrow_of_i512_opt u)
  );
  printf "@]"

let _ = test ()
