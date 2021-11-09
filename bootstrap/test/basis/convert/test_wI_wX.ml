open! Basis.Rudiments
open! Basis
open Format

let test () =
  printf "@[<h>";
  List.iter I128.([of_string "0x8000_0000_0000_0000_0000_0000_0000_0000i128"; of_i64 (-1L);
    of_i64 0L; of_i64 1L; of_string "0x7fff_ffff_ffff_ffff_ffff_ffff_ffff_ffffi128"]) ~f:(fun u ->
    printf "extend_to_i512 %a -> %a\n"
      I128.xpp_x u
      I512.xpp_x (I128.extend_to_i512 u)
  );
  printf "\n";
  List.iter I512.([of_i64 (-1L); of_i64 0L; of_i64 1L;
    of_string "0x7fff_ffff_ffff_ffff_ffff_ffff_ffff_ffffi512";
    of_string "0x8000_0000_0000_0000_0000_0000_0000_0000i512";
    of_string "0xffff_ffff_ffff_ffff_ffff_ffff_ffff_ffffi512";
    of_string "0x1_0000_0000_0000_0000_0000_0000_0000_0000i512";
    of_string "0x1_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffffi512"]) ~f:(fun u ->
    printf "trunc_of_i512/narrow_of_i512_opt %a -> %a/%a\n"
      I512.xpp_x u
      I128.xpp_x (I128.trunc_of_i512 u)
      (Option.xpp I128.xpp) (I128.narrow_of_i512_opt u)
  );
  printf "@]"

let _ = test ()
