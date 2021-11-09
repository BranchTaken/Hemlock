open! Basis.Rudiments
open! Basis
open Format

let test () =
  printf "@[<h>";
  List.iter U128.([of_u64 0L; of_u64 1L;
    of_string "0xffff_ffff_ffff_ffff_ffff_ffff_ffff_ffffu128"]) ~f:(fun u ->
    printf "extend_to_u512 %a -> %a\n"
      U128.xpp_x u
      U512.xpp_x (U128.extend_to_u512 u)
  );
  printf "\n";
  List.iter U512.([of_u64 0L; of_u64 1L;
    of_string "0xffff_ffff_ffff_ffff_ffff_ffff_ffff_ffffu512";
    of_string "0x1_0000_0000_0000_0000_0000_0000_0000_0000u512"]) ~f:(fun u ->
    printf "trunc_of_u512/narrow_of_u512_opt %a -> %a/%a\n"
      U512.xpp_x u
      U128.xpp_x (U128.trunc_of_u512 u)
      (Option.xpp U128.xpp) (U128.narrow_of_u512_opt u)
  );
  printf "@]"

let _ = test ()
