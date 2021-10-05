open! Basis.Rudiments
open! Basis
open Format

let test () =
  printf "@[<h>";
  List.iter U128.([of_u64 0L; of_u64 1L;
    of_string "0x7fff_ffff_ffff_ffff_ffff_ffff_ffff_ffffu128";
    of_string "0x8000_0000_0000_0000_0000_0000_0000_0000u128"]) ~f:(fun u ->
    printf "bits_to_i128/like_to_i128_opt %a -> %a/%a\n"
      U128.pp_x u
      I128.pp_x (U128.bits_to_i128 u)
      (Option.pp I128.pp_x) (U128.like_to_i128_opt u)
  );
  printf "\n";
  List.iter I128.([min_value; of_i64 (-1L); of_i64 0L; of_i64 1L; max_value]) ~f:(fun u ->
    printf "bits_of_i128/like_of_i128_opt %a -> %a/%a\n"
      I128.pp_x u
      U128.pp_x (U128.bits_of_i128 u)
      (Option.pp U128.pp) (U128.like_of_i128_opt u)
  );
  printf "@]"

let _ = test ()
