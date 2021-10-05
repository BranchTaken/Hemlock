open! Basis.Rudiments
open! Basis
open Format

let test () =
  printf "@[<h>";
  List.iter I128.([of_string "0x8000_0000_0000_0000_0000_0000_0000_0000i128";
    of_i64 (-1L); of_i64 0L; of_i64 1L; of_string "0x7fff_ffff_ffff_ffff_ffff_ffff_ffff_ffffi128"])
    ~f:(fun u ->
      printf "widen_to_u512_opt %a -> %a\n"
        I128.pp_x u
        (Option.pp U512.pp_x) (I128.widen_to_u512_opt u)
    );
  printf "\n";
  List.iter U512.([of_u64 0L; of_u64 1L;
    of_string "0x7fff_ffff_ffff_ffff_ffff_ffff_ffff_ffffu512";
    of_string "0x8000_0000_0000_0000_0000_0000_0000_0000u512";
    of_string "0xffff_ffff_ffff_ffff_ffff_ffff_ffff_ffffu512";
    of_string "0x1_0000_0000_0000_0000_0000_0000_0000_0000u512";
    of_string "0x1_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffffu512"]) ~f:(fun u ->
    printf "trunc_of_u512/narrow_of_u512_opt %a -> %a/%a\n"
      U512.pp_x u
      I128.pp_x (I128.trunc_of_u512 u)
      (Option.pp I128.pp) (I128.narrow_of_u512_opt u)
  );
  printf "@]"

let _ = test ()
