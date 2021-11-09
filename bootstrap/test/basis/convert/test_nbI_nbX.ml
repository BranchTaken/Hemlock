open! Basis.Rudiments
open! Basis
open Format

let test () =
  printf "@[<h>";
  List.iter I16.([kv (-32768L); kv (-1L); kv 0L; kv 1L; kv 32767L]) ~f:(fun u ->
    printf "extend_to_i32 %a -> %a\n"
      I16.xpp u
      I32.xpp (I16.extend_to_i32 u)
  );
  printf "\n";
  List.iter I32.([kv (-32769L); kv (-32768L); kv (-1L); kv 0L; kv 1L; kv 32767L; kv 32768L])
    ~f:(fun u ->
      printf "trunc_of_i32/narrow_of_i32_opt %a -> %a/%a\n"
        I32.xpp u
        I16.xpp (I16.trunc_of_i32 u)
        (Option.xpp I16.xpp) (I16.narrow_of_i32_opt u)
    );
  printf "@]"

let _ = test ()
