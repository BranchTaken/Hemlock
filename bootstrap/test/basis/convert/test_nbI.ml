open! Basis.Rudiments
open! Basis
open Format

let test () =
  printf "@[<h>";
  List.iter I16.([kv (-32768L); kv (-1L); kv 0L; kv 1L; kv 32767L]) ~f:(fun u ->
    printf "extend_to_uns %a -> %a\n"
      I16.pp u
      Uns.pp_x (I16.extend_to_uns u)
  );
  printf "\n";
  List.iter [0L; 1L; 32767L; 32768L; 65535L] ~f:(fun u ->
    printf "trunc_of_uns/narrow_of_uns_opt %a -> %a/%a\n"
      Uns.pp u
      I16.pp (I16.trunc_of_uns u)
      (Option.pp I16.pp) (I16.narrow_of_uns_opt u)
  );
  printf "\n";
  List.iter I16.([kv (-32768L); kv (-1L); kv 0L; kv 1L; kv 32767L]) ~f:(fun u ->
    printf "extend_to_sint %a -> %a\n"
      I16.pp u
      Sint.pp (I16.extend_to_sint u)
  );
  printf "\n";
  List.iter Sint.([kv (-32769L); kv (-32768L); kv (-1L); kv 0L; kv 1L; kv 32767L; kv 32768L;
    kv 65535L]) ~f:(fun i ->
    printf "trunc_of_sint/narrow_of_sint_opt %a -> %a/%a\n"
      Sint.pp i
      I16.pp (I16.trunc_of_sint i)
      (Option.pp I16.pp) (I16.narrow_of_sint_opt i)
  );

  printf "@]"

let _ = test ()
