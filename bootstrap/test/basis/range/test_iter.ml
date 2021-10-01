open! Basis.Rudiments
open! Basis
open Format

let test () =
  printf "@[<h>";
  printf "iter (0 .. 0) ->"; Range.iter (0L =:< 0L) ~f:(fun i -> printf " %Lu" i); printf "\n";
  printf "iter (0 .. 3) ->"; Range.iter (0L =:< 3L) ~f:(fun i -> printf " %Lu" i); printf "\n";
  printf "iter_right (0 .. 3) ->"; Range.iter_right (0L =:< 3L) ~f:(fun i -> printf " %Lu" i);
  printf "\n";
  printf "iter (253u8 .. 2u8) ->"; RangeH.U8.(iter (U8.kv 253L =:< U8.kv 2L)) ~f:(fun i ->
    printf " %a" U8.pp i
  );
  printf "\n";
  printf "iter (-2i .. 2i) ->"; RangeH.Sint.(iter (Sint.kv (-2L) =:< Sint.kv 2L)) ~f:(fun i ->
    printf " %a" Sint.pp i
  );
  printf "\n";
  printf "iter (0 ..= 0) ->"; RangeF.Uns.(iter (0L =:= 0L)) ~f:(fun i -> printf " %Lu" i);
  printf "\n";
  printf "iter (0 ..= 3) ->"; RangeF.Uns.(iter (0L =:= 3L)) ~f:(fun i -> printf " %Lu" i);
  printf "\n";

  printf "iter (253u8 ..= 2u8) ->"; RangeF.U8.(iter (U8.kv 253L =:= U8.kv 2L)) ~f:(fun i ->
    printf " %a" U8.pp i
  );
  printf "\n";

  printf "@]"

let _ = test ()
