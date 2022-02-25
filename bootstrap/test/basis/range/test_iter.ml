open! Basis.Rudiments
open! Basis

let print s =
  File.Fmt.stdout |> Fmt.fmt s |> ignore

let test () =
  let f_uns = (fun i -> File.Fmt.stdout |> Fmt.fmt " " |> Uns.pp i |> ignore) in
  print "iter [0..0) ->"; Range.Uns.iter (0L =:< 0L) ~f:f_uns; print "\n";
  print "iter [0..3) ->"; Range.Uns.iter (0L =:< 3L) ~f:f_uns; print "\n";
  print "iter_right [0..3) ->"; Range.Uns.iter_right (0L =:< 3L) ~f:f_uns; print "\n";
  let f_u8 = (fun i -> File.Fmt.stdout |> Fmt.fmt " " |> U8.pp i |> ignore) in
  print "iter [253u8..2u8) ->"; Range.U8.(iter (U8.kv 253L =:< U8.kv 2L)) ~f:f_u8; print "\n";
  let f_sint = (fun i -> File.Fmt.stdout |> Fmt.fmt " " |> Sint.pp i |> ignore) in
  print "iter [-2i..2i) ->"; Range.Sint.(iter (Sint.kv (-2L) =:< Sint.kv 2L)) ~f:f_sint;
  print "\n";
  print "iter [0..0] ->"; (Range.Uns.iter (0L =:= 0L)) ~f:f_uns; print "\n";
  print "iter [0..3] ->"; (Range.Uns.iter (0L =:= 3L)) ~f:f_uns; print "\n";
  print "iter [253u8..2u8] ->"; Range.U8.(iter (U8.kv 253L =:= U8.kv 2L)) ~f:f_u8; print "\n"

let _ = test ()
