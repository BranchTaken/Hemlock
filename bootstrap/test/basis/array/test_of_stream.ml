open! Basis.Rudiments
open! Basis
open Array
open Format

let test () =
  printf "@[<h>";
  let test stream = begin
    printf "of_stream[_rev] %a -> %a / %a\n"
      (Stream.pp Uns.pp) stream
      (pp Uns.pp) (of_stream stream)
      (pp Uns.pp) (of_stream_rev stream);
  end in
  let f list = begin
    match list with
    | [] -> None
    | hd :: tl -> Some (hd, tl)
  end in
  let streams = List.map ~f:(Stream.init_indef ~f) [
    [];
    [0];
    [0; 1];
    [0; 1; 2];
    [0; 1; 2; 3];
  ] in
  List.iter streams ~f:test;
  printf "@]"

let _ = test ()
