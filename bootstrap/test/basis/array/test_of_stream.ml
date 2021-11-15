open! Basis.Rudiments
open! Basis
open Array

let test () =
  let test stream = begin
    File.Fmt.stdout
    |> Fmt.fmt "of_stream[_rev] "
    |> (Stream.pp Uns.pp) stream
    |> Fmt.fmt " -> "
    |> (pp Uns.pp) (of_stream stream)
    |> Fmt.fmt " / "
    |> (pp Uns.pp) (of_stream_rev stream)
    |> Fmt.fmt "\n"
    |> ignore
  end in
  let f list = begin
    match list with
    | [] -> None
    | hd :: tl -> Some (hd, tl)
  end in
  let streams = List.map ~f:(Stream.init_indef ~f) [
    [];
    [0L];
    [0L; 1L];
    [0L; 1L; 2L];
    [0L; 1L; 2L; 3L];
  ] in
  List.iter streams ~f:test

let _ = test ()
