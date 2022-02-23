open! Basis.Rudiments
open! Basis
open Bytes

let test () =
  let test ?sep slices = begin
    File.Fmt.stdout
    |> Fmt.fmt "join"
    |> (fun formatter ->
      match sep with
      | None -> formatter
      | Some sep -> formatter |> Fmt.fmt " ~sep:" |> Slice.pp sep
    )
    |> Fmt.fmt " "
    |> (List.pp Slice.pp) slices
    |> Fmt.fmt " -> "
    |> Slice.pp (Slice.join ?sep slices)
    |> Fmt.fmt "\n"
    |> ignore
  end in
  let e = Slice.init [||] in
  let a = Slice.init [|U8.kv 0L|] in
  let b = Slice.init [|U8.kv 1L|] in
  let c = Slice.init [|U8.kv 2L|] in
  let d = Slice.init [|U8.kv 3L|] in
  test [];
  test [e];
  test [e; e];
  test [e; e; e];

  test [a];

  test [a; e];
  test [e; a];
  test [a; b];

  test [a; e; e];
  test [e; a; e];
  test [e; e; a];
  test [a; b; e];
  test [a; e; b];
  test [a; b; c];

  test ~sep:d [];
  test ~sep:d [e];
  test ~sep:d [e; e];
  test ~sep:d [e; e; e];

  test ~sep:d [a];

  test ~sep:d [a; e];
  test ~sep:d [e; a];
  test ~sep:d [a; b];

  test ~sep:d [a; e; e];
  test ~sep:d [e; a; e];
  test ~sep:d [e; e; a];
  test ~sep:d [a; b; e];
  test ~sep:d [a; e; b];
  test ~sep:d [a; b; c]

let _ = test ()
