open! Basis.Rudiments
open! Basis
open Bitset

let test () =
  let e = empty in
  assert (length e = 0L);
  File.Fmt.stdout
  |> fmt e
  |> Fmt.fmt "\n"
  |> ignore;

  let s = singleton 0L in
  assert (length s = 1L);
  File.Fmt.stdout
  |> fmt s
  |> Fmt.fmt "\n"
  |> ignore

let _ = test ()
