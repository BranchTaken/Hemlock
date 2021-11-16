open! Basis.Rudiments
open! Basis
open SetTest
open Set

let test () =
  let e = empty (module Uns) in
  assert (length e = 0L);
  File.Fmt.stdout
  |> fmt e
  |> Fmt.fmt "\n"
  |> ignore;

  let s = singleton (cmper_m e) 0L in
  assert (length s = 1L);
  File.Fmt.stdout
  |> fmt s
  |> Fmt.fmt "\n"
  |> ignore

let _ = test ()
