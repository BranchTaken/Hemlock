open! Basis.Rudiments
open! Basis
open! OrdmapTest
open Ordmap

let test () =
  let e = empty (module Uns) in
  validate e;
  assert (length e = 0L);
  File.Fmt.stdout |> (fmt_internals Unit.pp) e |> Fmt.fmt "\n" |> ignore;

  let s = singleton (cmper_m e) ~k:0L ~v:"zero" in
  validate s;
  assert (length s = 1L);
  File.Fmt.stdout |> (fmt_internals String.pp) s |> Fmt.fmt "\n" |> ignore

let _ = test ()
