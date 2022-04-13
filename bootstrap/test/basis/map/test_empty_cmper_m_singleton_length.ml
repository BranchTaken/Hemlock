open! Basis.Rudiments
open! Basis
open MapTest
open Map

let test () =
  let e = empty (module UnsTestCmper) in
  validate e;
  assert (length e = 0L);
  File.Fmt.stdout
  |> (fmt_internals ~alt:true Unit.pp) e
  |> Fmt.fmt "\n"
  |> ignore;

  let s = singleton (cmper_m e) ~k:0L ~v:"0" in
  validate s;
  assert (length s = 1L);
  File.Fmt.stdout
  |> (fmt_internals ~alt:true String.pp) s
  |> Fmt.fmt "\n"
  |> ignore

let _ = test ()
