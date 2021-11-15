open! Basis.Rudiments
open! Basis
open Bool

let test () =
  let fn t0 t1 = begin
    File.Fmt.stdout
    |> Fmt.fmt "cmp "
    |> pp t0
    |> Fmt.fmt " "
    |> pp t1
    |> Fmt.fmt " -> "
    |> Cmp.pp (cmp t0 t1)
    |> Fmt.fmt "\n"
    |> pp t0
    |> Fmt.fmt " = "
    |> pp t1
    |> Fmt.fmt " -> "
    |> pp (t0 = t1)
    |> Fmt.fmt "\n"
    |> pp t0
    |> Fmt.fmt " <> "
    |> pp t1
    |> Fmt.fmt " -> "
    |> pp (t0 <> t1)
    |> ignore
  end in
  fn false false;
  File.Fmt.stdout |> Fmt.fmt "\n\n" |> ignore;
  fn false true;
  File.Fmt.stdout |> Fmt.fmt "\n\n" |> ignore;
  fn true false;
  File.Fmt.stdout |> Fmt.fmt "\n\n" |> ignore;
  fn true true;
  File.Fmt.stdout |> Fmt.fmt "\n" |> ignore

let _ = test ()
