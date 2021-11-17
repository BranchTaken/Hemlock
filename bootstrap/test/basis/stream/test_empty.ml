open! Basis.Rudiments
open! Basis
open Stream

let test () =
  let t = empty in
  File.Fmt.stdout
  |> Fmt.fmt "empty = "
  |> (pp Uns.pp) t
  |> Fmt.fmt "\n"
  |> ignore

let _ = test ()
