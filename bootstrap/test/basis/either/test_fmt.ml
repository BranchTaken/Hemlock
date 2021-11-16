open! Basis.Rudiments
open! Basis
open Either

let test () =
  let rec fn = function
    | [] -> ()
    | either :: eithers' -> begin
        File.Fmt.stdout
        |> Fmt.fmt "fmt -> "
        |> (fmt Uns.pp Uns.pp) either
        |> Fmt.fmt "\npp -> "
        |> (pp Uns.pp Uns.pp) either
        |> Fmt.fmt "\n"
        |> ignore;
        fn eithers'
      end
  in
  let eithers = [
    First 0L;
    Second 0L;
  ] in
  fn eithers

let _ = test ()
