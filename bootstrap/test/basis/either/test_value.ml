open! Basis.Rudiments
open! Basis
open Either

let test () =
  let rec fn = function
    | [] -> ()
    | either :: eithers' -> begin
        File.Fmt.stdout
        |> Fmt.fmt "value "
        |> (pp Uns.pp Uns.pp) either
        |> Fmt.fmt " -> "
        |> Uns.pp (value either)
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
