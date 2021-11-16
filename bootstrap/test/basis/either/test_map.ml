open! Basis.Rudiments
open! Basis
open Either

let test () =
  let rec fn = function
    | [] -> ()
    | either :: eithers' -> begin
        File.Fmt.stdout
        |> Fmt.fmt "map "
        |> (pp Uns.pp Uns.pp) either
        |> Fmt.fmt " -> "
        |> (pp Uns.pp Uns.pp) (map ~first:(fun x -> x + 2L) ~second:(fun x -> x + 4L) either)
        |> Fmt.fmt "\n"
        |> ignore;
        fn eithers'
      end
  in
  let eithers = [
    First 1L;
    Second 2L;
  ] in
  fn eithers

let _ = test ()
