open! Basis.Rudiments
open! Basis
open Either

let test () =
  let rec fn = function
    | [] -> ()
    | either :: eithers' -> begin
        File.Fmt.stdout
        |> Fmt.fmt "hash_fold "
        |> (pp Uns.pp Uns.pp) either
        |> Fmt.fmt " -> "
        |> Hash.pp (Hash.t_of_state (hash_fold Uns.hash_fold Uns.hash_fold either Hash.State.empty))
        |> Fmt.fmt "\n"
        |> ignore;
        fn eithers'
      end
  in
  let eithers = [
    First 0L;
    First 1L;
    Second 0L;
    Second 1L;
  ] in
  fn eithers

let _ = test ()
