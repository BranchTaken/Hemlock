open! Basis.Rudiments
open! Basis
open Basis.Array

let test () =
  let rec fn arrs = begin
    match arrs with
    | [] -> ()
    | arr :: arrs' -> begin
        File.Fmt.stdout
        |> Fmt.fmt "hash_fold "
        |> (pp Uns.pp) arr
        |> Fmt.fmt " -> "
        |> Hash.pp (Hash.t_of_state (hash_fold Uns.hash_fold arr Hash.State.empty))
        |> Fmt.fmt "\n"
        |> ignore;
        fn arrs'
      end
  end in
  let arrs = [
    [||];
    [|0L|];
    [|0L; 0L|];
    [|0L; 1L|]
  ] in
  fn arrs

let _ = test ()
