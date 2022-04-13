open! Basis.Rudiments
open! Basis
open Ordset

let test () =
  let rec fn = function
    | [] -> ()
    | arr :: arrs' -> begin
        let ordset = of_array (module Uns) arr in
        File.Fmt.stdout
        |> Fmt.fmt "hash_fold (of_array (module Uns) "
        |> (Array.pp Uns.pp) arr
        |> Fmt.fmt ") -> "
        |> Hash.pp (Hash.t_of_state (hash_fold ordset Hash.State.empty))
        |> Fmt.fmt "\n"
        |> ignore;
        fn arrs'
      end
  in
  let arrs = [
    [||];
    [|0L|];
    [|0L; 1L|];
    [|0L; 2L|]
  ] in
  fn arrs

let _ = test ()
