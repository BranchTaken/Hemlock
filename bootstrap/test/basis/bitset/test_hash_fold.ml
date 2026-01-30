open! Basis.Rudiments
open! Basis
open Bitset

let test () =
  let rec fn = function
    | [] -> ()
    | arr :: arrs' -> begin
        let bitset = of_array arr in
        File.Fmt.stdout
        |> Fmt.fmt "hash_fold (of_array "
        |> (Array.pp Uns.pp) arr
        |> Fmt.fmt ") -> "
        |> Hash.pp (Hash.t_of_state (hash_fold bitset Hash.State.empty))
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
