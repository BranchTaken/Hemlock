open! Basis.Rudiments
open! Basis
open OrdmapTest
open Ordmap

let test () =
  let rec fn = function
    | [] -> ()
    | l :: lists' -> begin
        let ordmap = of_klist l in
        File.Fmt.stdout
        |> Fmt.fmt "hash_fold (of_klist "
        |> (List.pp Uns.pp) l
        |> Fmt.fmt ") -> "
        |> Hash.pp (Hash.t_of_state (hash_fold Uns.hash_fold ordmap Hash.State.empty))
        |> Fmt.fmt "\n"
        |> ignore;
        fn lists'
      end
  in
  let lists = [
    [];
    [0L];
    [0L; 1L];
    [0L; 2L];
    [2L; 3L]
  ] in
  fn lists

let _ = test ()
