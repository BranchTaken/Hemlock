open! Basis.Rudiments
open! Basis
open! SetTest
open Set

let test () =
  let rec fn = function
    | [] -> ()
    | l :: lists' -> begin
        let set = of_list (module Uns) l in
        File.Fmt.stdout
        |> Fmt.fmt "hash_fold (of_list (module Uns) "
        |> (List.pp Uns.pp) l
        |> Fmt.fmt ") -> "
        |> Hash.pp (Hash.t_of_state (hash_fold set Hash.State.empty))
        |> Fmt.fmt "\n"
        |> ignore;
        fn lists'
      end
  in
  let lists = [
    [];
  ] in
  fn lists

let _ = test ()
