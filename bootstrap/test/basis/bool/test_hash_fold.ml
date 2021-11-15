open! Basis.Rudiments
open! Basis
open Bool

let test () =
  let rec test_hash_fold bools = begin
    match bools with
    | [] -> ()
    | b :: bools' -> begin
        File.Fmt.stdout
        |> Fmt.fmt "hash_fold "
        |> pp b
        |> Fmt.fmt " -> "
        |> Hash.pp (Hash.t_of_state (hash_fold b Hash.State.empty))
        |> Fmt.fmt "\n"
        |> ignore;
        test_hash_fold bools'
      end
  end in
  let bools = [false; true] in
  test_hash_fold bools

let _ = test ()
