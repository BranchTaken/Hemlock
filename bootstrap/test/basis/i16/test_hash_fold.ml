open! Basis.Rudiments
open! Basis
open I16

let test () =
  let rec test_hash_fold xs = begin
    match xs with
    | [] -> ()
    | x :: xs' -> begin
        File.Fmt.stdout
        |> Fmt.fmt "hash_fold "
        |> fmt ~alt:true ~zpad:true ~width:4L ~base:Fmt.Hex ~pretty:true x
        |> Fmt.fmt " -> "
        |> Hash.pp (Hash.t_of_state (hash_fold x Hash.State.empty))
        |> Fmt.fmt "\n"
        |> ignore;
        test_hash_fold xs'
      end
  end in
  let xs = [min_value; neg_one; zero; one; max_value] in
  test_hash_fold xs

let _ = test ()
