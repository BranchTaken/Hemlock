open! Basis.Rudiments
open! Basis
open List

let test () =
  let rec fn lists = begin
    match lists with
    | [] -> ()
    | l :: lists' -> begin
        File.Fmt.stdout
        |> Fmt.fmt "hash_fold "
        |> (pp Uns.pp) l
        |> Fmt.fmt " -> "
        |> Hash.pp (Hash.t_of_state (hash_fold Uns.hash_fold l Hash.State.empty))
        |> Fmt.fmt "\n"
        |> ignore;
        fn lists'
      end
  end in
  let lists = [
    [];
    [0L];
    [0L; 0L];
    [0L; 1L]
  ] in
  fn lists

let _ = test ()
