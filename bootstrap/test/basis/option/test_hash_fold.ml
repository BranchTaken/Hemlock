open! Basis.Rudiments
open! Basis
open Option

let test () =
  let rec fn = function
    | [] -> ()
    | option :: options' -> begin
        File.Fmt.stdout
        |> Fmt.fmt "hash_fold "
        |> (pp Uns.pp) option
        |> Fmt.fmt " -> "
        |> Hash.pp (Hash.t_of_state (hash_fold Uns.hash_fold option Hash.State.empty))
        |> Fmt.fmt "\n"
        |> ignore;
        fn options'
      end
  in
  let options = [
    None;
    (Some 0L);
    (Some 1L)
  ] in
  fn options

let _ = test ()
