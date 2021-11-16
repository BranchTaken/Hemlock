open! Basis.Rudiments
open! Basis
open MapTest
open Map

let test () =
  let rec fn = function
    | [] -> ()
    | l :: lists' -> begin
        let map = of_klist l in
        File.Fmt.stdout
        |> Fmt.fmt "hash_fold (of_klist "
        |> (List.pp Uns.pp) l
        |> Fmt.fmt ") -> "
        |> Hash.pp (Hash.t_of_state (hash_fold Uns.hash_fold map Hash.State.empty))
        |> Fmt.fmt "\n"
        |> ignore;
        fn lists'
      end
  in
  (* NB: [0; 1] and [0; 2] collide. This is because we're using UnsTestCmper to get stable test
   * output; the hashing results from all but the last binding hashed are discarded. *)
  let lists = [
    [];
    [0L];
    [0L; 1L];
    [0L; 2L];
    [2L; 3L]
  ] in
  fn lists

let _ = test ()
