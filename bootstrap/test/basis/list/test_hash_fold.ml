open! Basis.Rudiments
open! Basis
open List
open Format

let test () =
  printf "@[<h>";
  let rec fn lists = begin
    match lists with
    | [] -> ()
    | l :: lists' -> begin
        printf "hash_fold %a -> %a\n"
          (xpp Uns.xpp) l
          Hash.xpp (Hash.t_of_state
            (hash_fold Uns.hash_fold l Hash.State.empty));
        fn lists'
      end
  end in
  let lists = [
    [];
    [0L];
    [0L; 0L];
    [0L; 1L]
  ] in
  fn lists;
  printf "@]"

let _ = test ()
