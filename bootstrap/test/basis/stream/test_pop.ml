open! Basis.Rudiments
open! Basis
open Stream
open Format

let test () =
  let ppt = (pp Uns.pp) in
  printf "@[<h>";
  let rec test_pop t = begin
    match t with
    | lazy Nil -> ()
    | _ -> begin
        let elm, t' = pop t in
        printf "pop %a = %a %a\n" ppt t Uns.pp elm ppt t';
        test_pop t'
      end
  end in
  test_pop (init (0L =:< 3L) ~f:(fun i -> i));
  printf "@]"

let _ = test ()
