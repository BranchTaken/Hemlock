open! Basis.Rudiments
open! Basis
open Stream
open Format

let test () =
  let xppt = (xpp Uns.xpp) in
  printf "@[<h>";
  let rec test_pop t = begin
    match t with
    | lazy Nil -> ()
    | _ -> begin
        let elm, t' = pop t in
        printf "pop %a = %a %a\n" xppt t Uns.xpp elm xppt t';
        test_pop t'
      end
  end in
  test_pop (init (0L =:< 3L) ~f:(fun i -> i));
  printf "@]"

let _ = test ()
