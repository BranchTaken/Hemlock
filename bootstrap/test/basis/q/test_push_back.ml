open! Basis.Rudiments
open! Basis
open Q
open Format

let test () =
  let ppt = (pp Uns.pp) in
  printf "@[<h>";
  let rec fn i n t = begin
    match i <= n with
    | false -> ()
    | true -> begin
        let t' = push_back i t in
        printf "push_back %a %a = %a\n" Uns.pp i ppt t ppt t';
        fn (succ i) n t'
      end
  end in
  fn 0 3 empty;
  printf "@]"

let _ = test ()
