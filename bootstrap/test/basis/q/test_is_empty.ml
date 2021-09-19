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
        let e = is_empty t in
        printf "is_empty %a = %b\n" ppt t e;
        fn (succ i) n (push_back i t)
      end
  end in
  fn 0 3 empty

let _ = test ()
