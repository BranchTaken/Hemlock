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
        let l = length t in
        printf "length %a = %a\n" ppt t Uns.pp l;
        fn (succ i) n (push_back i t)
      end
  end in
  fn 0 3 empty

let _ = test ()
