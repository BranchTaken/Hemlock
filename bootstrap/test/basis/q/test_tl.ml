open! Basis.Rudiments
open! Basis
open Q
open Format

let test () =
  let xppt = (xpp Uns.xpp) in
  printf "@[<h>";
  let rec fn i n t = begin
    match i <= n with
    | false -> ()
    | true -> begin
        let t' = tl t in
        printf "tl %a = %a\n" xppt t xppt t';
        fn (succ i) n (push_back i t)
      end
  end in
  (* halts if we start with empty *)
  fn 1L 4L (push_back 0L empty);
  printf "@]"

let _ = test ()
