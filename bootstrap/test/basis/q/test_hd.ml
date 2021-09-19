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
        let elm = hd t in
        printf "hd %a = %a\n" ppt t Uns.pp elm;
        fn (succ i) n (push_back i t)
      end
  end in
  (* halts if we start with empty *)
  fn 1 4 (push_back 0 empty);
  printf "@]"

let _ = test ()
