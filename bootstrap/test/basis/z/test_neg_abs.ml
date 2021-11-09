open! Basis.Rudiments
open! Basis
open Z
open Format

let test () =
  printf "@[<h>";
  let rec test = function
    | [] -> ()
    | u :: us' -> begin
        let fn u = begin
          printf "neg,abs %a -> %a, %a\n"
            xpp_x u
            xpp_x (neg u)
            xpp_x (abs u)
        end in
        fn u;
        fn (neg u);
        test us'
      end
  in
  let us = [
    of_string "0";
    of_string "1";
    of_string "2";
    of_string "3";
    of_string "0x8000_0000_0000_0000";
    of_string "0xffff_ffff_ffff_ffff"
  ] in
  test us;
  printf "@]"

let _ = test ()
