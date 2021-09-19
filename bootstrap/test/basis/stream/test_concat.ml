open! Basis.Rudiments
open! Basis
open Stream
open Format

let test () =
  let ppt = (pp Uns.pp) in
  printf "@[<h>";
  let rec test_concat_up_to i l n = begin
    match i <= l, l <= n with
    | _, false -> ()
    | false, _ -> test_concat_up_to 0 (succ l) n
    | true, true -> begin
        let t0 = init ~f:(fun i -> i) i in
        let t1 = init ~f:(fun j -> i + j) (l - i) in
        let t = concat t0 t1 in
        printf "concat %a %a = %a\n" ppt t0 ppt t1 ppt t;
        test_concat_up_to (succ i) l n
      end
  end in
  test_concat_up_to 0 0 3;
  printf "@]"

let _ = test ()
