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
    | false, _ -> test_concat_up_to 0L (succ l) n
    | true, true -> begin
        let t0 = init (0L =:< i) ~f:(fun i -> i) in
        let t1 = init (i =:< l) ~f:(fun j -> j) in
        let t = concat t0 t1 in
        printf "concat %a %a = %a\n" ppt t0 ppt t1 ppt t;
        test_concat_up_to (succ i) l n
      end
  end in
  test_concat_up_to 0L 0L 3L;
  printf "@]"

let _ = test ()
