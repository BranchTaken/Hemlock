open! Basis.Rudiments
open! Basis
open Stream
open Format

let test () =
  let xppt = (xpp Uns.xpp) in
  printf "@[<h>";
  let rec test_take_up_to i l n =
    (* l + 1 so that we test taking one more than the stream contains *)
    match i <= l + 1L, l <= n with
    | _, false -> ()
    | false, _ -> test_take_up_to 0L (succ l) n
    | true, true -> begin
        let t = init (0L =:< l) ~f:(fun i -> i) in
        let t' = take i t in
        printf "take %a %a = %a\n" xppt t Uns.xpp i xppt t';
        test_take_up_to (succ i) l n
      end in
  test_take_up_to 0L 0L 3L;
  printf "@]"

let _ = test ()
