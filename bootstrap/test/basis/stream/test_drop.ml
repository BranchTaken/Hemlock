open! Basis.Rudiments
open! Basis
open Stream
open Format

let test () =
  let xppt = (xpp Uns.xpp) in
  printf "@[<h>";
  let rec test_drop_up_to i l n =
    (* l + 1 so that we test dropping one more than the stream contains *)
    match i <= l + 1L, l <= n with
    | _, false -> ()
    | false, _ -> test_drop_up_to 0L (succ l) n
    | true, true -> begin
        let t = init (0L =:< l) ~f:(fun i -> i) in
        let t' = drop i t in
        printf "drop %a %a = %a\n" Uns.xpp i xppt t xppt t';
        test_drop_up_to (succ i) l n
      end in
  test_drop_up_to 0L 0L 3L;
  printf "@]"

let _ = test ()
