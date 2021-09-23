open! Basis.Rudiments
open! Basis
open Stream
open Format

let test () =
  let ppt = (pp Uns.pp) in
  printf "@[<h>";
  let rec test_tl_up_to i n = begin
    match i <= n with
    | false -> ()
    | true -> begin
        let t = init ~f:(fun i -> i) i in
        let t' = tl t in
        printf "tl %a = %a\n" ppt t ppt t';
        test_tl_up_to (succ i) n
      end
  end in
  (* halts if we start at 0 *)
  test_tl_up_to 1L 4L;
  printf "@]"

let _ = test ()
