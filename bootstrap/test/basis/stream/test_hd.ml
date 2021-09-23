open! Basis.Rudiments
open! Basis
open Stream
open Format

let test () =
  let ppt = (pp Uns.pp) in
  printf "@[<h>";
  let rec test_hd_up_to i n = begin
    match i <= n with
    | false -> ()
    | true -> begin
        let t = init ~f:(fun i -> i) i in
        let elm = hd t in
        printf "hd %a = %a\n" ppt t Uns.pp elm;
        test_hd_up_to (succ i) n
      end
  end in
  (* halts if we start at 0 *)
  test_hd_up_to 1L 4L;
  printf "@]"

let _ = test ()
