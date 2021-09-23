open! Basis.Rudiments
open! Basis
open List
open Format

let test () =
  let list_pairs = [
    ([], []);

    ([100L], [200L]);
    ([100L; 110L], [200L; 210L]);
    ([100L; 110L; 120L], [200L; 210L; 220L]);
  ] in
  printf "@[<h>";
  iter list_pairs ~f:(fun (a, b) ->
    let f i accum a_elm b_elm = begin
      let len = length a in
      let limit = len - 2L in
      ((i + a_elm + b_elm) :: accum), (i = limit)
    end in
    printf "foldi2 %a %a -> %a\n"
      (pp Uns.pp) a
      (pp Uns.pp) b
      (pp Uns.pp) (foldi2_until a b ~init:[] ~f)
  );
  printf "@]"

let _ = test ()
