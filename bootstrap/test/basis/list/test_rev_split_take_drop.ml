open! Basis.Rudiments
open! Basis
open List
open Format

let test () =
  let lists = [
    [];
    [0L];
    [0L; 1L]
  ] in
  printf "@[<h>";
  iter lists ~f:(fun l ->
    Range.iter (0L =:< (succ (length l))) ~f:(fun i ->
      let a, b = split i l in
      printf "split/take,drop %a %a -> %a, %a / %a, %a\n"
        Uns.xpp i
        (xpp Uns.xpp) l
        (xpp Uns.xpp) a
        (xpp Uns.xpp) b
        (xpp Uns.xpp) (take i l)
        (xpp Uns.xpp) (drop i l)
      ;

      let a, b = rev_split i l in
      printf "rev_split/rev_take,drop %a %a -> %a, %a / %a, %a\n"
        Uns.xpp i
        (xpp Uns.xpp) l
        (xpp Uns.xpp) a
        (xpp Uns.xpp) b
        (xpp Uns.xpp) (rev_take i l)
        (xpp Uns.xpp) (drop i l)
    )
  );
  printf "@]"

let _ = test ()
