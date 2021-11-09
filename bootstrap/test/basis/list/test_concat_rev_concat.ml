open! Basis.Rudiments
open! Basis
open List
open Format

let test () =
  let list_pairs = [
    ([], []);
    ([0L; 1L], []);
    ([0L; 1L], [2L; 3L]);
    ([], [2L; 3L]);
  ] in
  printf "@[<h>";
  iter list_pairs ~f:(fun (a, b) ->
    printf "concat %a %a -> %a\n"
      (xpp Uns.xpp) a
      (xpp Uns.xpp) b
      (xpp Uns.xpp) (concat a b);
    printf "       %a %@ %a -> %a\n"
      (xpp Uns.xpp) a
      (xpp Uns.xpp) b
      (xpp Uns.xpp) (a @ b);
    printf "rev_concat %a %a -> %a\n"
      (xpp Uns.xpp) a
      (xpp Uns.xpp) b
      (xpp Uns.xpp) (rev_concat a b);
    (* Brittle test; change in conjunction with implementation. *)
    printf "concat_unordered %a %a -> %a\n"
      (xpp Uns.xpp) a
      (xpp Uns.xpp) b
      (xpp Uns.xpp) (concat_unordered a b)
  );
  printf "@]"

let _ = test ()
