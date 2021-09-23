open! Basis.Rudiments
open! Basis
open U32
open Format

let test () =
  printf "@[<h>";
  let rec test_pairs = function
    | [] -> ()
    | (x, y) :: pairs' -> begin
        printf "%a ** %a -> %a\n" pp_x x pp_x y pp_x (x ** y);
        test_pairs pairs'
      end
  in
  let pairs = [
    (kv 0L, kv 0L);
    (kv 0L, kv 1L);

    (kv 0xffff_ffffL, kv 0L);
    (kv 0xffff_ffffL, kv 1L);

    (kv 2L, kv 15L);
    (kv 2L, kv 16L);
    (kv 2L, kv 31L);
    (kv 2L, kv 32L);

    (kv 0xfL, kv 0xfL);
    (kv 0xffL, kv 0xffL);

    (kv 1L, kv 0xffff_ffffL);

    (kv 0xffff_ffffL, kv 0xffff_ffffL);
  ] in
  test_pairs pairs;
  printf "@]"

let _ = test ()
