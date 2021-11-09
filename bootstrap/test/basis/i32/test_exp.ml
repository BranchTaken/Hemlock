open! Basis.Rudiments
open! Basis
open I32
open Format

let test () =
  printf "@[<h>";
  let rec test_pairs = function
    | [] -> ()
    | (x, y) :: pairs' -> begin
        printf "%a ** %a -> %a\n" xpp_x x xpp_x y xpp_x (x ** y);
        test_pairs pairs'
      end
  in
  let pairs = [
    (kv 0L, kv 0L);
    (kv 0L, kv 1L);
    (kv 1L, kv 0L);
    (kv 1L, kv 1L);

    (kv (-1L), kv 0L);
    (kv (-1L), kv 1L);

    (kv 2L, kv 15L);
    (kv 2L, kv 16L);
    (kv 2L, kv 31L);
    (kv 2L, kv 32L);

    (kv 0xfL, kv 0xfL);
    (kv 0xffL, kv 0xffL);

    (kv 1L, kv (-1L));

    (kv (-1L), kv (-1L));

  ] in
  test_pairs pairs;
  printf "@]"

let _ = test ()
