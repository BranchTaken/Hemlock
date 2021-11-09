open! Basis.Rudiments
open! Basis
open U8
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

    (kv 0xffL, kv 0L);
    (kv 0xffL, kv 1L);

    (kv 2L, kv 3L);
    (kv 2L, kv 4L);
    (kv 2L, kv 7L);
    (kv 2L, kv 8L);

    (kv 0xfL, kv 0xfL);
    (kv 0xffL, kv 0xffL);

    (kv 1L, kv 0xffL);
  ] in
  test_pairs pairs;
  printf "@]"

let _ = test ()
