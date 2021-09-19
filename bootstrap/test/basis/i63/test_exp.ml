open! Basis.Rudiments
open! Basis
open I63
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
    (kv 0, kv 0);
    (kv 0, kv 1);

    (kv 0x3fff_ffff_ffff_ffff, kv 0);
    (kv 0x3fff_ffff_ffff_ffff, kv 1);

    (kv 2, kv 31);
    (kv 2, kv 32);
    (kv 2, kv 62);
    (kv 2, kv 63);

    (kv 0xf, kv 0xf);
    (kv 0xff, kv 0xff);

    (kv 1, kv 0x3fff_ffff_ffff_ffff);

    (kv 0x3fff_ffff_ffff_ffff, kv 0x3fff_ffff_ffff_ffff);
  ] in
  test_pairs pairs;
  printf "@]"

let _ = test ()
