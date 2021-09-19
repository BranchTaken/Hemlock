open! Basis.Rudiments
open! Basis
open U63
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
    (0, 0);
    (0, 1);

    (0x7fff_ffff_ffff_ffff, 0);
    (0x7fff_ffff_ffff_ffff, 1);

    (2, 31);
    (2, 32);
    (2, 62);
    (2, 63);

    (0xf, 0xf);
    (0xff, 0xff);

    (1, 0x7fff_ffff_ffff_ffff);

    (0x7fff_ffff_ffff_ffff, 0x7fff_ffff_ffff_ffff);
  ] in
  test_pairs pairs;
  printf "@]"

let _ = test ()
