open! Basis.Rudiments
open! Basis
open N
open Format

let test () =
  printf "@[<h>";
  let rec test_rs rs = begin
    match rs with
    | [] -> ()
    | r :: rs' -> begin
        let x = of_real r in
        printf "of_real %h -> %a; to_real -> %h\n"
          r xpp_x x (to_real x);
        test_rs rs'
      end
  end in
  let rs = [
    -1.;
    0.;
    0x1.1p-1;
    1.;

    0x1.f_ffff_ffff_ffffp48;
    0x1.f_ffff_ffff_ffffp52;
    0x1.f_ffff_ffff_ffffp56;

    0x1.f_ffff_ffff_ffffp127;
    0x1.f_ffff_ffff_ffffp128;
    0x1.f_ffff_ffff_ffffp132;

    0x1.f_ffff_ffff_ffffp255;
    0x1.f_ffff_ffff_ffffp256;
    0x1.f_ffff_ffff_ffffp260;
    0x1.f_ffff_ffff_ffffp1023;

    0x1p254;
    0x1p255;
    0x1p256;
  ] in
  test_rs rs;
  printf "\n";
  let rec test_xs xs = begin
    match xs with
    | [] -> ()
    | x :: xs' -> begin
        let r = to_real x in
        printf "to_real %a -> %h; of_real -> %a\n"
          xpp_x x r xpp_x (of_real r);
        test_xs xs'
      end
  end in
  let xs = [
    zero;
    one;
  ] in
  test_xs xs;
  printf "@]"

let _ = test ()
