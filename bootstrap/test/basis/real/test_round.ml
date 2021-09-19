open! Basis.Rudiments
open! Basis
open Real
open Format

let test () =
  printf "@[<h>";
  let rec fn ts = begin
    match ts with
    | [] -> ()
    | t :: ts' -> begin
        printf ("round %h -> (Down: %h) (Up: %h) (Nearest: %h) ([default]: %h)"
          ^^ " (Zero: %h)\n") t (round ~dir:Down t) (round ~dir:Up t)
          (round ~dir:Nearest t) (round t) (round ~dir:Zero t);
        fn ts'
      end
  end in
  fn [
    nan;
    neg_inf;
    -0x1.0_0000_0000_0001;
    -0x1.0;
    -0x1.f_ffff_ffff_ffffp-1;
    -0x0.8_0000_0000_0001;
    -0x0.8;
    -0x0.7_ffff_ffff_ffff;
    -0x0.00_0000_0000_0008;
    -0x0.0;
    0x0.0;
    0x0.00_0000_0000_0008;
    0x0.7_ffff_ffff_ffff;
    0x0.8;
    0x0.8_0000_0000_0001;
    0x1.f_ffff_ffff_ffffp-1;
    0x1.0;
    0x1.0_0000_0000_0001;
    inf;
  ];
  printf "@]"

let _ = test ()
