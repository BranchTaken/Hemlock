open! Basis.Rudiments
open! Basis
open Zint

let test () =
  let rec test_rs rs = begin
    match rs with
    | [] -> ()
    | r :: rs' -> begin
        let x = of_real r in
        File.Fmt.stdout
        |> Fmt.fmt "of_real "
        |> Real.fmt ~alt:true ~radix:Radix.Hex r
        |> Fmt.fmt " -> "
        |> fmt ~alt:true ~radix:Radix.Hex x
        |> Fmt.fmt "; to_real -> "
        |> Real.fmt ~alt:true ~radix:Radix.Hex (to_real x)
        |> Fmt.fmt "\n"
        |> ignore;
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
  File.Fmt.stdout |> Fmt.fmt "\n" |> ignore;
  let rec test_xs xs = begin
    match xs with
    | [] -> ()
    | x :: xs' -> begin
        let r = to_real x in
        File.Fmt.stdout
        |> Fmt.fmt "to_real "
        |> fmt ~alt:true ~radix:Radix.Hex x
        |> Fmt.fmt " -> "
        |> Real.fmt ~alt:true ~radix:Radix.Hex r
        |> Fmt.fmt "; of_real -> "
        |> fmt ~alt:true ~radix:Radix.Hex (of_real r)
        |> Fmt.fmt "\n"
        |> ignore;
        test_xs xs'
      end
  end in
  let xs = [
    zero;
    one;
  ] in
  test_xs xs

let _ = test ()
