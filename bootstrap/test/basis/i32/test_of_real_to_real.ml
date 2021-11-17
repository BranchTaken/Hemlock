open! Basis.Rudiments
open! Basis
open I32

let test () =
  let rec test_rs rs = begin
    match rs with
    | [] -> ()
    | r :: rs' -> begin
        let x = of_real r in
        File.Fmt.stdout
        |> Fmt.fmt "of_real "
        |> Real.fmt ~alt:true ~base:Fmt.Hex r
        |> Fmt.fmt " -> "
        |> fmt ~alt:true ~zpad:true ~width:8L ~base:Fmt.Hex ~pretty:true x
        |> Fmt.fmt "; to_real -> "
        |> Real.fmt ~alt:true ~base:Fmt.Hex (to_real x)
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

    0x1.f_ffff_ffff_ffffp61;

    0x1p30;
    0x1p31;
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
        |> fmt ~alt:true ~zpad:true ~width:8L ~base:Fmt.Hex ~pretty:true x
        |> Fmt.fmt " -> "
        |> Real.fmt ~alt:true ~base:Fmt.Hex r
        |> Fmt.fmt "; of_real -> "
        |> fmt ~alt:true ~zpad:true ~width:8L ~base:Fmt.Hex ~pretty:true (of_real r)
        |> Fmt.fmt "\n"
        |> ignore;
        test_xs xs'
      end
  end in
  let xs = [
    neg_one;
    zero;
    one;
    max_value;
  ] in
  test_xs xs

let _ = test ()
