open! Basis.Rudiments
open! Basis
open Real

let test () =
  let rec fn ts = begin
    match ts with
    | [] -> ()
    | t :: ts' -> begin
        File.Fmt.stdout
        |> Fmt.fmt "round "
        |> fmt ~alt:true ~radix:Radix.Hex t
        |> Fmt.fmt " -> (Down: "
        |> fmt ~alt:true ~radix:Radix.Hex (round ~dir:Down t)
        |> Fmt.fmt ") (Up: "
        |> fmt ~alt:true ~radix:Radix.Hex (round ~dir:Up t)
        |> Fmt.fmt ") Nearest: "
        |> fmt ~alt:true ~radix:Radix.Hex (round ~dir:Nearest t)
        |> Fmt.fmt ") ([default]: "
        |> fmt ~alt:true ~radix:Radix.Hex (round t)
        |> Fmt.fmt ") (Zero: "
        |> fmt ~alt:true ~radix:Radix.Hex (round ~dir:Zero t)
        |> Fmt.fmt ")\n"
        |> ignore;
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
  ]

let _ = test ()
