open! Basis.Rudiments
open! Basis
open Real

let test () =
  List.iter [
    (* Zero. *)
    "0.";
    "0.0";
    "00.0";
    "00.00";

    "0e0";
    "0e-0";
    "0e+0";
    "0e00";

    (* Suffixes. *)
    "0r";
    "0r64";
    "0r32";

    (* e mantissa forms. *)
    "0e0";
    "1e0";
    "0.e0";
    "1.e0";
    "0.0e0";
    "0.1e0";
    "1.0e0";
    "1.1e0";
    "12e0";
    "12.e0";
    "12.3e0";
    "12.34e0";

    (* p mantissa forms. *)
    "0b0p0";
    "0b1p0";
    "0b0.p0";
    "0b1.p0";
    "0b0.0p0";
    "0b0.1p0";
    "0b1.0p0";
    "0b1.1p0";
    "0b10p0";
    "0b10.p0";
    "0b10.0p0";
    "0b10.1p0";
    "0b10.01p0";

    "0o0p0";
    "0o1p0";
    "0o0.p0";
    "0o1.p0";
    "0o0.0p0";
    "0o0.1p0";
    "0o1.0p0";
    "0o1.1p0";
    "0o12p0";
    "0o12.p0";
    "0o12.0p0";
    "0o12.3p0";
    "0o12.34p0";

    "0x0p0";
    "0x1p0";
    "0x0.p0";
    "0x1.p0";
    "0x0.0p0";
    "0x0.1p0";
    "0x1.0p0";
    "0x1.1p0";
    "0x12p0";
    "0x12.p0";
    "0x12.0p0";
    "0x12.3p0";
    "0x12.34p0";

    (* Base-specific digits. *)
    "0b10_._01";
    "0o76543210_._01234567";
    "9876753210_._0123456789";
    "0xfedcba98.0";
    "0x0.89abcdef";
    "0x76543210_.0";
    "0x0._01234567";

    (* Min/max constants. *)
    "0b1p-1074";
    "0o1p-1074";
    "0x1p-1074";
    "0x0.0000_0000_0000_1p-1022";
    "0x1.ffff_ffff_ffff_fp1023";

    "0b1p-149r32";
    "0o1p-149r32";
    "0x1p-149r32";
    "0x0.0000_02p-126r32";
    "0x1.ffff_fep127r32";

    (* Underscores. *)
    "0b__0__.__0__p__+__0__r";
    "0o__0__.__0__p__+__0__r";
    "0__.__0__e__+__0__r";
    "0x__0__.__0__p__+__0__r";

    (* Infinity/NaN. *)
    "-inf";
    "inf";
    "+inf";
    "nan";

    (* Miscellaneous. *)
    "1.0";
    "1_000_000.0";
    "42.e44";
    "42.3e-78";
    "1.5r32";
    "1.234_567_e_+89_r32";

    "0x0r";
    "0x1r";
    "0x3r";
    "0xffr";
    "0x1p42";
    "0x0.1p42";
    "0x0.01p42";
    "0x0.001p42";
    "0x0.0001p42";

    "0x0.1";
    "0x0.01";
    "0x00.001";
    "0x00.0001";

    "0b1.101p42";
    "0o7.406p42";
    "0x4.a3";
    "0x4a.3d2p+42";
    "0x4a.3d2p+42_r32";
    "0x4a.3d2p0";
    "0x4a.3d2p-42";
    "0x1.28f48p-36";

    "0x0p0";
    "0x0p-0";
    "0x0p1";
  ] ~f:(fun s ->
    File.Fmt.stdout
    |> Fmt.fmt "of_string\t" |> String.pp s
    |> ignore;
    let r = of_string s in
    File.Fmt.stdout
    |> Fmt.fmt "\t->\t" |> pp r
    |> Fmt.fmt "\t" |> fmt ~alt:true ~radix:Radix.Hex ~notation:Fmt.Normalized ~precision:13L r
    |> Fmt.fmt "\n" |> ignore
  )

let _ = test ()
