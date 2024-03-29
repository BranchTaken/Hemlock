open! Basis.Rudiments
open! Basis
open! Hmc
open Hmc.Realer

type precision =
  | Precise
  | Rounded

let to_precision_r64 r =
  match to_r64_opt r with
  | Some r64 -> Precise, r64
  | None -> Rounded, to_r64 r

let to_precision_r32 r =
  match to_r32_opt r with
  | Some r32 -> Precise, r32
  | None -> Rounded, to_r32 r

let test () =
  let rs = [
    zero; one; inf;
    nan;
    neg zero; neg one; neg inf;
    create ~sign:Pos ~exponent:Zint.zero ~mantissa:Nat.one;
    create ~sign:Pos ~exponent:Zint.zero ~mantissa:Nat.(of_uns 0x3L);
    create ~sign:Pos ~exponent:Zint.zero ~mantissa:Nat.(of_uns 0x1fL);
    create ~sign:Pos ~exponent:Zint.zero ~mantissa:Nat.(of_uns 0xffff_ffL);
    create ~sign:Pos ~exponent:Zint.zero ~mantissa:Nat.(of_uns 0x1_ffff_ffL);
    create ~sign:Pos ~exponent:Zint.zero ~mantissa:Nat.(of_uns 0x1_ffff_ffff_fffffL);
    create ~sign:Pos ~exponent:Zint.zero ~mantissa:Nat.(of_uns 0x3_ffff_ffffff_fffL);
    create ~sign:Pos ~exponent:Zint.(one + one) ~mantissa:Nat.(of_uns 0x156L);

    create ~sign:Pos ~exponent:(Zint.of_sint Sint.(kv ~-126L)) ~mantissa:Nat.(of_uns 0xff_ffffL);
    create ~sign:Pos ~exponent:(Zint.of_sint Sint.(kv ~-126L)) ~mantissa:Nat.(of_uns 0x1ff_ffffL);

    create ~sign:Pos ~exponent:(Zint.of_sint Sint.(kv ~-127L)) ~mantissa:Nat.(of_uns 0x7f_ffffL);
    create ~sign:Pos ~exponent:(Zint.of_sint Sint.(kv ~-127L)) ~mantissa:Nat.(of_uns 0xff_ffffL);

    create ~sign:Pos ~exponent:(Zint.of_sint Sint.(kv ~-149L)) ~mantissa:Nat.(of_uns 0x1L);
    create ~sign:Pos ~exponent:(Zint.of_sint Sint.(kv ~-150L)) ~mantissa:Nat.(of_uns 0x1L);

    create ~sign:Pos ~exponent:(Zint.of_sint Sint.(kv ~-1023L))
      ~mantissa:Nat.(of_uns 0xffff_ffff_ffff_fL);
    create ~sign:Pos ~exponent:(Zint.of_sint Sint.(kv ~-1023L))
      ~mantissa:Nat.(of_uns 0x1_ffff_ffff_fffffL);

    create ~sign:Pos ~exponent:(Zint.of_sint Sint.(kv ~-1074L)) ~mantissa:Nat.(of_uns 0x1L);
    create ~sign:Pos ~exponent:(Zint.of_sint Sint.(kv ~-1075L)) ~mantissa:Nat.(of_uns 0x1L);
  ] in
  List.iter rs ~f:(fun r ->
    let prec_s = function
      | Precise -> "Precise"
      | Rounded -> "Rounded"
    in
    let prec64, r64 = to_precision_r64 r in
    let prec32, r32 = to_precision_r32 r in
    File.Fmt.stdout
    |> pp r
    |> Fmt.fmt "\n"
    |> Fmt.fmt "  to_r64 -> "
    |> Fmt.fmt (prec_s prec64)
    |> Fmt.fmt " "
    |> Real.fmt ~precision:13L ~notation:Fmt.Normalized ~radix:Radix.Hex r64
    |> Fmt.fmt "\n  to_r32 -> "
    |> Fmt.fmt (prec_s prec32)
    |> Fmt.fmt " "
    |> Real.fmt ~precision:6L ~notation:Fmt.Normalized ~radix:Radix.Hex r32
    |> Fmt.fmt "\n"
    |> ignore;
  )

let _ = test ()
