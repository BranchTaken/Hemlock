open! Basis.Rudiments
open! Basis
open! Hmc
open Hmc.Realer
open Format

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
  printf "@[<h>";
  let rec test_hash_fold rs = begin
    match rs with
    | [] -> ()
    | r :: rs' -> begin
        let prec_s = function
          | Precise -> "Precise"
          | Rounded -> "Rounded"
        in
        printf "%a\n" pp r;
        let prec64, r64 = to_precision_r64 r in
        printf "  to_r64 -> %s %a\n" (prec_s prec64) Real.pp r64;
        let prec32, r32 = to_precision_r32 r in
        printf "  to_r32 -> %s %a\n" (prec_s prec32) Real.pp r32;
        test_hash_fold rs'
      end
  end in
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
  ] in
  test_hash_fold rs;
  printf "@]"

let _ = test ()
