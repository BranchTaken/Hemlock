open! Basis.Rudiments
open! Basis
open! Hmc
open Hmc.Realer
open Format

let test () =
  let rec fn = function
    | [] -> ()
    | x :: xs' -> begin
        printf "%a\n" pp x;
        fn xs'
      end
  in
  printf "@[<h>";
  fn [
    zero;
    one;
    inf;
    nan;
    neg zero;
    neg one;
    neg inf;
    create ~sign:Neg ~exponent:(Zint.of_uns 0) ~mantissa:(Nat.of_uns 0x1f0f);
    create ~sign:Neg ~exponent:(Zint.of_uns 42) ~mantissa:(Nat.of_uns 0x1f0f);

    (* Denormalized mantissas. *)
    create ~sign:Neg ~exponent:(Zint.of_uns 42) ~mantissa:(Nat.of_uns 0x1f0e);
    create ~sign:Neg ~exponent:(Zint.of_uns 42) ~mantissa:(Nat.of_uns 0x1f0c);
    create ~sign:Neg ~exponent:(Zint.of_uns 42) ~mantissa:(Nat.of_uns 0x1f08);
    (* Same as above, but with normalized mantissas. *)
    create ~sign:Neg ~exponent:(Zint.of_uns 42) ~mantissa:(Nat.of_uns 0xf87);
    create ~sign:Neg ~exponent:(Zint.of_uns 42) ~mantissa:(Nat.of_uns 0x7c3);
    create ~sign:Neg ~exponent:(Zint.of_uns 42) ~mantissa:(Nat.of_uns 0x3e1);
  ];
  printf "@]"

let _ = test ()
