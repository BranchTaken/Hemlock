open! Basis.Rudiments
open! Basis
open! Hmc
open Hmc.Realer

let test () =
  let rec fn = function
    | [] -> ()
    | x :: xs' -> begin
        File.Fmt.stdout
        |> pp x
        |> Fmt.fmt "\n"
        |> ignore;
        fn xs'
      end
  in
  fn [
    zero;
    one;
    inf;
    nan;
    neg zero;
    neg one;
    neg inf;
    create ~sign:Neg ~exponent:(Zint.of_uns 0L) ~mantissa:(Nat.of_uns 0x1f0fL);
    create ~sign:Neg ~exponent:(Zint.of_uns 42L) ~mantissa:(Nat.of_uns 0x1f0fL);

    (* Denormalized mantissas. *)
    create ~sign:Neg ~exponent:(Zint.of_uns 42L) ~mantissa:(Nat.of_uns 0x1f0eL);
    create ~sign:Neg ~exponent:(Zint.of_uns 42L) ~mantissa:(Nat.of_uns 0x1f0cL);
    create ~sign:Neg ~exponent:(Zint.of_uns 42L) ~mantissa:(Nat.of_uns 0x1f08L);
    (* Same as above, but with normalized mantissas. *)
    create ~sign:Neg ~exponent:(Zint.of_uns 42L) ~mantissa:(Nat.of_uns 0xf87L);
    create ~sign:Neg ~exponent:(Zint.of_uns 42L) ~mantissa:(Nat.of_uns 0x7c3L);
    create ~sign:Neg ~exponent:(Zint.of_uns 42L) ~mantissa:(Nat.of_uns 0x3e1L);
  ]

let _ = test ()
