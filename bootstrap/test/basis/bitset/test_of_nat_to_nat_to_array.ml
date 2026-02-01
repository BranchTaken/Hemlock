open! Basis.Rudiments
open! Basis
open Bitset

let test () =
  let test ms = begin
    let bitset = of_nat ms in
    File.Fmt.stdout
    |> Fmt.fmt "of_nat "
    |> Nat.fmt ~alt:true ~radix:Radix.Hex ms
    |> Fmt.fmt "; to_nat -> "
    |> Nat.fmt ~alt:true ~radix:Radix.Hex (to_nat bitset)
    |> Fmt.fmt "; to_array -> "
    |> (Array.pp Uns.pp) (to_array bitset)
    |> Fmt.fmt "\n"
    |> ignore
  end in
  let test_nats = List.map ~f:(fun s -> Nat.of_string s) [
    "0x0n";
    "0x1n";
    "0x3n";
    "0x7n";
    "0x4_0000_0000_0000_0003n";
    "0x800_0002_0000_0000_0000_0003n";
  ] in
  List.iter test_nats ~f:(fun ms ->
    test ms
  )

let _ = test ()
