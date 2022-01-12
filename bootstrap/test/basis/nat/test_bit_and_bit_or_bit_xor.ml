open! Basis.Rudiments
open! Basis
open Nat

let test () =
  let rec test_pairs = function
    | [] -> ()
    | (x, y) :: pairs' -> begin
        File.Fmt.stdout
        |> Fmt.fmt "bit_{and,or,xor} "
        |> fmt ~alt:true ~base:Fmt.Hex x
        |> Fmt.fmt " "
        |> fmt ~alt:true ~base:Fmt.Hex y
        |> Fmt.fmt " -> "
        |> fmt ~alt:true ~base:Fmt.Hex (bit_and x y)
        |> Fmt.fmt ", "
        |> fmt ~alt:true ~base:Fmt.Hex (bit_or x y)
        |> Fmt.fmt ", "
        |> fmt ~alt:true ~base:Fmt.Hex (bit_xor x y)
        |> Fmt.fmt "\n"
        |> ignore;
        test_pairs pairs'
      end
  in
  let u64_max = of_u64 U64.max_value in
  let pairs = [
    (zero, zero);
    (u64_max, zero);
    (zero, u64_max);
    (u64_max, u64_max);
  ] in
  test_pairs pairs

let _ = test ()
