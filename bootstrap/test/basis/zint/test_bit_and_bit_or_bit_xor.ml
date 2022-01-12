open! Basis.Rudiments
open! Basis
open Zint

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
  let pairs = [
    (zero, zero);
    (neg one, zero);
    (zero, neg one);
    (neg one, neg one);
  ] in
  test_pairs pairs

let _ = test ()
