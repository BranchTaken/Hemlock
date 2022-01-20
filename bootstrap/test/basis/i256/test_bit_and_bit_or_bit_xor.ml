open! Basis.Rudiments
open! Basis
open I256

let test () =
  let rec test_pairs = function
    | [] -> ()
    | (x, y) :: pairs' -> begin
        File.Fmt.stdout
        |> Fmt.fmt "bit_{and,or,xor} "
        |> fmt ~alt:true ~zpad:true ~width:64L ~radix:Radix.Hex ~pretty:true x
        |> Fmt.fmt " "
        |> fmt ~alt:true ~zpad:true ~width:64L ~radix:Radix.Hex ~pretty:true y
        |> Fmt.fmt " -> "
        |> fmt ~alt:true ~zpad:true ~width:64L ~radix:Radix.Hex ~pretty:true (bit_and x y)
        |> Fmt.fmt ", "
        |> fmt ~alt:true ~zpad:true ~width:64L ~radix:Radix.Hex ~pretty:true (bit_or x y)
        |> Fmt.fmt ", "
        |> fmt ~alt:true ~zpad:true ~width:64L ~radix:Radix.Hex ~pretty:true (bit_xor x y)
        |> Fmt.fmt "\n"
        |> ignore;
        test_pairs pairs'
      end
  in
  let pairs = [
    (min_value, min_value);
    (max_value, min_value);
    (min_value, max_value);
    (max_value, max_value);
  ] in
  test_pairs pairs

let _ = test ()
