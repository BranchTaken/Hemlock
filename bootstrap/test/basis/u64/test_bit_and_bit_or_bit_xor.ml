open! Basis.Rudiments
open! Basis
open U64

let test () =
  let rec test_pairs = function
    | [] -> ()
    | (x, y) :: pairs' -> begin
        File.Fmt.stdout
        |> Fmt.fmt "bit_{and,or,xor} "
        |> fmt ~alt:true ~zpad:true ~width:16L ~radix:Radix.Hex x
        |> Fmt.fmt " "
        |> fmt ~alt:true ~zpad:true ~width:16L ~radix:Radix.Hex y
        |> Fmt.fmt " -> "
        |> fmt ~alt:true ~zpad:true ~width:16L ~radix:Radix.Hex (bit_and x y)
        |> Fmt.fmt ", "
        |> fmt ~alt:true ~zpad:true ~width:16L ~radix:Radix.Hex (bit_or x y)
        |> Fmt.fmt ", "
        |> fmt ~alt:true ~zpad:true ~width:16L ~radix:Radix.Hex (bit_xor x y)
        |> Fmt.fmt "\n"
        |> ignore;
        test_pairs pairs'
      end
  in
  let pairs = [
    (of_string "0", of_string "0");
    (of_string "0xffff_ffff_ffff_ffff", of_string "0");
    (of_string "0", of_string "0xffff_ffff_ffff_ffff");
    (of_string "0xffff_ffff_ffff_ffff", of_string "0xffff_ffff_ffff_ffff");
  ] in
  test_pairs pairs

let _ = test ()
