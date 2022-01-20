open! Basis.Rudiments
open! Basis
open I256

let test () =
  let rec test = function
    | [] -> ()
    | x :: xs' -> begin
        File.Fmt.stdout
        |> Fmt.fmt "floor_pow2,ceil_pow2 "
        |> fmt ~alt:true ~zpad:true ~width:64L ~radix:Radix.Hex ~pretty:true x
        |> Fmt.fmt " -> "
        |> fmt ~alt:true ~zpad:true ~width:64L ~radix:Radix.Hex ~pretty:true (floor_pow2 x)
        |> Fmt.fmt ", "
        |> fmt ~alt:true ~zpad:true ~width:64L ~radix:Radix.Hex ~pretty:true (ceil_pow2 x)
        |> Fmt.fmt "\n"
        |> ignore;
        test xs'
      end
  in
  let xs = [
    of_string "1";
    of_string "2";
    of_string "3";
    max_value;
  ] in
  test xs

let _ = test ()
