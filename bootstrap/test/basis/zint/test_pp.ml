open! Basis.Rudiments
open! Basis
open Zint

let test () =
  let rec fn = function
    | [] -> ()
    | x :: xs' -> begin
        File.Fmt.stdout
        |> fmt ~alt:true ~radix:Radix.Bin ~pretty:true x
        |> Fmt.fmt " "
        |> fmt ~alt:true ~radix:Radix.Oct ~pretty:true x
        |> Fmt.fmt " "
        |> pp x
        |> Fmt.fmt " "
        |> fmt ~alt:true ~radix:Radix.Hex ~pretty:true x
        |> Fmt.fmt " "
        |> fmt ~alt:true ~zpad:true ~width:8L ~radix:Radix.Hex ~pretty:true x
        |> Fmt.fmt "\n"
        |> ignore;
        fn xs'
      end
  in
  fn [
    zero;
    one;
    neg_one;
    of_string "42";
  ]

let _ = test ()
