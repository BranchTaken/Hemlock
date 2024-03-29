open! Basis.Rudiments
open! Basis
open I64

let test () =
  let rec fn = function
    | [] -> ()
    | x :: xs' -> begin
        File.Fmt.stdout
        |> fmt ~alt:true ~zpad:true ~width:64L ~radix:Radix.Bin ~pretty:true x
        |> Fmt.fmt " "
        |> fmt ~alt:true ~radix:Radix.Oct ~pretty:true x
        |> Fmt.fmt " "
        |> pp x
        |> Fmt.fmt " "
        |> fmt ~alt:true ~zpad:true ~width:16L ~radix:Radix.Hex ~pretty:true x
        |> Fmt.fmt "\n"
        |> ignore;
        fn xs'
      end
  in
  fn [
    zero;
    one;
    of_string "42";
    min_value;
    max_value;
  ]

let _ = test ()
