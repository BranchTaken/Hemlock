open! Basis.Rudiments
open! Basis
open I256

let test () =
  let rec test = function
    | [] -> ()
    | x :: xs' -> begin
        File.Fmt.stdout
        |> Fmt.fmt "bit_not "
        |> fmt ~alt:true ~zpad:true ~width:64L ~base:Fmt.Hex ~pretty:true x
        |> Fmt.fmt " -> "
        |> fmt ~alt:true ~zpad:true ~width:64L ~base:Fmt.Hex ~pretty:true (bit_not x)
        |> Fmt.fmt "\n"
        |> ignore;
        test xs'
      end
  in
  let xs = [
    min_value;
    max_value;
  ] in
  test xs

let _ = test ()
