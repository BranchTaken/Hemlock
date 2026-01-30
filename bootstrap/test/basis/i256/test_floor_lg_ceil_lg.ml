open! Basis.Rudiments
open! Basis
open I256

let test () =
  let rec test = function
    | [] -> ()
    | x :: xs' -> begin
        File.Fmt.stdout
        |> Fmt.fmt "floor_lg,ceil_lg "
        |> fmt ~alt:true ~zpad:true ~width:64L ~radix:Radix.Hex ~pretty:true x
        |> Fmt.fmt " -> "
        |> Uns.pp (floor_lg x)
        |> Fmt.fmt ", "
        |> Uns.pp (ceil_lg x)
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
