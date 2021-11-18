open! Basis.Rudiments
open! Basis
open U128

let test () =
  let rec test = function
    | [] -> ()
    | x :: xs' -> begin
        File.Fmt.stdout
        |> Fmt.fmt "bit_not "
        |> fmt ~alt:true ~zpad:true ~width:32L ~base:Fmt.Hex ~pretty:true x
        |> Fmt.fmt " -> "
        |> fmt ~alt:true ~zpad:true ~width:32L ~base:Fmt.Hex ~pretty:true (bit_not x)
        |> Fmt.fmt "\n"
        |> ignore;
        test xs'
      end
  in
  let xs = [
    of_string "0";
    of_string "0xffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff"
  ] in
  test xs

let _ = test ()
