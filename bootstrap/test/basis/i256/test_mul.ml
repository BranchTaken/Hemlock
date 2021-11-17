open! Basis.Rudiments
open! Basis
open I256

let test () =
  let rec test_pairs = function
    | [] -> ()
    | (x, y) :: pairs' -> begin
        let z = x * y in
        File.Fmt.stdout
        |> fmt ~alt:true ~zpad:true ~width:64L ~base:Fmt.Hex ~pretty:true x
        |> Fmt.fmt " * "
        |> fmt ~alt:true ~zpad:true ~width:64L ~base:Fmt.Hex ~pretty:true y
        |> Fmt.fmt " -> "
        |> fmt ~alt:true ~zpad:true ~width:64L ~base:Fmt.Hex ~pretty:true z
        |> Fmt.fmt "\n"
        |> ignore;
        test_pairs pairs'
      end
  in
  let pairs = [
    (of_string "0", of_string "0");
    (of_string "0", of_string "1");
    (of_string "1", of_string
        "0xffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff");
    (of_string "0xffff_ffff", of_string "0xffff_ffff");
    (of_string "0xffff_ffff_ffff_ffff", of_string "0xffff_ffff_ffff_ffff");
    (of_string "0xffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff",
      of_string "0xffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff");
    (of_string "0xffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff",
      of_string "0xffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff")
  ] in
  test_pairs pairs

let _ = test ()
