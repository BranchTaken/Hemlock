open! Basis.Rudiments
open! Basis
open I8

let test () =
  let rec fn = function
    | [] -> ()
    | x :: xs' -> begin
        File.Fmt.stdout
        |> pp x
        |> Fmt.fmt " "
        |> fmt ~alt:true ~zpad:true ~width:2L ~base:Fmt.Hex ~pretty:true x
        |> Fmt.fmt "\n"
        |> ignore;
        fn xs'
      end
  in
  fn [min_value; neg_one; zero; one; max_value]

let _ = test ()
