open! Basis.Rudiments
open! Basis
open I32

let test () =
  let rec fn = function
    | [] -> ()
    | x :: xs' -> begin
        let _ =
          File.Fmt.stdout
          |> fmt x
          |> Fmt.fmt " "
          |> fmt ~alt:true ~zpad:true ~width:8L ~base:Fmt.Hex x
          |> Fmt.fmt "\n"
        in
        fn xs'
      end
  in
  fn [min_value; neg_one; zero; one; max_value]

let _ = test ()
