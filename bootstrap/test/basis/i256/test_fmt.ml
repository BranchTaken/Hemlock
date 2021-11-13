open! Basis.Rudiments
open! Basis
open I256

let test () =
  let rec fn = function
    | [] -> ()
    | x :: xs' -> begin
        let _ =
          File.Fmt.stdout
          |> pp x
          |> Fmt.fmt " "
          |> fmt ~alt:true ~zpad:true ~width:64L ~base:Fmt.Hex ~pretty:true x
          |> Fmt.fmt "\n"
        in
        fn xs'
      end
  in
  fn [
    neg_one;
    zero;
    one;
    of_string "42";
    min_value;
    max_value;
  ]

let _ = test ()
