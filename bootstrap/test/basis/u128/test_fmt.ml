open! Basis.Rudiments
open! Basis
open U128

let test () =
  let rec fn = function
    | [] -> ()
    | x :: xs' -> begin
        let _ =
          File.Fmt.stdout
          |> pp x
          |> Fmt.fmt " "
          |> fmt ~alt:true ~zpad:true ~width:32L ~base:Fmt.Hex ~pretty:true x
          |> Fmt.fmt "\n"
        in
        fn xs'
      end
  in
  fn [
    zero;
    one;
    of_string "42";
    min_value;
    max_value
  ]

let _ = test ()
