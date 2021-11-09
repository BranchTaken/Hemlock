open! Basis.Rudiments
open! Basis
open U256

let test () =
  let rec fn = function
    | [] -> ()
    | x :: xs' -> begin
        let _ =
          File.Fmt.stdout
          |> xfmt ~alt:true x
          |> Fmt.fmt " "
          |> xfmt ~alt:true ~zpad:true ~width:64L ~base:Fmt.Hex x
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
