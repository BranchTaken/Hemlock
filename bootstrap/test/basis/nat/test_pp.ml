open! Basis.Rudiments
open! Basis
open Nat

let test () =
  let rec fn = function
    | [] -> ()
    | x :: xs' -> begin
        File.Fmt.stdout
        |> fmt ~alt:true ~base:Fmt.Bin ~pretty:true x
        |> Fmt.fmt " "
        |> fmt ~alt:true ~base:Fmt.Oct ~pretty:true x
        |> Fmt.fmt " "
        |> pp x
        |> Fmt.fmt " "
        |> fmt ~alt:true ~base:Fmt.Hex ~pretty:true x
        |> Fmt.fmt "\n"
        |> ignore;
        fn xs'
      end
  in
  fn [
    zero;
    one;
    of_string "42";
  ]

let _ = test ()
