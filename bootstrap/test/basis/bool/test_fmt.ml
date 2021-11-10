open! Basis.Rudiments
open! Basis
open Bool

let test () =
  let rec fn = function
    | [] -> ()
    | t :: ts' -> begin
        File.Fmt.stdout |> fmt t |> Fmt.fmt "\n" |> ignore;
        fn ts'
      end
  in
  fn [false; true]

let _ = test ()
