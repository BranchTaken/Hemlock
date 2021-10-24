open! Basis.Rudiments
open! Basis
open Bool

let test () =
  let rec fn = function
    | [] -> ()
    | t :: ts' -> begin
        let _ = File.Fmt.stdout |> fmt t |> String.fmt "\n" in
        fn ts'
      end
  in
  fn [false; true]

let _ = test ()
