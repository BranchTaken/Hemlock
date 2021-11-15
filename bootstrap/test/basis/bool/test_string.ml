open! Basis.Rudiments
open! Basis
open Bool

let test () =
  let rec fn = function
    | [] -> ()
    | t :: ts' -> begin
        let s = to_string t in
        File.Fmt.stdout
        |> Fmt.fmt "to_string "
        |> pp t
        |> Fmt.fmt " -> "
        |> String.pp s
        |> Fmt.fmt " ; of_string "
        |> String.pp s
        |> Fmt.fmt " -> "
        |> pp (of_string s)
        |> Fmt.fmt "\n"
        |> ignore;
        fn ts'
      end
  in
  fn [false; true]

let _ = test ()
