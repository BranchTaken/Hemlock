open! Basis.Rudiments
open! Basis
open Bool

let test () =
  let rec fn = function
    | [] -> ()
    | t :: ts' -> begin
        let x = to_uns t in
        File.Fmt.stdout
        |> Fmt.fmt "to_uns "
        |> pp t
        |> Fmt.fmt " -> "
        |> Uns.pp x
        |> Fmt.fmt " ; of_uns "
        |> Uns.pp x
        |> Fmt.fmt " -> "
        |> pp (of_uns x)
        |> Fmt.fmt "\n"
        |> ignore;
        fn ts'
      end
  in
  fn [false; true]

let _ = test ()
