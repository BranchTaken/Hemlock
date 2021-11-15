open! Basis.Rudiments
open! Basis
open Bool

let test () =
  let rec fn bs = begin
    match bs with
    | [] -> ()
    | b :: bs' -> begin
        File.Fmt.stdout
        |> Fmt.fmt "not "
        |> pp b
        |> Fmt.fmt " -> "
        |> pp (not b)
        |> Fmt.fmt "\n"
        |> ignore;
        fn bs'
      end
  end in
  fn [false; true]

let _ = test ()
