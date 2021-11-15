open! Basis.Rudiments
open! Basis
open! Bool

let test () =
  let side_effect b s = begin
    File.Fmt.stdout
    |> Fmt.fmt "side effect "
    |> Fmt.fmt s
    |> Fmt.fmt "\n"
    |> ignore;
    b
  end in
  let rec fn pairs = begin
    match pairs with
    | [] -> ()
    | (a, b) :: pairs' -> begin
        let r = ((side_effect a "a") && (side_effect b "b")) in
        File.Fmt.stdout
        |> Fmt.fmt "("
        |> pp a
        |> Fmt.fmt " && "
        |> pp b
        |> Fmt.fmt ") -> "
        |> pp r
        |> Fmt.fmt "\n"
        |> ignore;
        fn pairs'
      end
  end in
  fn [(false, false); (false, true); (true, false); (true, true)]

let _ = test ()
