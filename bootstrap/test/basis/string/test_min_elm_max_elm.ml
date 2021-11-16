open! Basis.Rudiments
open! Basis
open String

let test () =
  let test_elm s ~cmp = begin
    File.Fmt.stdout
    |> Basis.Fmt.fmt "min_elm "
    |> pp s
    |> Basis.Fmt.fmt " -> "
    |> Option.fmt Codepoint.pp (min_elm s ~cmp)
    |> Basis.Fmt.fmt "\nmax_elm "
    |> pp s
    |> Basis.Fmt.fmt " -> "
    |> Option.fmt Codepoint.pp (max_elm s ~cmp)
    |> Basis.Fmt.fmt "\n"
    |> ignore
  end in
  test_elm "" ~cmp:Codepoint.cmp;
  test_elm "baced" ~cmp:Codepoint.cmp;
  let cmp cp0 cp1 = begin
    match Codepoint.cmp cp0 cp1 with
    | Lt -> Cmp.Gt
    | Eq -> Cmp.Eq
    | Gt -> Cmp.Lt
  end in
  test_elm "" ~cmp;
  test_elm "baced" ~cmp

let _ = test ()
