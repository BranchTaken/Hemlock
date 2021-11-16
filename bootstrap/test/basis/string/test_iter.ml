open! Basis.Rudiments
open! Basis
open String

let test () =
  let test_iter s = begin
    File.Fmt.stdout
    |> Basis.Fmt.fmt "iter "
    |> pp s
    |> Basis.Fmt.fmt " ->"
    |> ignore;
    let () = iter s ~f:(fun cp ->
      File.Fmt.stdout
      |> Basis.Fmt.fmt " "
      |> Basis.Fmt.fmt (of_codepoint cp)
      |> ignore
    ) in
    File.Fmt.stdout |> Basis.Fmt.fmt "\n" |> ignore
  end in
  let strs = [
    "";
    "abcde";
  ] in
  List.iter strs ~f:test_iter

let _ = test ()
