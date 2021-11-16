open! Basis.Rudiments
open! Basis
open String

let test () =
  let test_fold_right s = begin
    File.Fmt.stdout
    |> Basis.Fmt.fmt "fold_right "
    |> pp s
    |> Basis.Fmt.fmt " ->"
    |> ignore;
    let () = fold_right s ~init:() ~f:(fun cp _ ->
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
  List.iter strs ~f:test_fold_right

let _ = test ()
