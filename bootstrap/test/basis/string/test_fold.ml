open! Basis.Rudiments
open! Basis
open String

let test () =
  let test_fold s = begin
    File.Fmt.stdout
    |> Basis.Fmt.fmt "fold "
    |> pp s
    |> Basis.Fmt.fmt " ->"
    |> ignore;
    let () = fold s ~init:() ~f:(fun _ cp ->
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
  List.iter strs ~f:test_fold

let _ = test ()
