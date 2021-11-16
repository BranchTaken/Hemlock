open! Basis.Rudiments
open! Basis
open String

let test () =
  let test_foldi s = begin
    File.Fmt.stdout
    |> Basis.Fmt.fmt "foldi "
    |> pp s
    |> Basis.Fmt.fmt " ->"
    |> ignore;
    let () = foldi s ~init:() ~f:(fun i _ cp ->
      File.Fmt.stdout
      |> Basis.Fmt.fmt " "
      |> Uns.pp i
      |> Basis.Fmt.fmt ":"
      |> Basis.Fmt.fmt (of_codepoint cp)
      |> ignore
    ) in
    File.Fmt.stdout |> Basis.Fmt.fmt "\n" |> ignore
  end in
  let strs = [
    "";
    "abcde";
  ] in
  List.iter strs ~f:test_foldi

let _ = test ()
