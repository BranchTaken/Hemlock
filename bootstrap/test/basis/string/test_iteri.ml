open! Basis.Rudiments
open! Basis
open String

let test () =
  let test_iteri s = begin
    File.Fmt.stdout
    |> Basis.Fmt.fmt "iteri "
    |> pp s
    |> Basis.Fmt.fmt " ->"
    |> ignore;
    let () = iteri s ~f:(fun i cp ->
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
  List.iter strs ~f:test_iteri

let _ = test ()
