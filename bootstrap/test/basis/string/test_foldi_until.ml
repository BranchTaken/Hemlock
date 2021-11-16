open! Basis.Rudiments
open! Basis
open String

let test () =
  let test_foldi_until s = begin
    File.Fmt.stdout
    |> Basis.Fmt.fmt "foldi_until "
    |> pp s
    |> Basis.Fmt.fmt " ->"
    |> ignore;
    let () = foldi_until s ~init:() ~f:(fun i _ cp ->
      let until = Codepoint.(cp = (of_char 'c')) in
      File.Fmt.stdout
      |> Basis.Fmt.fmt " "
      |> Uns.pp i
      |> Basis.Fmt.fmt ":"
      |> Basis.Fmt.fmt (of_codepoint cp)
      |> ignore;
      (), until
    ) in
    File.Fmt.stdout |> Basis.Fmt.fmt "\n" |> ignore
  end in
  let strs = [
    "";
    "abcde";
  ] in
  List.iter strs ~f:test_foldi_until

let _ = test ()
