open! Basis.Rudiments
open! Basis
open String

let test () =
  let test_fold_right_until s = begin
    File.Fmt.stdout
    |> Basis.Fmt.fmt "fold_right_until "
    |> pp s
    |> Basis.Fmt.fmt " ->"
    |> ignore;
    let () = fold_right_until s ~init:() ~f:(fun cp _ ->
      let until = Codepoint.(cp = (of_char 'c')) in
      File.Fmt.stdout
      |> Basis.Fmt.fmt " "
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
  List.iter strs ~f:test_fold_right_until

let _ = test ()
