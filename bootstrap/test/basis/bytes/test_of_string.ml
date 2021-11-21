open! Basis.Rudiments
open! Basis
open Bytes

let test () =
  let strs = [
    "";
    "<_>«‡𐆗»[_]";
  ] in
  List.iter strs ~f:(fun s ->
    let bytes = of_string_slice (String.C.Slice.of_string s) in
    File.Fmt.stdout
    |> String.pp s
    |> Fmt.fmt " -> "
    |> pp bytes
    |> Fmt.fmt " -> "
    |> String.pp (to_string_hlt bytes)
    |> Fmt.fmt "\n"
    |> ignore
  )

let _ = test ()
