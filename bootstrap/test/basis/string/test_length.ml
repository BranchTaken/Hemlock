open! Basis.Rudiments
open! Basis
open String

let test () =
  let strs = [
    "";
    "<_>";
    "«»";
    "‡";
    "𐆗";
  ] in
  List.iter strs ~f:(fun s ->
    File.Fmt.stdout
    |> Basis.Fmt.fmt "s="
    |> pp s
    |> Basis.Fmt.fmt ", blength="
    |> Uns.pp (B.length s)
    |> Basis.Fmt.fmt ", clength="
    |> Uns.pp (C.length s)
    |> Basis.Fmt.fmt ", is_empty="
    |> Bool.pp (is_empty s)
    |> Basis.Fmt.fmt "\n"
    |> ignore
  )

let _ = test ()
