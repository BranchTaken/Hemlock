open! Basis.Rudiments
open! Basis
open String

let test () =
  let strs = [
    "";
    "<_>";
    "«»";
  ] in
  List.iter strs ~f:(fun s ->
    Range.Uns.iter (0L =:< (C.length s + 2L)) ~f:(fun i ->
      File.Fmt.stdout
      |> Basis.Fmt.fmt "prefix "
      |> pp s
      |> Basis.Fmt.fmt " "
      |> Uns.pp i
      |> Basis.Fmt.fmt " -> "
      |> pp (prefix i s)
      |> Basis.Fmt.fmt "\nsuffix "
      |> pp s
      |> Basis.Fmt.fmt " "
      |> Uns.pp i
      |> Basis.Fmt.fmt " -> "
      |> pp (suffix i s)
      |> Basis.Fmt.fmt "\n"
      |> ignore
    )
  )

let _ = test ()
