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
    |> pp s
    |> Basis.Fmt.fmt " |slice| -> "
    |> pp (pare ~base:(C.Cursor.hd s) ~past:(C.Cursor.tl s) s)
    |> Basis.Fmt.fmt "\n"
    |> (fun formatter ->
      match C.length s with
      | 0L -> formatter
      | _ -> begin
          formatter
          |> pp s
          |> Basis.Fmt.fmt " .|slice| -> "
          |> pp (pare ~base:C.Cursor.(succ (hd s)) ~past:C.Cursor.(tl s) s)
          |> Basis.Fmt.fmt "\n"
          |> pp s
          |> Basis.Fmt.fmt " |slice|. -> "
          |> pp (pare ~base:C.Cursor.(hd s) ~past:C.Cursor.(pred (tl s)) s)
          |> Basis.Fmt.fmt "\n"
        end
    )
    |> (fun formatter ->
      match C.length s with
      | 0L
      | 1L -> formatter
      | _ ->
        formatter
        |> pp s
        |> Basis.Fmt.fmt " .|slice|. -> "
        |> pp (pare ~base:C.Cursor.(succ (hd s)) ~past:C.Cursor.(pred (tl s)) s)
        |> Basis.Fmt.fmt "\n"
    )
    |> ignore
  )

let _ = test ()
