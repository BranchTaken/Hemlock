open! Basis.Rudiments
open! Basis
open String
open Format

let test () =
  let strs = [
    "";
    "<_>";
    "«»";
    "‡";
    "𐆗";
  ] in
  List.iter strs ~f:(fun s ->
    printf "%a |slice| -> %a\n"
      pp s pp (pare ~base:(C.Cursor.hd s) ~past:(C.Cursor.tl s) s);
    let () = match C.length s with
      | 0L -> ()
      | _ -> begin
          printf "%a .|slice| -> %a\n"
            pp s pp (pare ~base:C.Cursor.(succ (hd s)) ~past:C.Cursor.(tl s) s);
          printf "%a |slice|. -> %a\n"
            pp s pp (pare ~base:C.Cursor.(hd s) ~past:C.Cursor.(pred (tl s)) s)
        end
    in
    let () = match C.length s with
      | 0L -> ()
      | 1L -> ()
      | _ ->
        printf "%a .|slice|. -> %a\n"
          pp s pp (pare ~base:C.Cursor.(succ (hd s)) ~past:C.Cursor.(pred (tl s)) s)
    in
    ()
  )

let _ = test ()
