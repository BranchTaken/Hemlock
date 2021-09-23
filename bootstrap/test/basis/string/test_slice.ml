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
      pp s pp (pare ~base:(Cursor.hd s) ~past:(Cursor.tl s));
    let () = match clength s with
      | 0L -> ()
      | _ -> begin
          printf "%a .|slice| -> %a\n"
            pp s pp (pare ~base:Cursor.(succ (hd s)) ~past:Cursor.(tl s));
          printf "%a |slice|. -> %a\n"
            pp s pp (pare ~base:Cursor.(hd s) ~past:Cursor.(pred (tl s)))
        end
    in
    let () = match clength s with
      | 0L -> ()
      | 1L -> ()
      | _ ->
        printf "%a .|slice|. -> %a\n"
          pp s pp (pare ~base:Cursor.(succ (hd s)) ~past:Cursor.(pred (tl s)))
    in
    ()
  )

let _ = test ()
