open! Basis.Rudiments
open! Basis
open String
open Format

let test () =
  let patterns = [
    "";
    "a";
    "aa";
    "aba";

    "ab";
    "aab";
    "aabab";
    "aaabaabab";
    "aaaabaaabaabab";
    "aaaaaabaaaabaaabaabab";

    "ab";
    "abaab";
    "abaabaaab";
  ] in
  let s = concat patterns in
  printf "@[";
  List.iter patterns ~f:(fun pattern ->
    let p = C.Slice.Pattern.create (C.Slice.of_string pattern) in
    printf "%a@\n" slice_pattern_xpp p;
    printf "     in_:%a@\n" xpp s;

    let print_matches matches = begin
      match matches with
      | [] -> ()
      | matches -> begin
          let _ = List.fold matches ~init:None ~f:(fun prev cursor ->
            assert (match prev with
              | None -> true
              | Some c -> Uns.((C.Cursor.bindex c) < (C.Cursor.bindex cursor))
            );
            let offset = match prev with
              | None -> (C.Cursor.bindex cursor) + 2L
              | Some c -> (C.Cursor.bindex cursor) - (C.Cursor.bindex c)
            in
            printf "%*s" (Int64.to_int offset) "|";
            Some cursor
          ) in
          ()
        end
    end in

    printf "     all:";
    print_matches (substr_find_all s ~may_overlap:true ~pattern);
    printf "@\n";

    printf "disjoint:";
    print_matches (substr_find_all s ~may_overlap:false ~pattern);
    printf "@\n";

    printf "   first:";
    let () = match substr_find s ~pattern with
      | None -> ()
      | Some cursor -> printf " %*s" (Int64.to_int (succ (C.Cursor.bindex cursor))) "|";
    in
    printf "@\n";
    printf "@\n";
    ()
  );
  printf "@]"

let _ = test ()
