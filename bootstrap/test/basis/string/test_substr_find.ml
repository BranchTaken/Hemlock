open! Basis.Rudiments
open! Basis
open String

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
  let s = join patterns in
  List.iter patterns ~f:(fun pattern ->
    let p = C.Slice.Pattern.create (C.Slice.of_string pattern) in
    File.Fmt.stdout
    |> slice_pattern_pp p
    |> Basis.Fmt.fmt "\n"
    |> Basis.Fmt.fmt "     in_:"
    |> pp s
    |> Basis.Fmt.fmt "\n"
    |> ignore;

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
            File.Fmt.stdout
            |> Basis.Fmt.fmt ~width:offset "|"
            |> ignore;
            Some cursor
          ) in
          ()
        end
    end in

    File.Fmt.stdout |> Basis.Fmt.fmt "     all:" |> ignore;
    print_matches (substr_find_all s ~may_overlap:true ~pattern);
    File.Fmt.stdout |> Basis.Fmt.fmt "\ndisjoint:" |> ignore;
    print_matches (substr_find_all s ~may_overlap:false ~pattern);
    File.Fmt.stdout |> Basis.Fmt.fmt "\n   first:" |> ignore;
    let () = match substr_find s ~pattern with
      | None -> ()
      | Some cursor ->
        File.Fmt.stdout
        |> Basis.Fmt.fmt " "
        |> Basis.Fmt.fmt ~width:(succ (C.Cursor.bindex cursor)) "|"
        |> ignore
    in
    File.Fmt.stdout |> Basis.Fmt.fmt "\n\n" |> ignore
  )

let _ = test ()
