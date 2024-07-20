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
    let p = C.Slice.(Pattern.create (of_string pattern)) in
    File.Fmt.stdout
    |> slice_pattern_pp p
    |> Basis.Fmt.fmt "\n"
    |> Basis.Fmt.fmt "  in_:"
    |> pp s
    |> Basis.Fmt.fmt "\n"
    |> ignore;

    let rec iter_chop_prefix p ~base ~past = begin
      match C.Cursor.(base >= past) with
      | true -> ()
      | false -> begin
          let in_ = C.Slice.of_cursors ~base ~past in

          File.Fmt.stdout
          |> Basis.Fmt.fmt "first: "
          |> Basis.Fmt.fmt ~width:(C.Cursor.bindex base) ~pad:"-" ""
          |> ignore;
          let () = match C.Slice.Pattern.find ~in_ p with
            | None -> ()
            | Some cursor ->
              File.Fmt.stdout
              |> Basis.Fmt.fmt ~width:(succ ((C.Cursor.bindex cursor) - (C.Cursor.bindex base))) "|"
              |> ignore
          in
          File.Fmt.stdout |> Basis.Fmt.fmt "\n" |> ignore;
          iter_chop_prefix p ~base:(C.Cursor.succ base) ~past
        end
    end in
    let base, past = C.Slice.of_string s |> C.Slice.cursors in
    iter_chop_prefix p ~base ~past
  )

let _ = test ()
