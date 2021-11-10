open! Basis.Rudiments
open! Basis
open Text

let stream_of_bytes_list bl =
  Stream.init_indef bl ~f:(fun bl ->
    match bl with
    | [] -> None
    | bytes :: bl' -> Some (bytes, bl')
  )

let stream_of_string_list sl =
  stream_of_bytes_list (List.map sl ~f:(fun s ->
    Bytes.Slice.of_string_slice (String.C.Slice.of_string s)))

let test () =
  let fn sl = begin
    File.Fmt.stdout |> (List.pp String.pp) sl |> Fmt.fmt "\n" |> ignore;
    let text = of_bytes_stream (stream_of_string_list sl) in

    let rec fwd_iter ~line ~col cursor = begin
      match Cursor.next_opt cursor with
      | None -> cursor
      | Some (cp, cursor') -> begin
          let pos = Cursor.pos cursor' in
          let () = match cp with
            | cp when Codepoint.(cp = nl) -> begin
                assert (Pos.line pos = Uns.succ line);
                assert (Pos.col pos = 0L)
              end
            | cp when Codepoint.(cp = ht) -> begin
                let tabwidth = tabwidth (Cursor.container cursor) in
                assert (Pos.line pos = line);
                assert ((Pos.col pos) / tabwidth = Uns.succ (col / tabwidth));
                assert (Pos.col pos % tabwidth = 0L);
              end
            | _ -> begin
                assert (Pos.line pos = line);
                assert (Pos.col pos = Uns.succ col)
              end
          in
          let line', col' = Pos.line pos, Pos.col pos in
          fwd_iter ~line:line' ~col:col' cursor'
        end
    end in
    let tl = fwd_iter ~line:1L ~col:0L (Cursor.hd text) in
    let pos = Cursor.pos tl in
    File.Fmt.stdout |> Fmt.fmt "  pos tl = " |> Uns.pp (Pos.line pos) |> Fmt.fmt ":"
    |> Uns.fmt (Pos.col pos) |> Fmt.fmt "\n" |> ignore;

    let rec rev_iter ~line ~col cursor = begin
      match Cursor.index cursor > 0L with
      | false -> cursor
      | true -> begin
          let cp, cursor' = Cursor.prev cursor in
          let pos = Cursor.pos cursor' in
          let () = match cp with
            | cp when Codepoint.(cp = nl) -> begin
                assert (Pos.line pos = Uns.pred line)
              end
            | cp when Codepoint.(cp = ht) -> begin
                let tabwidth = tabwidth (Cursor.container cursor) in
                assert (Pos.line pos = line);
                assert ((Pos.col (Cursor.pos cursor)) % tabwidth = 0L)
              end
            | _ -> begin
                assert (Pos.line pos = line);
                assert (Pos.col pos = Uns.pred col)
              end
          in
          let line', col' = Pos.line pos, Pos.col pos in
          rev_iter ~line:line' ~col:col' cursor'
        end
    end in
    let hd' = rev_iter ~line:(Pos.line pos) ~col:(Pos.col pos) tl in
    File.Fmt.stdout
    |> Fmt.fmt "  pos hd = " |> Uns.fmt (Pos.line (Cursor.pos hd')) |> Fmt.fmt ":"
    |> Uns.fmt (Pos.col (Cursor.pos hd')) |> Fmt.fmt "\n" |> ignore;
    assert Cursor.(hd text = hd');
  end in
  fn [""];
  fn ["Hello"];
  fn ["Hello"; "\n"; "Goodbye"];
  fn ["A"; "\n"; "B"; "\n"];
  fn ["A"; "\n"; "B"; "\n"; "C"];
  fn ["A"; "\n"; "\n"; "C"];
  fn ["\t"];
  fn ["A"; "\t"];
  fn ["\t"; "B"];
  fn ["A"; "\t"; "B"];
  fn ["A"; "\t"; "\t"; "B"];
  fn ["A"; "\n"; "\t"; "B"];
  fn ["A"; "\t"; "\n"; "B"];
  fn ["A"; "\t"; "B"; "\t"; "C"]

let _ = test ()
