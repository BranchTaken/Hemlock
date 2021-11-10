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

    let rec fwd_iter cursor = begin
      match Cursor.next_opt cursor with
      | None -> cursor
      | Some (cp, cursor') -> begin
          File.Fmt.stdout |> Fmt.fmt (Codepoint.to_string cp) |> ignore;
          fwd_iter cursor'
        end
    end in
    let hd = Cursor.hd text in
    File.Fmt.stdout |> Fmt.fmt "  fwd   -> index=" |> Uns.pp (Cursor.index hd) |> Fmt.fmt " \""
    |> ignore;
    let tl = fwd_iter hd in
    File.Fmt.stdout |> Fmt.fmt "\"\n" |> ignore;

    let rec rev_iter cursor = begin
      match Cursor.index cursor > 0L with
      | false -> cursor
      | true -> begin
          let cp, cursor' = Cursor.prev cursor in
          File.Fmt.stdout |> Fmt.fmt (Codepoint.to_string cp) |> ignore;
          rev_iter cursor'
        end
    end in
    File.Fmt.stdout |> Fmt.fmt "  rev   -> index=" |> Uns.pp (Cursor.index tl) |> Fmt.fmt " \""
    |> ignore;
    let hd' = rev_iter tl in
    File.Fmt.stdout |> Fmt.fmt "\"\n" |> ignore;
    assert Cursor.(hd text = hd');

    let slice =
      Slice.init ~base:(Cursor.hd text) ~past:(Cursor.tl text) text in
    let s' = Slice.to_string slice in
    File.Fmt.stdout |> Fmt.fmt "  slice -> " |> String.pp s' |> Fmt.fmt "\n" |> ignore
  end in
  fn [];
  fn [""];
  fn [""; ""];
  fn ["Hello"];
  fn ["Hello"; " "; "Goodbye"]

let _ = test ()
