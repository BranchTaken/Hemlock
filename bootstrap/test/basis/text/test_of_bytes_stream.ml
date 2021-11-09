open! Basis.Rudiments
open! Basis
open Text
open Format

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
  printf "@[<h>";
  let fn sl = begin
    printf "%a\n" (List.xpp String.xpp) sl;
    let text = of_bytes_stream (stream_of_string_list sl) in

    let rec fwd_iter cursor = begin
      match Cursor.next_opt cursor with
      | None -> cursor
      | Some (cp, cursor') -> begin
          printf "%s" (Codepoint.to_string cp);
          fwd_iter cursor'
        end
    end in
    let hd = Cursor.hd text in
    printf "  fwd   -> index=%Lu \"" (Cursor.index hd);
    let tl = fwd_iter hd in
    printf "\"\n";

    let rec rev_iter cursor = begin
      match Cursor.index cursor > 0L with
      | false -> cursor
      | true -> begin
          let cp, cursor' = Cursor.prev cursor in
          printf "%s" (Codepoint.to_string cp);
          rev_iter cursor'
        end
    end in
    printf "  rev   -> index=%Lu \"" (Cursor.index tl);
    let hd' = rev_iter tl in
    printf "\"\n";
    assert Cursor.(hd text = hd');

    let slice =
      Slice.init ~base:(Cursor.hd text) ~past:(Cursor.tl text) text in
    let s' = Slice.to_string slice in
    printf "  slice -> %a\n" String.xpp s'
  end in
  fn [];
  fn [""];
  fn [""; ""];
  fn ["Hello"];
  fn ["Hello"; " "; "Goodbye"];
  printf "@]"

let _ = test ()
