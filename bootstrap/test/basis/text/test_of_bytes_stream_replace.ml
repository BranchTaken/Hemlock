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

let stream_of_byte_list bl =
  stream_of_bytes_list (List.map bl ~f:(fun b ->
    let bytes = [|b|] in
    Bytes.(Slice.of_cursors ~base:(Cursor.hd bytes) ~past:(Cursor.tl bytes))
  ))

let test () =
  printf "@[<h>";
  let fn bl = begin
    printf "%a\n" (List.pp Byte.pp_x) bl;
    let text = of_bytes_stream (stream_of_byte_list bl) in

    let rec fwd_iter cursor = begin
      match Cursor.next_opt cursor with
      | None -> cursor
      | Some (cp, cursor') -> begin
          let () = match Cursor.rvalid cursor with
            | true -> printf "%s" (Codepoint.to_string cp)
            | false -> printf "%s" "«�»"
          in
          fwd_iter cursor'
        end
    end in
    let hd = Cursor.hd text in
    printf "  fwd   -> index=%u \"" (Cursor.index hd);
    let tl = fwd_iter hd in
    printf "\"\n";

    let rec rev_iter cursor = begin
      match Cursor.index cursor > 0 with
      | false -> cursor
      | true -> begin
          let cp, cursor' = Cursor.prev cursor in
          let () = match Cursor.lvalid cursor with
            | true -> printf "%s" (Codepoint.to_string cp)
            | false -> printf "%s" "«�»"
          in
          rev_iter cursor'
        end
    end in
    printf "  rev   -> index=%u \"" (Cursor.index tl);
    let hd' = rev_iter tl in
    printf "\"\n";
    assert Cursor.(hd text = hd');

    let slice = Slice.of_cursors ~base:(Cursor.hd text) ~past:(Cursor.tl text) in
    let s' = Slice.to_string slice in
    printf "  slice -> %a\n" String.pp s'
  end in
  begin
    let open Byte in
    fn [kv 0x61];
    fn [(kv 0xf0); (kv 0x80); (kv 0x80)];
    fn [(kv 0xe0); (kv 0x80)];
    fn [(kv 0xc0)];
    fn [(kv 0xf0); (kv 0x80); (kv 0x80); (kv 0xf0)];
    fn [(kv 0xe0); (kv 0x80); (kv 0xe0)];
    fn [(kv 0xc0); (kv 0xc0)];
    fn [kv 0x80];
    fn [(kv 0x80); (kv 0x80); (kv 0x80); (kv 0x80)];
    fn [kv 0x61; kv 0xc0; kv 0x62];
    fn [kv 0x61; kv 0xe0; kv 0x80; kv 0x63];
    fn [kv 0x61; kv 0xc0; kv 0x80; kv 0x80; kv 0x64];
    fn [kv 0x61; kv 0xff; kv 0x65];
    fn [kv 0xef; kv 0xbf; kv 0xbd];
    (* Overlong encoding. *)
    (* "a<b" *)
    fn [kv 0x61; kv 0x3c; kv 0x62];
    fn [kv 0x61; kv 0xc0; kv 0xbc; kv 0x62];
    fn [kv 0x61; kv 0xe0; kv 0x80; kv 0xbc; kv 0x62];
    fn [kv 0x61; kv 0xf0; kv 0x80; kv 0x80; kv 0xbc; kv 0x62];
    (* "a<" *)
    fn [kv 0x61; kv 0x3c];
    fn [kv 0x61; kv 0xc0; kv 0xbc];
    fn [kv 0x61; kv 0xe0; kv 0x80; kv 0xbc];
    fn [kv 0x61; kv 0xf0; kv 0x80; kv 0x80; kv 0xbc];
    (* "<b" *)
    fn [kv 0x3c; kv 0x62];
    fn [kv 0xc0; kv 0xbc; kv 0x62];
    fn [kv 0xe0; kv 0x80; kv 0xbc; kv 0x62];
    fn [kv 0xf0; kv 0x80; kv 0x80; kv 0xbc; kv 0x62];
    (* "a«b" *)
    fn [kv 0x61; kv 0xc2; kv 0xab; kv 0x62];
    fn [kv 0x61; kv 0xe0; kv 0x82; kv 0xab; kv 0x62];
    fn [kv 0x61; kv 0xf0; kv 0x80; kv 0x82; kv 0xab; kv 0x62];
    (* "a«" *)
    fn [kv 0x61; kv 0xc2; kv 0xab];
    fn [kv 0x61; kv 0xe0; kv 0x82; kv 0xab];
    fn [kv 0x61; kv 0xf0; kv 0x80; kv 0x82; kv 0xab];
    (* "«b" *)
    fn [kv 0xc2; kv 0xab; kv 0x62];
    fn [kv 0xe0; kv 0x82; kv 0xab; kv 0x62];
    fn [kv 0xf0; kv 0x80; kv 0x82; kv 0xab; kv 0x62];
    (* "a‡b" *)
    fn [kv 0x61; kv 0xe2; kv 0x80; kv 0xa1; kv 0x62];
    fn [kv 0x61; kv 0xf0; kv 0x82; kv 0x80; kv 0xa1; kv 0x62];
    (* "a‡" *)
    fn [kv 0x61; kv 0xe2; kv 0x80; kv 0xa1];
    fn [kv 0x61; kv 0xf0; kv 0x82; kv 0x80; kv 0xa1];
    (* "‡b" *)
    fn [kv 0xe2; kv 0x80; kv 0xa1; kv 0x62];
    fn [kv 0xf0; kv 0x82; kv 0x80; kv 0xa1; kv 0x62];
  end;
  printf "@]"

let _ = test ()
