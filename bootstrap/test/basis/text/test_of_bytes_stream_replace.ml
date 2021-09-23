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
    printf "  fwd   -> index=%Lu \"" (Cursor.index hd);
    let tl = fwd_iter hd in
    printf "\"\n";

    let rec rev_iter cursor = begin
      match Cursor.index cursor > 0L with
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
    printf "  rev   -> index=%Lu \"" (Cursor.index tl);
    let hd' = rev_iter tl in
    printf "\"\n";
    assert Cursor.(hd text = hd');

    let slice = Slice.of_cursors ~base:(Cursor.hd text) ~past:(Cursor.tl text) in
    let s' = Slice.to_string slice in
    printf "  slice -> %a\n" String.pp s'
  end in
  begin
    let open Byte in
    fn [kv 0x61L];
    fn [(kv 0xf0L); (kv 0x80L); (kv 0x80L)];
    fn [(kv 0xe0L); (kv 0x80L)];
    fn [(kv 0xc0L)];
    fn [(kv 0xf0L); (kv 0x80L); (kv 0x80L); (kv 0xf0L)];
    fn [(kv 0xe0L); (kv 0x80L); (kv 0xe0L)];
    fn [(kv 0xc0L); (kv 0xc0L)];
    fn [kv 0x80L];
    fn [(kv 0x80L); (kv 0x80L); (kv 0x80L); (kv 0x80L)];
    fn [kv 0x61L; kv 0xc0L; kv 0x62L];
    fn [kv 0x61L; kv 0xe0L; kv 0x80L; kv 0x63L];
    fn [kv 0x61L; kv 0xc0L; kv 0x80L; kv 0x80L; kv 0x64L];
    fn [kv 0x61L; kv 0xffL; kv 0x65L];
    fn [kv 0xefL; kv 0xbfL; kv 0xbdL];
    (* Overlong encoding. *)
    (* "a<b" *)
    fn [kv 0x61L; kv 0x3cL; kv 0x62L];
    fn [kv 0x61L; kv 0xc0L; kv 0xbcL; kv 0x62L];
    fn [kv 0x61L; kv 0xe0L; kv 0x80L; kv 0xbcL; kv 0x62L];
    fn [kv 0x61L; kv 0xf0L; kv 0x80L; kv 0x80L; kv 0xbcL; kv 0x62L];
    (* "a<" *)
    fn [kv 0x61L; kv 0x3cL];
    fn [kv 0x61L; kv 0xc0L; kv 0xbcL];
    fn [kv 0x61L; kv 0xe0L; kv 0x80L; kv 0xbcL];
    fn [kv 0x61L; kv 0xf0L; kv 0x80L; kv 0x80L; kv 0xbcL];
    (* "<b" *)
    fn [kv 0x3cL; kv 0x62L];
    fn [kv 0xc0L; kv 0xbcL; kv 0x62L];
    fn [kv 0xe0L; kv 0x80L; kv 0xbcL; kv 0x62L];
    fn [kv 0xf0L; kv 0x80L; kv 0x80L; kv 0xbcL; kv 0x62L];
    (* "a«b" *)
    fn [kv 0x61L; kv 0xc2L; kv 0xabL; kv 0x62L];
    fn [kv 0x61L; kv 0xe0L; kv 0x82L; kv 0xabL; kv 0x62L];
    fn [kv 0x61L; kv 0xf0L; kv 0x80L; kv 0x82L; kv 0xabL; kv 0x62L];
    (* "a«" *)
    fn [kv 0x61L; kv 0xc2L; kv 0xabL];
    fn [kv 0x61L; kv 0xe0L; kv 0x82L; kv 0xabL];
    fn [kv 0x61L; kv 0xf0L; kv 0x80L; kv 0x82L; kv 0xabL];
    (* "«b" *)
    fn [kv 0xc2L; kv 0xabL; kv 0x62L];
    fn [kv 0xe0L; kv 0x82L; kv 0xabL; kv 0x62L];
    fn [kv 0xf0L; kv 0x80L; kv 0x82L; kv 0xabL; kv 0x62L];
    (* "a‡b" *)
    fn [kv 0x61L; kv 0xe2L; kv 0x80L; kv 0xa1L; kv 0x62L];
    fn [kv 0x61L; kv 0xf0L; kv 0x82L; kv 0x80L; kv 0xa1L; kv 0x62L];
    (* "a‡" *)
    fn [kv 0x61L; kv 0xe2L; kv 0x80L; kv 0xa1L];
    fn [kv 0x61L; kv 0xf0L; kv 0x82L; kv 0x80L; kv 0xa1L];
    (* "‡b" *)
    fn [kv 0xe2L; kv 0x80L; kv 0xa1L; kv 0x62L];
    fn [kv 0xf0L; kv 0x82L; kv 0x80L; kv 0xa1L; kv 0x62L];
  end;
  printf "@]"

let _ = test ()
