open! Basis.Rudiments
open! Basis
open Text

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
  let fn bl = begin
    File.Fmt.stdout
    |> (List.pp (Byte.fmt ~alt:true ~zpad:true ~width:2L ~base:Fmt.Hex ~pretty:true)) bl
    |> Fmt.fmt "\n" |> ignore;
    let text = of_bytes_stream (stream_of_byte_list bl) in

    let rec fwd_iter cursor = begin
      match Cursor.next_opt cursor with
      | None -> cursor
      | Some (cp, cursor') -> begin
          File.Fmt.stdout
          |> Fmt.fmt (match Cursor.rvalid cursor with
            | true ->  Codepoint.to_string cp
            | false -> "«�»"
          )
          |> ignore;
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
          File.Fmt.stdout
          |> Fmt.fmt (match Cursor.lvalid cursor with
            | true -> Codepoint.to_string cp
            | false -> "«�»"
          )
          |> ignore;
          rev_iter cursor'
        end
    end in
    File.Fmt.stdout |> Fmt.fmt "  rev   -> index=" |> Uns.pp (Cursor.index tl) |> Fmt.fmt " \""
    |> ignore;
    let hd' = rev_iter tl in
    File.Fmt.stdout |> Fmt.fmt "\"\n" |> ignore;
    assert Cursor.(hd text = hd');

    let slice = Slice.init text in
    let s' = Slice.to_string slice in
    File.Fmt.stdout |> Fmt.fmt "  slice -> " |> String.pp s' |> Fmt.fmt "\n" |> ignore
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
  end

let _ = test ()
