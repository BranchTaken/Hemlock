open! Basis.Rudiments
open! Basis
open Bytes
open Format

module CodepointSeqRev = struct
  module T = struct
    type t = {
      bytes: byte array;
      bpast: uns;
    }

    let init bytes =
      {bytes; bpast=Array.length bytes}

    let length t =
      t.bpast

    let next t =
      match (length t) = 0 with
      | true -> None
      | false -> begin
          let bpast' = Uns.pred t.bpast in
          let b = Array.get bpast' t.bytes in
          let t' = {t with bpast=bpast'} in
          Some (b, t')
        end
  end
  include T
  include Codepoint.Seq.MakeRev(T)
end

module StringReplaceSeqRev = struct
  module T = struct
    type t = {
      seq: CodepointSeqRev.t;
      (* vpast is the index past the unprocessed sequence, were all encoding errors corrected via
       * replacement. *)
      vpast: uns;
    }

    let init bytes =
      let rec fn seq vlength = begin
        match CodepointSeqRev.to_codepoint seq with
        | Some (Valid (cp, seq')) -> begin
            let vlength' =
              (Codepoint.Utf8.length_of_codepoint cp) + vlength in
            fn seq' vlength'
          end
        | Some (Invalid seq') -> begin
            let cp = Codepoint.replacement in
            let vlength' =
              (Codepoint.Utf8.length_of_codepoint cp) + vlength in
            fn seq' vlength'
          end
        | None -> vlength
      end in
      let seq = CodepointSeqRev.init bytes in
      let vpast = fn seq 0 in
      {seq; vpast}

    let length t =
      t.vpast

    let next t =
      match CodepointSeqRev.to_codepoint t.seq with
      | Some (Valid (cp, seq')) -> begin
          let vincr = Codepoint.Utf8.length_of_codepoint cp in
          let vpast' = t.vpast - vincr in
          let t' = {seq=seq'; vpast=vpast'} in
          cp, t'
        end
      | Some (Invalid seq') -> begin
          let cp = Codepoint.replacement in
          let vincr = Codepoint.Utf8.length_of_codepoint cp in
          let vpast' = t.vpast - vincr in
          let t' = {seq=seq'; vpast=vpast'} in
          cp, t'
        end
      | None -> not_reached ()
  end
  include T
  include String.Seq.Codepoint.MakeRev(T)
end

let rev_to_string_replace bytes =
  StringReplaceSeqRev.(to_string (init bytes))

let test () =
  let test_to_string (bytes_list:byte list) = begin
    let bytes = Array.of_list bytes_list in
    printf "to_string %a -> %s, \"%s\", \"%s\"\n"
      pp bytes
      (match to_string bytes with
        | None -> "None"
        | Some s -> "\"" ^ s ^ "\""
      )
      (to_string_replace bytes)
      (rev_to_string_replace bytes)
  end in
  let open Byte in
  printf "@[<h>";
  test_to_string [kv 0x61];
  test_to_string [(kv 0xf0); (kv 0x80); (kv 0x80)];
  test_to_string [(kv 0xe0); (kv 0x80)];
  test_to_string [(kv 0xc0)];
  test_to_string [(kv 0xf0); (kv 0x80); (kv 0x80); (kv 0xf0)];
  test_to_string [(kv 0xe0); (kv 0x80); (kv 0xe0)];
  test_to_string [(kv 0xc0); (kv 0xc0)];
  test_to_string [kv 0x80];
  test_to_string [(kv 0x80); (kv 0x80); (kv 0x80); (kv 0x80)];
  test_to_string [kv 0x61; kv 0xc0; kv 0x62];
  test_to_string [kv 0x61; kv 0xe0; kv 0x80; kv 0x63];
  test_to_string [kv 0x61; kv 0xc0; kv 0x80; kv 0x80; kv 0x64];
  test_to_string [kv 0x61; kv 0xff; kv 0x65];
  (* Overlong encoding. *)
  (* "a<b" *)
  test_to_string [kv 0x61; kv 0x3c; kv 0x62];
  test_to_string [kv 0x61; kv 0xc0; kv 0xbc; kv 0x62];
  test_to_string [kv 0x61; kv 0xe0; kv 0x80; kv 0xbc; kv 0x62];
  test_to_string [kv 0x61; kv 0xf0; kv 0x80; kv 0x80; kv 0xbc; kv 0x62];
  (* "a<" *)
  test_to_string [kv 0x61; kv 0x3c];
  test_to_string [kv 0x61; kv 0xc0; kv 0xbc];
  test_to_string [kv 0x61; kv 0xe0; kv 0x80; kv 0xbc];
  test_to_string [kv 0x61; kv 0xf0; kv 0x80; kv 0x80; kv 0xbc];
  (* "<b" *)
  test_to_string [kv 0x3c; kv 0x62];
  test_to_string [kv 0xc0; kv 0xbc; kv 0x62];
  test_to_string [kv 0xe0; kv 0x80; kv 0xbc; kv 0x62];
  test_to_string [kv 0xf0; kv 0x80; kv 0x80; kv 0xbc; kv 0x62];
  (* "a«b" *)
  test_to_string [kv 0x61; kv 0xc2; kv 0xab; kv 0x62];
  test_to_string [kv 0x61; kv 0xe0; kv 0x82; kv 0xab; kv 0x62];
  test_to_string [kv 0x61; kv 0xf0; kv 0x80; kv 0x82; kv 0xab; kv 0x62];
  (* "a«" *)
  test_to_string [kv 0x61; kv 0xc2; kv 0xab];
  test_to_string [kv 0x61; kv 0xe0; kv 0x82; kv 0xab];
  test_to_string [kv 0x61; kv 0xf0; kv 0x80; kv 0x82; kv 0xab];
  (* "«b" *)
  test_to_string [kv 0xc2; kv 0xab; kv 0x62];
  test_to_string [kv 0xe0; kv 0x82; kv 0xab; kv 0x62];
  test_to_string [kv 0xf0; kv 0x80; kv 0x82; kv 0xab; kv 0x62];
  (* "a‡b" *)
  test_to_string [kv 0x61; kv 0xe2; kv 0x80; kv 0xa1; kv 0x62];
  test_to_string [kv 0x61; kv 0xf0; kv 0x82; kv 0x80; kv 0xa1; kv 0x62];
  (* "a‡" *)
  test_to_string [kv 0x61; kv 0xe2; kv 0x80; kv 0xa1];
  test_to_string [kv 0x61; kv 0xf0; kv 0x82; kv 0x80; kv 0xa1];
  (* "‡b" *)
  test_to_string [kv 0xe2; kv 0x80; kv 0xa1; kv 0x62];
  test_to_string [kv 0xf0; kv 0x82; kv 0x80; kv 0xa1; kv 0x62];
  printf "@]"

let _ = test ()
