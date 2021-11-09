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
      match (length t) = 0L with
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
      let vpast = fn seq 0L in
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
      xpp bytes
      (match to_string bytes with
        | None -> "None"
        | Some s -> "\"" ^ s ^ "\""
      )
      (to_string_replace bytes)
      (rev_to_string_replace bytes)
  end in
  let open Byte in
  printf "@[<h>";
  test_to_string [kv 0x61L];
  test_to_string [(kv 0xf0L); (kv 0x80L); (kv 0x80L)];
  test_to_string [(kv 0xe0L); (kv 0x80L)];
  test_to_string [(kv 0xc0L)];
  test_to_string [(kv 0xf0L); (kv 0x80L); (kv 0x80L); (kv 0xf0L)];
  test_to_string [(kv 0xe0L); (kv 0x80L); (kv 0xe0L)];
  test_to_string [(kv 0xc0L); (kv 0xc0L)];
  test_to_string [kv 0x80L];
  test_to_string [(kv 0x80L); (kv 0x80L); (kv 0x80L); (kv 0x80L)];
  test_to_string [kv 0x61L; kv 0xc0L; kv 0x62L];
  test_to_string [kv 0x61L; kv 0xe0L; kv 0x80L; kv 0x63L];
  test_to_string [kv 0x61L; kv 0xc0L; kv 0x80L; kv 0x80L; kv 0x64L];
  test_to_string [kv 0x61L; kv 0xffL; kv 0x65L];
  (* Overlong encoding. *)
  (* "a<b" *)
  test_to_string [kv 0x61L; kv 0x3cL; kv 0x62L];
  test_to_string [kv 0x61L; kv 0xc0L; kv 0xbcL; kv 0x62L];
  test_to_string [kv 0x61L; kv 0xe0L; kv 0x80L; kv 0xbcL; kv 0x62L];
  test_to_string [kv 0x61L; kv 0xf0L; kv 0x80L; kv 0x80L; kv 0xbcL; kv 0x62L];
  (* "a<" *)
  test_to_string [kv 0x61L; kv 0x3cL];
  test_to_string [kv 0x61L; kv 0xc0L; kv 0xbcL];
  test_to_string [kv 0x61L; kv 0xe0L; kv 0x80L; kv 0xbcL];
  test_to_string [kv 0x61L; kv 0xf0L; kv 0x80L; kv 0x80L; kv 0xbcL];
  (* "<b" *)
  test_to_string [kv 0x3cL; kv 0x62L];
  test_to_string [kv 0xc0L; kv 0xbcL; kv 0x62L];
  test_to_string [kv 0xe0L; kv 0x80L; kv 0xbcL; kv 0x62L];
  test_to_string [kv 0xf0L; kv 0x80L; kv 0x80L; kv 0xbcL; kv 0x62L];
  (* "a«b" *)
  test_to_string [kv 0x61L; kv 0xc2L; kv 0xabL; kv 0x62L];
  test_to_string [kv 0x61L; kv 0xe0L; kv 0x82L; kv 0xabL; kv 0x62L];
  test_to_string [kv 0x61L; kv 0xf0L; kv 0x80L; kv 0x82L; kv 0xabL; kv 0x62L];
  (* "a«" *)
  test_to_string [kv 0x61L; kv 0xc2L; kv 0xabL];
  test_to_string [kv 0x61L; kv 0xe0L; kv 0x82L; kv 0xabL];
  test_to_string [kv 0x61L; kv 0xf0L; kv 0x80L; kv 0x82L; kv 0xabL];
  (* "«b" *)
  test_to_string [kv 0xc2L; kv 0xabL; kv 0x62L];
  test_to_string [kv 0xe0L; kv 0x82L; kv 0xabL; kv 0x62L];
  test_to_string [kv 0xf0L; kv 0x80L; kv 0x82L; kv 0xabL; kv 0x62L];
  (* "a‡b" *)
  test_to_string [kv 0x61L; kv 0xe2L; kv 0x80L; kv 0xa1L; kv 0x62L];
  test_to_string [kv 0x61L; kv 0xf0L; kv 0x82L; kv 0x80L; kv 0xa1L; kv 0x62L];
  (* "a‡" *)
  test_to_string [kv 0x61L; kv 0xe2L; kv 0x80L; kv 0xa1L];
  test_to_string [kv 0x61L; kv 0xf0L; kv 0x82L; kv 0x80L; kv 0xa1L];
  (* "‡b" *)
  test_to_string [kv 0xe2L; kv 0x80L; kv 0xa1L; kv 0x62L];
  test_to_string [kv 0xf0L; kv 0x82L; kv 0x80L; kv 0xa1L; kv 0x62L];
  printf "@]"

let _ = test ()
