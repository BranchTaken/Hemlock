open Rudiments0

type t = byte array

module ArraySeq = struct
  module T = struct
    type t = {
      slice: String.Slice.t;
      bindex: uns;
    }
    type elm = byte

    let init t =
      {slice=t; bindex=0}

    let length t =
      String.Slice.blength t.slice - t.bindex

    let next t =
      assert (length t > 0);
      let b = String.Slice.get t.bindex t.slice in
      let t' = {t with bindex=Uns.succ t.bindex} in
      b, t'
  end
  include T
  include Array.Seq.MakeMono(T)
end

module Cursor = struct
  module T = struct
    type container = byte array
    type elm = byte
    type t = {
      array: container;
      index: uns;
    }

    let cmp t0 t1 =
      (* == is excessively vague in OCaml. *)
      assert ((t0.array == t1.array)
              || (Stdlib.( = ) t0.array t1.array));
      Uns.cmp t0.index t1.index

    let hd array =
      {array; index=0}

    let tl array =
      {array; index=(Array.length array)}

    let seek i t =
      match Sint.(i < (kv 0)) with
      | true -> begin
          match (Uns.of_sint Sint.(neg i)) > t.index with
          | true -> halt "Cannot seek before beginning of array"
          | false -> {t with index=(t.index - Uns.of_sint (Sint.neg i))}
        end
      | false -> begin
          match (t.index + (Uns.of_sint i)) > (Array.length t.array) with
          | true -> halt "Cannot seek past end of array"
          | false -> {t with index=(t.index + (Uns.of_sint i))}
        end

    let succ t =
      seek (Sint.kv 1) t

    let pred t =
      seek (Sint.kv (-1)) t

    let lget t =
      Array.get (Uns.pred t.index) t.array

    let rget t =
      Array.get t.index t.array

    let prev t =
      lget t, pred t

    let next t =
      rget t, succ t

    let container t =
      t.array

    let index t =
      t.index
  end
  include T
  include Cmpable.Make(T)
end

module CodepointSeq = struct
  module T = struct
    type t = {
      cursor: Cursor.t;
      past: Cursor.t;
    }

    let init ~cursor ~past =
      {cursor; past}

    let length t =
      (Cursor.index t.past) - (Cursor.index t.cursor)

    let next t =
      match (length t) = 0 with
      | true -> None
      | false -> begin
          let b, cursor' = Cursor.next t.cursor in
          let t' = {t with cursor=cursor'} in
          Some (b, t')
        end
  end
  include T
  include Codepoint.Seq.Make(T)
end

(** Action to take if a UTF-8 encoding error is encountered in [transcode]. *)
type on_invalid =
  | Error   (* Return [None]. *)
  | Replace (* Replace with '�'. *)
  | Halt    (* Halt. *)

module StringSeq = struct
  module T = struct
    type t = {
      on_invalid: on_invalid;
      seq: CodepointSeq.t;
      (* vlength is how long bytes would be if all encoding errors were
       * corrected via replacement. *)
      vlength: uns;
      (* vindex tracks how many bytes would be required to correctly encode
       * already-consumed bytes. *)
      vindex: uns
    }

    let rec vlength ~on_invalid seq vindex =
      match on_invalid, (CodepointSeq.to_codepoint seq) with
      | _, Some (Valid (cp, seq')) -> begin
          let vindex' = vindex + (Codepoint.Utf8.length_of_codepoint cp) in
          vlength ~on_invalid seq' vindex'
        end
      | Error, Some (Invalid _) -> None
      | Replace, Some (Invalid seq') -> begin
          let cp = Codepoint.replacement in
          let vindex' =
            vindex + (Codepoint.Utf8.length_of_codepoint cp) in
          vlength ~on_invalid seq' vindex'
        end
      | Halt, Some (Invalid _) -> halt "Invalid utf8 sequence"
      | _, None -> Some vindex

    let init ~on_invalid ~cursor ~past =
      let seq = CodepointSeq.init ~cursor ~past in
      match vlength ~on_invalid seq 0 with
      | Some vlength -> Some {on_invalid; seq; vlength; vindex=0}
      | None -> None

    let length t =
      t.vlength - t.vindex

    let next t =
      match t.on_invalid, (CodepointSeq.to_codepoint t.seq) with
      | _, Some (Valid (cp, seq')) -> begin
          let vincr = Codepoint.Utf8.length_of_codepoint cp in
          let vindex' = t.vindex + vincr in
          let t' = {t with seq=seq'; vindex=vindex'} in
          cp, t'
        end
      | Error, Some (Invalid _) -> not_reached ()
      | Replace, Some (Invalid seq') -> begin
          let cp = Codepoint.replacement in
          let vincr = Codepoint.Utf8.length_of_codepoint cp in
          let vindex' = t.vindex + vincr in
          let t' = {t with seq=seq'; vindex=vindex'} in
          cp, t'
        end
      | Halt, Some (Invalid _)
      | _, None -> not_reached ()
  end
  include T
  include String.Seq.Codepoint.Make(T)
end

module Slice = struct
  include Slice.MakeMono(Cursor)

  let length t =
    (Cursor.index (past t)) - (Cursor.index (base t))

  let get i t =
    Array.get (Cursor.index (base t) + i) (container t)

  let pp ppf t =
    let open Format in
    fprintf ppf "@[<h>[|";
    let rec fn ppf cursor past = begin
      match Cursor.(cursor < past) with
      | true -> begin
          let elm, cursor' = Cursor.next cursor in
          fprintf ppf ";@ %a" Byte.pp_x elm;
          fn ppf cursor' past
        end
      | false -> ()
    end in
    let cursor, past = to_cursors t in
    begin
      match Cursor.(cursor < past) with
      | true -> begin
          let elm, cursor' = Cursor.next cursor in
          fprintf ppf "%a" Byte.pp_x elm;
          fn ppf cursor' past
        end
      | false -> ()
    end;
    fprintf ppf "|]@]"

  let hash_fold t state =
    Hash.State.Gen.init state
    |> Hash.State.Gen.fold_u8 (length t) ~f:(fun i ->
      (Byte.to_uns (Cursor.(rget (seek (Uns.to_sint i) (base t))))))
    |> Hash.State.Gen.fini
    |> Uns.hash_fold (length t)

  let of_codepoint cp =
    of_container (Array.of_list (Codepoint.to_bytes cp))

  let of_string_slice slice =
    of_container (ArraySeq.to_array (ArraySeq.init slice))

  let transcode ?(on_invalid=Error) t =
    let cursor, past = to_cursors t in
    match StringSeq.init ~on_invalid ~cursor ~past with
    | Some seq -> Some (StringSeq.to_string seq)
    | None -> None

  let to_string t =
    transcode t

  let to_string_replace t =
    match transcode ~on_invalid:Replace t with
    | Some string -> string
    | None -> not_reached ()

  let to_string_hlt t =
    match transcode ~on_invalid:Halt t with
    | Some  string -> string
    | None -> not_reached ()
end

let pp ppf t =
  Slice.(pp ppf (of_container t))

let hash_fold t state =
  Slice.(hash_fold (of_container t) state)

let length t =
  Slice.(length (of_container t))

let get i t =
  Slice.(get i (of_container t))

let of_codepoint cp =
  Slice.(to_container (of_codepoint cp))

let of_string_slice slice =
  Slice.(to_container (of_string_slice slice))

let to_string t =
  Slice.(to_string (of_container t))

let to_string_replace t =
  Slice.(to_string_replace (of_container t))

let to_string_hlt t =
  Slice.(to_string_hlt (of_container t))

(******************************************************************************)
(* Begin tests. *)

let%expect_test "hash_fold" =
  let open Format in
  printf "@[<h>";
  let rec fn strs = begin
    match strs with
    | [] -> ()
    | s :: strs' -> begin
        let bytes = of_string_slice (String.Slice.of_string s) in
        printf "hash_fold %a (%a) -> %a\n"
          pp bytes
          String.pp s
          Hash.pp (Hash.t_of_state
            (hash_fold bytes Hash.State.empty));
        fn strs'
      end
  end in
  let strs = [""; "hello"; "<"; "«"; "‡"; "𐆗"] in
  fn strs;
  printf "@]";

  [%expect{|
    hash_fold [||] ("") -> 0xb465_a9ec_cd79_1cb6_4bbd_1bf2_7da9_18d6u128
    hash_fold [|0x68u8; 0x65u8; 0x6cu8; 0x6cu8; 0x6fu8|] ("hello") -> 0xe7f7_3e0e_c178_5525_e460_58c5_1383_657cu128
    hash_fold [|0x3cu8|] ("<") -> 0x4fa1_90f5_fd4b_19d9_e73e_229a_b8e4_9c7eu128
    hash_fold [|0xc2u8; 0xabu8|] ("«") -> 0x237d_65de_c606_4e09_6241_b399_77d7_fc8bu128
    hash_fold [|0xe2u8; 0x80u8; 0xa1u8|] ("‡") -> 0x0eb9_1d81_6e4f_e11c_829d_ba36_47d6_1f81u128
    hash_fold [|0xf0u8; 0x90u8; 0x86u8; 0x97u8|] ("𐆗") -> 0x59b5_cf23_cff9_5c91_4b98_7455_0bbc_946fu128
    |}]

let%expect_test "hash_fold empty" =
  let hash_empty state = begin
    state
    |> hash_fold [||]
  end in
  let e1 =
    Hash.State.empty
    |> hash_empty
  in
  let e2 =
    Hash.State.empty
    |> hash_empty
    |> hash_empty
  in
  assert U128.((Hash.t_of_state e1) <> (Hash.t_of_state e2));

  [%expect{|
    |}]

let%expect_test "of_codepoint" =
  let open Format in
  let strs = [
    "<";
    "«";
    "‡";
    "𐆗";
  ] in
  let cps = List.fold_right strs ~init:[] ~f:(fun s cps ->
    String.Cursor.(rget (hd s)) :: cps
  ) in
  printf "@[<h>";
  List.iter cps ~f:(fun cp ->
    let bytes = of_codepoint cp in
    printf "'%s' -> %a -> %a\n"
      (String.of_codepoint cp)
      pp bytes
      String.pp (to_string_hlt bytes)
  );
  printf "@]";

  [%expect{|
    '<' -> [|0x3cu8|] -> "<"
    '«' -> [|0xc2u8; 0xabu8|] -> "«"
    '‡' -> [|0xe2u8; 0x80u8; 0xa1u8|] -> "‡"
    '𐆗' -> [|0xf0u8; 0x90u8; 0x86u8; 0x97u8|] -> "𐆗"
    |}]

let%expect_test "of_string" =
  let open Format in
  let strs = [
    "";
    "<_>«‡𐆗»[_]";
  ] in
  printf "@[<h>";
  List.iter strs ~f:(fun s ->
    let bytes = of_string_slice (String.Slice.of_string s) in
    printf "%a -> %a -> %a\n"
      String.pp s
      pp bytes
      String.pp (to_string_hlt bytes)
  );
  printf "@]";

  [%expect{|
    "" -> [||] -> ""
    "<_>«‡𐆗»[_]" -> [|0x3cu8; 0x5fu8; 0x3eu8; 0xc2u8; 0xabu8; 0xe2u8; 0x80u8; 0xa1u8; 0xf0u8; 0x90u8; 0x86u8; 0x97u8; 0xc2u8; 0xbbu8; 0x5bu8; 0x5fu8; 0x5du8|] -> "<_>«‡𐆗»[_]"
    |}]

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
      (* vpast is the index past the unprocessed sequence, were all encoding
       * errors corrected via replacement. *)
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

let%expect_test "to_string" =
  let open Format in
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
  printf "@]";

  [%expect{|
    to_string [|0x61u8|] -> "a", "a", "a"
    to_string [|0xf0u8; 0x80u8; 0x80u8|] -> None, "�", "�"
    to_string [|0xe0u8; 0x80u8|] -> None, "�", "�"
    to_string [|0xc0u8|] -> None, "�", "�"
    to_string [|0xf0u8; 0x80u8; 0x80u8; 0xf0u8|] -> None, "��", "��"
    to_string [|0xe0u8; 0x80u8; 0xe0u8|] -> None, "��", "��"
    to_string [|0xc0u8; 0xc0u8|] -> None, "��", "��"
    to_string [|0x80u8|] -> None, "�", "�"
    to_string [|0x80u8; 0x80u8; 0x80u8; 0x80u8|] -> None, "����", "����"
    to_string [|0x61u8; 0xc0u8; 0x62u8|] -> None, "a�b", "a�b"
    to_string [|0x61u8; 0xe0u8; 0x80u8; 0x63u8|] -> None, "a�c", "a�c"
    to_string [|0x61u8; 0xc0u8; 0x80u8; 0x80u8; 0x64u8|] -> None, "a��d", "a��d"
    to_string [|0x61u8; 0xffu8; 0x65u8|] -> None, "a�e", "a�e"
    to_string [|0x61u8; 0x3cu8; 0x62u8|] -> "a<b", "a<b", "a<b"
    to_string [|0x61u8; 0xc0u8; 0xbcu8; 0x62u8|] -> None, "a�b", "a�b"
    to_string [|0x61u8; 0xe0u8; 0x80u8; 0xbcu8; 0x62u8|] -> None, "a�b", "a�b"
    to_string [|0x61u8; 0xf0u8; 0x80u8; 0x80u8; 0xbcu8; 0x62u8|] -> None, "a�b", "a�b"
    to_string [|0x61u8; 0x3cu8|] -> "a<", "a<", "a<"
    to_string [|0x61u8; 0xc0u8; 0xbcu8|] -> None, "a�", "a�"
    to_string [|0x61u8; 0xe0u8; 0x80u8; 0xbcu8|] -> None, "a�", "a�"
    to_string [|0x61u8; 0xf0u8; 0x80u8; 0x80u8; 0xbcu8|] -> None, "a�", "a�"
    to_string [|0x3cu8; 0x62u8|] -> "<b", "<b", "<b"
    to_string [|0xc0u8; 0xbcu8; 0x62u8|] -> None, "�b", "�b"
    to_string [|0xe0u8; 0x80u8; 0xbcu8; 0x62u8|] -> None, "�b", "�b"
    to_string [|0xf0u8; 0x80u8; 0x80u8; 0xbcu8; 0x62u8|] -> None, "�b", "�b"
    to_string [|0x61u8; 0xc2u8; 0xabu8; 0x62u8|] -> "a«b", "a«b", "a«b"
    to_string [|0x61u8; 0xe0u8; 0x82u8; 0xabu8; 0x62u8|] -> None, "a�b", "a�b"
    to_string [|0x61u8; 0xf0u8; 0x80u8; 0x82u8; 0xabu8; 0x62u8|] -> None, "a�b", "a�b"
    to_string [|0x61u8; 0xc2u8; 0xabu8|] -> "a«", "a«", "a«"
    to_string [|0x61u8; 0xe0u8; 0x82u8; 0xabu8|] -> None, "a�", "a�"
    to_string [|0x61u8; 0xf0u8; 0x80u8; 0x82u8; 0xabu8|] -> None, "a�", "a�"
    to_string [|0xc2u8; 0xabu8; 0x62u8|] -> "«b", "«b", "«b"
    to_string [|0xe0u8; 0x82u8; 0xabu8; 0x62u8|] -> None, "�b", "�b"
    to_string [|0xf0u8; 0x80u8; 0x82u8; 0xabu8; 0x62u8|] -> None, "�b", "�b"
    to_string [|0x61u8; 0xe2u8; 0x80u8; 0xa1u8; 0x62u8|] -> "a‡b", "a‡b", "a‡b"
    to_string [|0x61u8; 0xf0u8; 0x82u8; 0x80u8; 0xa1u8; 0x62u8|] -> None, "a�b", "a�b"
    to_string [|0x61u8; 0xe2u8; 0x80u8; 0xa1u8|] -> "a‡", "a‡", "a‡"
    to_string [|0x61u8; 0xf0u8; 0x82u8; 0x80u8; 0xa1u8|] -> None, "a�", "a�"
    to_string [|0xe2u8; 0x80u8; 0xa1u8; 0x62u8|] -> "‡b", "‡b", "‡b"
    to_string [|0xf0u8; 0x82u8; 0x80u8; 0xa1u8; 0x62u8|] -> None, "�b", "�b"
    |}]
