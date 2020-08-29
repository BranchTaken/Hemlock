open Rudiments

(* Line/column position independent of previous lines' contents, with signed
 * column number to facilitate backward cursoring. *)
module Spos = struct
  type t = {
    line: uns;
    (* Column number if non-negative, in which case conversion to t is trivial.
     * If negative, the column number for the equivalent t must be calculated
     * via backward iteration. *)
    scol: sint;
  }

  let init ~line ~scol =
    {line; scol}

  let succ cp t =
    match cp with
    | cp when Codepoint.(cp = nl) -> {line=Uns.succ t.line; scol=Sint.kv 0}
    | _ -> {t with scol=Sint.succ t.scol}

  let pred cp t =
    match cp with
    | cp when Codepoint.(cp = nl) ->
      {line=Uns.pred t.line; scol=Sint.neg_one}
    | _ -> {t with scol=Sint.pred t.scol}

  let line t =
    t.line

  let scol t =
    t.scol
end

(* Line/column position independent of previous lines' contents. *)
module Pos = struct
  type t = {
    line: uns;
    col: uns;
  }

  let init ~line ~col =
    {line; col}

  let line t =
    t.line

  let col t =
    t.col
end

(* Absolute virtual index relative to beginning of text. *)
module Vind = struct
  type t = uns

  let cmp t0 t1 =
    Uns.cmp t0 t1

  let init vind =
    vind

  let index t =
    t
end

(* Text excerpt. *)
module Excerpt = struct
  type t = {
    (* Excerpt index; base is 0. *)
    eind: uns;
    (* Raw excerpt. *)
    bytes: Bytes.Slice.t;
  }

  (* Base excerpt, always hd of excerpts maps. *)
  let base = {eind=0; bytes=Bytes.Slice.of_container [||]}

  (* val of_bytes_slice: t -> Bytes.Slice.t -> t *)
  let of_bytes_slice pred bytes =
    let eind = Uns.succ pred.eind in
    {eind; bytes}

  (* val of_string_slice: t -> Bytes.Slice.t -> t *)
  let of_string_slice pred slice =
    let eind = Uns.succ pred.eind in
    let bytes = Bytes.Slice.of_string_slice slice in
    {eind; bytes}

  let eind t =
    t.eind

  let length t =
    Bytes.Slice.length t.bytes

  let get i t =
    Bytes.Slice.get i t.bytes

  module Cursor = struct
    module T = struct
      type container = t
      type t = {
        excerpt: container;
        index: uns;
      }

      let cmp t0 t1 =
        Uns.cmp t0.index t1.index

      let hd excerpt =
        {excerpt; index=0}

      let tl excerpt =
        {excerpt; index=Bytes.Slice.length excerpt.bytes}

      let succ t =
        match t.index = length t.excerpt with
        | true -> halt "Cannot seek past end of excerpt"
        | false -> {t with index=Uns.succ t.index}

      let pred t =
        match t.index = 0 with
        | true -> halt "Cannot seek before beginning of excerpt"
        | false -> {t with index=Uns.pred t.index}

      let lget t =
        get (Uns.pred t.index) t.excerpt

      let rget t =
        get t.index t.excerpt

      let prev t =
        lget t, pred t

      let next t =
        rget t, succ t

      let container t =
        t.excerpt
    end
    include T
    include Cmpable.Make(T)
  end
end

type t = {
  (* Filesystem path. *)
  path: string option;
  (* Excerpts already forced into text. The map is initialized with a base
   * excerpt, which simplifies various logic. *)
  excerpts: (uns, Excerpt.t, Uns.cmper_witness) Map.t;
  (* Lazy suspension which produces extended text. *)
  extend: t option Lazy.t;
}

let of_bytes_stream ?path stream =
  let rec susp_extend path pred_excerpt excerpts stream = lazy begin
    match Stream.is_empty stream with
    | true -> None
    | false -> begin
        let bytes, stream' = Stream.pop stream in
        let excerpt = Excerpt.of_bytes_slice pred_excerpt bytes in
        let excerpts' =
          Map.insert ~k:(Excerpt.eind excerpt) ~v:excerpt excerpts in
        let extend' = susp_extend path excerpt excerpts' stream' in
        let t' = {path; excerpts=excerpts'; extend=extend'} in
        Some t'
      end
  end in
  let excerpt = Excerpt.base in
  let excerpts =
    Map.singleton (module Uns) ~k:(Excerpt.eind excerpt) ~v:excerpt in
  let extend = susp_extend path excerpt excerpts stream in
  {path; excerpts; extend}

let of_string_slice ?path slice =
  let susp_extend () = lazy None in
  let excerpt = Excerpt.(of_string_slice base slice) in
  let excerpts =
    Map.singleton (module Uns) ~k:Excerpt.base.eind ~v:Excerpt.base
    |> Map.insert ~k:excerpt.eind ~v:excerpt in
  let extend = susp_extend () in
  {path; excerpts; extend}

let path t =
  t.path

module Cursor = struct
  module T = struct
    type container = t
    type elm = codepoint
    type t = {
      text: container;
      (* Virtual byte index, as if all encoding errors were replaced. *)
      vind: Vind.t;
      (* Line/column. *)
      spos: Spos.t;
      (* Excerpt cursor, used for iterating over bytes within a single excerpt.
       * Note that for the positions between excerpts, there are two logically
       * equivalent text cursors, one of which can only handle lget, and the
       * other of which can only handle rget. We make no effort to amortize
       * repeated conversions between equivalent text cursors to transparently
       * support lget/rget, under the assumption that no reasonable use case
       * suffers more than constant additional overhead. *)
      ecursor: Excerpt.Cursor.t;
    }

    let cmp t0 t1 =
      Vind.cmp t0.vind t1.vind

    let container t =
      t.text

    let index t =
      Vind.index t.vind

    let hd text =
      let excerpt = Map.get_hlt 0 text.excerpts in
      {
        text;
        vind=Vind.init 0;
        spos=Spos.init ~line:1 ~scol:(Sint.kv 0);
        ecursor=Excerpt.Cursor.hd excerpt;
      }

    module Codepoint_seq = struct
      module T = struct
        type nonrec t = t

        let init cursor =
          cursor

        let rec next t =
          match Excerpt.Cursor.(t.ecursor < tl (container t.ecursor)) with
          | true -> begin
              let b, ecursor' = Excerpt.Cursor.next t.ecursor in
              Some (b, {t with ecursor=ecursor'})
            end
          | false -> begin
              match (Uns.succ Excerpt.(eind (Cursor.container t.ecursor))) <
                (Map.length t.text.excerpts) with
              | true -> begin
                  let excerpt' = Map.get_hlt (Uns.succ t.ecursor.excerpt.eind)
                    t.text.excerpts in
                  let ecursor' = Excerpt.Cursor.hd excerpt' in
                  next {t with ecursor=ecursor'}
                end
              | false -> begin
                  match Lazy.force (t.text.extend) with
                  | None -> None
                  | Some text' -> next {t with text=text'}
                end
            end
      end
      include T
      include Codepoint.Seq.Make(T)
    end

    let next_opt t =
      match Codepoint_seq.(to_codepoint (init t)) with
      | None -> None
      | Some (Valid (cp, t')) -> begin
          let vincr = Codepoint.Utf8.length_of_codepoint cp in
          let vind' = Vind.(init ((index t.vind) + vincr)) in
          let spos' = Spos.succ cp t.spos in
          Some (cp, {t' with vind=vind'; spos=spos'})
        end
      | Some (Invalid t') -> begin
          let cp = Codepoint.replacement in
          let vincr = Codepoint.Utf8.length_of_codepoint cp in
          let vind' = Vind.(init ((index t.vind) + vincr)) in
          let spos' = Spos.succ cp t.spos in
          Some (cp, {t' with vind=vind'; spos=spos'})
        end

    let rget_opt t =
      match next_opt t with
      | None -> None
      | Some (cp, _) -> Some cp

    let rget t =
      match rget_opt t with
      | None -> halt "Out of bounds"
      | Some cp -> cp

    let next t =
      match next_opt t with
      | None -> halt "Out of bounds"
      | Some (cp, t') -> cp, t'

    let succ t =
      match next t with _, t' -> t'

    let seek_fwd offset t =
      let rec fn offset t = begin
        match offset with
        | 0 -> t
        | _ -> fn (Uns.pred offset) (succ t)
      end in
      fn offset t

    let tl text =
      let rec fn cursor = begin
        match next_opt cursor with
        | None -> cursor
        | Some (_, cursor') -> fn cursor'
      end in
      fn (hd text)

    module Codepoint_rev_seq = struct
      module T = struct
        type nonrec t = t

        let init cursor =
          cursor

        let rec next t =
          match Excerpt.Cursor.(t.ecursor > hd (container t.ecursor)) with
          | true -> begin
              let b, ecursor' = Excerpt.Cursor.prev t.ecursor in
              Some (b, {t with ecursor=ecursor'})
            end
          | false -> begin
              match (Excerpt.eind t.ecursor.excerpt) > 0 with
              | true -> begin
                  let excerpt' = Map.get_hlt (Uns.pred t.ecursor.excerpt.eind)
                    t.text.excerpts in
                  let ecursor' = Excerpt.Cursor.tl excerpt' in
                  next {t with ecursor=ecursor'}
                end
              | false -> None
            end
      end
      include T
      include Codepoint.Seq.Make_rev(T)
    end

    let prev t =
      match Codepoint_rev_seq.(to_codepoint (init t)) with
      | None -> halt "Out of bounds"
      | Some (Valid (cp, t')) -> begin
          let vdecr = Codepoint.Utf8.length_of_codepoint cp in
          let vind' = Vind.(init ((index t.vind) - vdecr)) in
          let spos' = Spos.pred cp t.spos in
          cp, {t' with vind=vind'; spos=spos'}
        end
      | Some (Invalid t') -> begin
          let cp = Codepoint.replacement in
          let vdecr = Codepoint.Utf8.length_of_codepoint cp in
          let vind' = Vind.(init ((index t.vind) - vdecr)) in
          let spos' = Spos.pred cp t.spos in
          cp, {t' with vind=vind'; spos=spos'}
        end

    let lget t =
      match prev t with cp, _ -> cp

    let pred t =
      match prev t with _, t' -> t'

    let seek_rev offset t =
      let rec fn offset t = begin
        match offset with
        | 0 -> t
        | _ -> fn (Uns.pred offset) (pred t)
      end in
      fn offset t

    let seek offset t =
      if Sint.(offset < kv 0) then seek_rev (Uns.of_sint (Sint.neg offset)) t
      else seek_fwd (Uns.of_sint offset) t

    let pos t =
      let rec col0_delta t i = begin
        match t.vind = 0 with
        | true -> i
        | false -> begin
            let cp, t' = prev t in
            match cp with
            | cp when Codepoint.(cp = nl) -> i
            | _ -> col0_delta t' (Uns.succ i)
          end
      end in
      let col = match Sint.is_negative (Spos.scol t.spos) with
        | false -> Uns.of_sint (Spos.scol t.spos)
        | true -> col0_delta t 0
      in
      {Pos.line=Spos.line t.spos; col}
  end
  include T
  include Cmpable.Make(T)
end

module Slice = struct
  include Slice.Make_mono(Cursor)

  module String_seq = struct
    module T = struct
      type t = {
        base: Cursor.t;
        past: Cursor.t;
      }

      let init slice =
        let base, past = to_cursors slice in
        {base; past}

      let length t =
        (Cursor.index t.past) - (Cursor.index t.base)

      let next t =
        let cp, base' = Cursor.next t.base in
        cp, {t with base=base'}
    end
    include T
    include String.Seq.Codepoint.Make(T)
  end

  let to_string t =
    String_seq.(to_string (init t))
end

(******************************************************************************)
(* Begin tests. *)

let%expect_test "path" =
  let open Format in
  printf "@[<h>";
  let text_path = path in
  let test_path ?path s = begin
    let text = of_string_slice ?path (String.Slice.of_string s) in
    printf "%a -> %a\n"
      (Option.pp String.pp) path
      (Option.pp String.pp) (text_path text)
  end in
  test_path "";
  test_path ~path:"/foo/bar" "";
  printf "@]";

  [%expect{|
    None -> None
    Some "/foo/bar" -> Some "/foo/bar"
    |}]

let%expect_test "of_string_slice" =
  let open Format in
  printf "@[<h>";
  let fn s = begin
    let text = of_string_slice (String.Slice.of_string s) in
    let slice =
      Slice.of_cursors ~base:(Cursor.hd text) ~past:(Cursor.tl text) in
    let s' = Slice.to_string slice in
    printf "%a -> %a\n" String.pp s String.pp s'
  end in
  fn "";
  fn "Hello";
  printf "@]";

  [%expect{|
    "" -> ""
    "Hello" -> "Hello"
    |}]

let stream_of_bytes_list bl =
  Stream.init_indef bl ~f:(fun bl ->
    match bl with
    | [] -> None
    | bytes :: bl' -> Some (bytes, bl')
  )

let stream_of_string_list sl =
  stream_of_bytes_list (List.map sl ~f:(fun s ->
    Bytes.Slice.of_string_slice (String.Slice.of_string s)))

let stream_of_byte_list bl =
  stream_of_bytes_list (List.map bl ~f:(fun b ->
    let bytes = [|b|] in
    Bytes.(Slice.of_cursors ~base:(Cursor.hd bytes) ~past:(Cursor.tl bytes))
  ))

let%expect_test "of_bytes_stream" =
  let open Format in
  printf "@[<h>";
  let fn sl = begin
    printf "%a\n" (List.pp String.pp) sl;
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
    printf "  fwd   -> index=%u \"" (Cursor.index hd);
    let tl = fwd_iter hd in
    printf "\"\n";

    let rec rev_iter cursor = begin
      match Cursor.index cursor > 0 with
      | false -> cursor
      | true -> begin
          let cp, cursor' = Cursor.prev cursor in
          printf "%s" (Codepoint.to_string cp);
          rev_iter cursor'
        end
    end in
    printf "  rev   -> index=%u \"" (Cursor.index tl);
    let hd' = rev_iter tl in
    printf "\"\n";
    assert Cursor.(hd text = hd');

    let slice =
      Slice.of_cursors ~base:(Cursor.hd text) ~past:(Cursor.tl text) in
    let s' = Slice.to_string slice in
    printf "  slice -> %a\n" String.pp s'
  end in
  fn [];
  fn [""];
  fn [""; ""];
  fn ["Hello"];
  fn ["Hello"; " "; "Goodbye"];
  printf "@]";

  [%expect{|
    []
      fwd   -> index=0 ""
      rev   -> index=0 ""
      slice -> ""
    [""]
      fwd   -> index=0 ""
      rev   -> index=0 ""
      slice -> ""
    [""; ""]
      fwd   -> index=0 ""
      rev   -> index=0 ""
      slice -> ""
    ["Hello"]
      fwd   -> index=0 "Hello"
      rev   -> index=5 "olleH"
      slice -> "Hello"
    ["Hello"; " "; "Goodbye"]
      fwd   -> index=0 "Hello Goodbye"
      rev   -> index=13 "eybdooG olleH"
      slice -> "Hello Goodbye"
    |}]

let%expect_test "of_bytes_stream_replace" =
  let open Format in
  printf "@[<h>";
  let fn bl = begin
    printf "%a\n" (List.pp Byte.pp_x) bl;
    let text = of_bytes_stream (stream_of_byte_list bl) in

    let rec fwd_iter cursor = begin
      match Cursor.next_opt cursor with
      | None -> cursor
      | Some (cp, cursor') -> begin
          printf "%s" (Codepoint.to_string cp);
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
          printf "%s" (Codepoint.to_string cp);
          rev_iter cursor'
        end
    end in
    printf "  rev   -> index=%u \"" (Cursor.index tl);
    let hd' = rev_iter tl in
    printf "\"\n";
    assert Cursor.(hd text = hd');

    let slice =
      Slice.of_cursors ~base:(Cursor.hd text) ~past:(Cursor.tl text) in
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
  printf "@]";

  [%expect{|
    [0x61u8]
      fwd   -> index=0 "a"
      rev   -> index=1 "a"
      slice -> "a"
    [0xf0u8; 0x80u8; 0x80u8]
      fwd   -> index=0 "�"
      rev   -> index=3 "�"
      slice -> "�"
    [0xe0u8; 0x80u8]
      fwd   -> index=0 "�"
      rev   -> index=3 "�"
      slice -> "�"
    [0xc0u8]
      fwd   -> index=0 "�"
      rev   -> index=3 "�"
      slice -> "�"
    [0xf0u8; 0x80u8; 0x80u8; 0xf0u8]
      fwd   -> index=0 "��"
      rev   -> index=6 "��"
      slice -> "��"
    [0xe0u8; 0x80u8; 0xe0u8]
      fwd   -> index=0 "��"
      rev   -> index=6 "��"
      slice -> "��"
    [0xc0u8; 0xc0u8]
      fwd   -> index=0 "��"
      rev   -> index=6 "��"
      slice -> "��"
    [0x80u8]
      fwd   -> index=0 "�"
      rev   -> index=3 "�"
      slice -> "�"
    [0x80u8; 0x80u8; 0x80u8; 0x80u8]
      fwd   -> index=0 "����"
      rev   -> index=12 "����"
      slice -> "����"
    [0x61u8; 0xc0u8; 0x62u8]
      fwd   -> index=0 "a�b"
      rev   -> index=5 "b�a"
      slice -> "a�b"
    [0x61u8; 0xe0u8; 0x80u8; 0x63u8]
      fwd   -> index=0 "a�c"
      rev   -> index=5 "c�a"
      slice -> "a�c"
    [0x61u8; 0xc0u8; 0x80u8; 0x80u8; 0x64u8]
      fwd   -> index=0 "a��d"
      rev   -> index=8 "d��a"
      slice -> "a��d"
    [0x61u8; 0xffu8; 0x65u8]
      fwd   -> index=0 "a�e"
      rev   -> index=5 "e�a"
      slice -> "a�e"
    [0x61u8; 0x3cu8; 0x62u8]
      fwd   -> index=0 "a<b"
      rev   -> index=3 "b<a"
      slice -> "a<b"
    [0x61u8; 0xc0u8; 0xbcu8; 0x62u8]
      fwd   -> index=0 "a�b"
      rev   -> index=5 "b�a"
      slice -> "a�b"
    [0x61u8; 0xe0u8; 0x80u8; 0xbcu8; 0x62u8]
      fwd   -> index=0 "a�b"
      rev   -> index=5 "b�a"
      slice -> "a�b"
    [0x61u8; 0xf0u8; 0x80u8; 0x80u8; 0xbcu8; 0x62u8]
      fwd   -> index=0 "a�b"
      rev   -> index=5 "b�a"
      slice -> "a�b"
    [0x61u8; 0x3cu8]
      fwd   -> index=0 "a<"
      rev   -> index=2 "<a"
      slice -> "a<"
    [0x61u8; 0xc0u8; 0xbcu8]
      fwd   -> index=0 "a�"
      rev   -> index=4 "�a"
      slice -> "a�"
    [0x61u8; 0xe0u8; 0x80u8; 0xbcu8]
      fwd   -> index=0 "a�"
      rev   -> index=4 "�a"
      slice -> "a�"
    [0x61u8; 0xf0u8; 0x80u8; 0x80u8; 0xbcu8]
      fwd   -> index=0 "a�"
      rev   -> index=4 "�a"
      slice -> "a�"
    [0x3cu8; 0x62u8]
      fwd   -> index=0 "<b"
      rev   -> index=2 "b<"
      slice -> "<b"
    [0xc0u8; 0xbcu8; 0x62u8]
      fwd   -> index=0 "�b"
      rev   -> index=4 "b�"
      slice -> "�b"
    [0xe0u8; 0x80u8; 0xbcu8; 0x62u8]
      fwd   -> index=0 "�b"
      rev   -> index=4 "b�"
      slice -> "�b"
    [0xf0u8; 0x80u8; 0x80u8; 0xbcu8; 0x62u8]
      fwd   -> index=0 "�b"
      rev   -> index=4 "b�"
      slice -> "�b"
    [0x61u8; 0xc2u8; 0xabu8; 0x62u8]
      fwd   -> index=0 "a«b"
      rev   -> index=4 "b«a"
      slice -> "a«b"
    [0x61u8; 0xe0u8; 0x82u8; 0xabu8; 0x62u8]
      fwd   -> index=0 "a�b"
      rev   -> index=5 "b�a"
      slice -> "a�b"
    [0x61u8; 0xf0u8; 0x80u8; 0x82u8; 0xabu8; 0x62u8]
      fwd   -> index=0 "a�b"
      rev   -> index=5 "b�a"
      slice -> "a�b"
    [0x61u8; 0xc2u8; 0xabu8]
      fwd   -> index=0 "a«"
      rev   -> index=3 "«a"
      slice -> "a«"
    [0x61u8; 0xe0u8; 0x82u8; 0xabu8]
      fwd   -> index=0 "a�"
      rev   -> index=4 "�a"
      slice -> "a�"
    [0x61u8; 0xf0u8; 0x80u8; 0x82u8; 0xabu8]
      fwd   -> index=0 "a�"
      rev   -> index=4 "�a"
      slice -> "a�"
    [0xc2u8; 0xabu8; 0x62u8]
      fwd   -> index=0 "«b"
      rev   -> index=3 "b«"
      slice -> "«b"
    [0xe0u8; 0x82u8; 0xabu8; 0x62u8]
      fwd   -> index=0 "�b"
      rev   -> index=4 "b�"
      slice -> "�b"
    [0xf0u8; 0x80u8; 0x82u8; 0xabu8; 0x62u8]
      fwd   -> index=0 "�b"
      rev   -> index=4 "b�"
      slice -> "�b"
    [0x61u8; 0xe2u8; 0x80u8; 0xa1u8; 0x62u8]
      fwd   -> index=0 "a‡b"
      rev   -> index=5 "b‡a"
      slice -> "a‡b"
    [0x61u8; 0xf0u8; 0x82u8; 0x80u8; 0xa1u8; 0x62u8]
      fwd   -> index=0 "a�b"
      rev   -> index=5 "b�a"
      slice -> "a�b"
    [0x61u8; 0xe2u8; 0x80u8; 0xa1u8]
      fwd   -> index=0 "a‡"
      rev   -> index=4 "‡a"
      slice -> "a‡"
    [0x61u8; 0xf0u8; 0x82u8; 0x80u8; 0xa1u8]
      fwd   -> index=0 "a�"
      rev   -> index=4 "�a"
      slice -> "a�"
    [0xe2u8; 0x80u8; 0xa1u8; 0x62u8]
      fwd   -> index=0 "‡b"
      rev   -> index=4 "b‡"
      slice -> "‡b"
    [0xf0u8; 0x82u8; 0x80u8; 0xa1u8; 0x62u8]
      fwd   -> index=0 "�b"
      rev   -> index=4 "b�"
      slice -> "�b"
    |}]

let%expect_test "pos" =
  let open Format in
  printf "@[<h>";
  let fn sl = begin
    printf "%a\n" (List.pp String.pp) sl;
    let text = of_bytes_stream (stream_of_string_list sl) in

    let rec fwd_iter ~line ~col cursor = begin
      match Cursor.next_opt cursor with
      | None -> cursor
      | Some (cp, cursor') -> begin
          let pos = Cursor.pos cursor' in
          let () = match cp with
            | cp when Codepoint.(cp = nl) -> begin
                assert (Pos.line pos = Uns.succ line);
                assert (Pos.col pos = 0)
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
    let tl = fwd_iter ~line:1 ~col:0 (Cursor.hd text) in
    let pos = Cursor.pos tl in
    printf "  pos tl = %u:%u\n" (Pos.line pos) (Pos.col pos);

    let rec rev_iter ~line ~col cursor = begin
      match Cursor.index cursor > 0 with
      | false -> cursor
      | true -> begin
          let cp, cursor' = Cursor.prev cursor in
          let pos = Cursor.pos cursor' in
          let () = match cp with
            | cp when Codepoint.(cp = nl) -> begin
                assert (Pos.line pos = Uns.pred line)
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
    printf "  pos hd = %u:%u\n"
      (Pos.line (Cursor.pos hd'))
      (Pos.col (Cursor.pos hd'));
    assert Cursor.(hd text = hd');
  end in
  fn [""];
  fn ["Hello"];
  fn ["Hello"; "\n"; "Goodbye"];
  fn ["A"; "\n"; "B"; "\n"];
  fn ["A"; "\n"; "B"; "\n"; "C"];
  fn ["A"; "\n"; "\n"; "C"];
  printf "@]";

  [%expect{|
    [""]
      pos tl = 1:0
      pos hd = 1:0
    ["Hello"]
      pos tl = 1:5
      pos hd = 1:0
    ["Hello"; "\n"; "Goodbye"]
      pos tl = 2:7
      pos hd = 1:0
    ["A"; "\n"; "B"; "\n"]
      pos tl = 3:0
      pos hd = 1:0
    ["A"; "\n"; "B"; "\n"; "C"]
      pos tl = 3:1
      pos hd = 1:0
    ["A"; "\n"; "\n"; "C"]
      pos tl = 3:1
      pos hd = 1:0
    |}]
