open Rudiments

let default_tabwidth = 8L

(* Line/column position independent of previous lines' contents, with signed column number to
 * facilitate backward cursoring. *)
module Spos = struct
  type t = {
    line: uns;
    (* Column number if non-negative, in which case conversion to t is trivial. If negative, the
     * column number for the equivalent t must be calculated via two-pass (backward, then forward)
     * scanning. *)
    scol: sint;
  }

  let init ~line ~scol =
    {line; scol}

  let pred cp t =
    match cp with
    | cp when Codepoint.(cp = nl) -> {line=Uns.pred t.line; scol=Sint.neg_one}
    | cp when Codepoint.(cp = ht) -> {t with scol=Sint.neg_one}
    | _ -> {t with scol=Sint.pred t.scol}

  let succ tabwidth cp t =
    match cp with
    | cp when Codepoint.(cp = nl) -> {line=Uns.succ t.line; scol=Sint.kv 0L}
    | cp when Codepoint.(cp = ht) ->
      {t with scol=Sint.(t.scol + (kv tabwidth) - (t.scol % (kv tabwidth)))}
    | _ -> {t with scol=Sint.succ t.scol}

  let line t =
    t.line

  let scol t =
    t.scol

  let pp t formatter =
    formatter
    |> Fmt.fmt "{line=" |> Uns.pp t.line
    |> Fmt.fmt "; scol=" |> Sint.pp t.scol
    |> Fmt.fmt "}"
end

(* Line/column position independent of previous lines' contents. *)
module Pos = struct
  module T = struct
    type t = {
      line: uns;
      col: uns;
    }

    let cmp t0 t1 =
      let open Cmp in
      match Uns.cmp t0.line t1.line with
      | Lt -> Lt
      | Eq -> Uns.cmp t0.col t1.col
      | Gt -> Gt

    let init ~line ~col =
      {line; col}

    let line t =
      t.line

    let col t =
      t.col

    let pp t formatter =
      formatter
      |> Uns.pp t.line
      |> Fmt.fmt ":"
      |> Uns.pp t.col
  end
  include T
  include Cmpable.Make(T)
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

  let pp = Uns.pp
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
  let base = {eind=0L; bytes=Bytes.Slice.init [||]}

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

  let pp t formatter =
    formatter
    |> Fmt.fmt "{eind=" |> Uns.pp t.eind
    |> Fmt.fmt "; bytes=" |> Bytes.Slice.pp t.bytes
    |> Fmt.fmt "}"

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
        {excerpt; index=0L}

      let tl excerpt =
        {excerpt; index=Bytes.Slice.length excerpt.bytes}

      let pred t =
        match t.index = 0L with
        | true -> halt "Cannot seek before beginning of excerpt"
        | false -> {t with index=Uns.pred t.index}

      let succ t =
        match t.index = length t.excerpt with
        | true -> halt "Cannot seek past end of excerpt"
        | false -> {t with index=Uns.succ t.index}

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

      let pp t formatter =
        formatter
        |> Fmt.fmt "{excerpt=" |> pp t.excerpt
        |> Fmt.fmt "; index=" |> Uns.pp t.index
        |> Fmt.fmt "}"
    end
    include T
    include Cmpable.Make(T)
  end
end

type t = {
  (* Filesystem path. *)
  path: string option;
  (* Tab width. *)
  tabwidth: uns;
  (* Excerpts already forced into text. The map is initialized with a base excerpt, which simplifies
   * various logic. *)
  excerpts: (uns, Excerpt.t, Uns.cmper_witness) Map.t;
  (* Lazy suspension which produces extended text. *)
  extend: t option Lazy.t;
}

let pp t formatter =
  formatter
  |> Fmt.fmt "{path=" |> (Option.pp String.pp) t.path
  |> Fmt.fmt "; tabwidth=" |> Uns.pp t.tabwidth
  |> Fmt.fmt "; ...}"

let of_bytes_stream ?path ?(tabwidth=default_tabwidth) stream =
  let rec susp_extend path pred_excerpt excerpts stream = lazy begin
    match Stream.is_empty stream with
    | true -> None
    | false -> begin
        let bytes, stream' = Stream.pop stream in
        let excerpt = Excerpt.of_bytes_slice pred_excerpt bytes in
        let excerpts' = Map.insert ~k:(Excerpt.eind excerpt) ~v:excerpt excerpts in
        let extend' = susp_extend path excerpt excerpts' stream' in
        let t' = {path; tabwidth; excerpts=excerpts'; extend=extend'} in
        Some t'
      end
  end in
  let excerpt = Excerpt.base in
  let excerpts = Map.singleton (module Uns) ~k:(Excerpt.eind excerpt) ~v:excerpt in
  let extend = susp_extend path excerpt excerpts stream in
  {path; tabwidth; excerpts; extend}

let of_string_slice ?path ?(tabwidth=default_tabwidth) slice =
  let susp_extend () = lazy None in
  let excerpt = Excerpt.(of_string_slice base slice) in
  let excerpts =
    Map.singleton (module Uns) ~k:Excerpt.base.eind ~v:Excerpt.base
    |> Map.insert ~k:excerpt.eind ~v:excerpt in
  let extend = susp_extend () in
  {path; tabwidth; excerpts; extend}

let path t =
  t.path

let tabwidth t =
  t.tabwidth

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
      (* Excerpt cursor, used for iterating over bytes within a single excerpt. Note that for the
       * positions between excerpts, there are two logically equivalent text cursors, one of which
       * can only handle lget, and the other of which can only handle rget. We make no effort to
       * amortize repeated conversions between equivalent text cursors to transparently support
       * lget/rget, under the assumption that no reasonable use case suffers more than constant
       * additional overhead. *)
      ecursor: Excerpt.Cursor.t;
    }

    let cmp t0 t1 =
      Vind.cmp t0.vind t1.vind

    let container t =
      t.text

    let index t =
      Vind.index t.vind

    let hd text =
      let excerpt = Map.get_hlt 0L text.excerpts in
      {
        text;
        vind=Vind.init 0L;
        spos=Spos.init ~line:1L ~scol:(Sint.kv 0L);
        ecursor=Excerpt.Cursor.hd excerpt;
      }

    let pp t formatter =
      formatter
      |> Fmt.fmt "{text=" |> pp t.text
      |> Fmt.fmt "; vind=" |> Vind.pp t.vind
      |> Fmt.fmt "; spos=" |> Spos.pp t.spos
      |> Fmt.fmt "; ecursor=" |> Excerpt.Cursor.pp t.ecursor
      |> Fmt.fmt "}"

    module CodepointSeq = struct
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
              match (Uns.succ Excerpt.(eind (Cursor.container t.ecursor))) < (Map.length
                  t.text.excerpts) with
              | true -> begin
                  let excerpt' = Map.get_hlt (Uns.succ t.ecursor.excerpt.eind) t.text.excerpts in
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

    let nextv_opt t =
      match CodepointSeq.(to_codepoint (init t)) with
      | None -> None
      | Some (Valid (cp, t')) -> begin
          let vincr = Codepoint.Utf8.length_of_codepoint cp in
          let vind' = Vind.(init ((index t.vind) + vincr)) in
          let spos' = Spos.succ (tabwidth (container t)) cp t.spos in
          Some (cp, true, {t' with vind=vind'; spos=spos'})
        end
      | Some (Invalid t') -> begin
          let cp = Codepoint.replacement in
          let vincr = Codepoint.Utf8.length_of_codepoint cp in
          let vind' = Vind.(init ((index t.vind) + vincr)) in
          let spos' = Spos.succ (tabwidth (container t)) cp t.spos in
          Some (cp, false, {t' with vind=vind'; spos=spos'})
        end

    let next_opt t =
      match nextv_opt t with
      | Some (cp, _, t) -> Some (cp, t)
      | None -> None

    let rget_opt t =
      match next_opt t with
      | None -> None
      | Some (cp, _) -> Some cp

    let rgetv_opt t =
      match nextv_opt t with
      | None -> None
      | Some (cp, valid, _) -> Some (cp, valid)

    let rvalid t =
      match nextv_opt t with
      | None -> halt "Out of bounds"
      | Some (_, valid, _) -> valid

    let rget t =
      match rget_opt t with
      | None -> halt "Out of bounds"
      | Some cp -> cp

    let nextv t =
      match nextv_opt t with
      | None -> halt "Out of bounds"
      | Some (cp, valid, t') -> cp, valid, t'

    let next t =
      match nextv t with cp, _, t' -> cp, t'

    let succ t =
      match next t with _, t' -> t'

    let seek_fwd offset t =
      let rec fn offset t = begin
        match offset with
        | 0L -> t
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

    module CodepointRevSeq = struct
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
              match (Excerpt.eind t.ecursor.excerpt) > 0L with
              | true -> begin
                  let excerpt' = Map.get_hlt (Uns.pred t.ecursor.excerpt.eind) t.text.excerpts in
                  let ecursor' = Excerpt.Cursor.tl excerpt' in
                  next {t with ecursor=ecursor'}
                end
              | false -> None
            end
      end
      include T
      include Codepoint.Seq.MakeRev(T)
    end

    let prevv t =
      match CodepointRevSeq.(to_codepoint (init t)) with
      | None -> halt "Out of bounds"
      | Some (Valid (cp, t')) -> begin
          let vdecr = Codepoint.Utf8.length_of_codepoint cp in
          let vind' = Vind.(init ((index t.vind) - vdecr)) in
          let spos' = Spos.pred cp t.spos in
          cp, true, {t' with vind=vind'; spos=spos'}
        end
      | Some (Invalid t') -> begin
          let cp = Codepoint.replacement in
          let vdecr = Codepoint.Utf8.length_of_codepoint cp in
          let vind' = Vind.(init ((index t.vind) - vdecr)) in
          let spos' = Spos.pred cp t.spos in
          cp, false, {t' with vind=vind'; spos=spos'}
        end

    let prev t =
      match prevv t with cp, _, t' -> cp, t'

    let lvalid t =
      match CodepointRevSeq.(to_codepoint (init t)) with
      | None -> halt "Out of bounds"
      | Some (Valid _) -> true
      | Some (Invalid _) -> false

    let lget t =
      match prev t with cp, _ -> cp

    let pred t =
      match prev t with _, t' -> t'

    let seek_rev offset t =
      let rec fn offset t = begin
        match offset with
        | 0L -> t
        | _ -> fn (Uns.pred offset) (pred t)
      end in
      fn offset t

    let seek offset t =
      if Sint.(offset < 0L) then seek_rev (Uns.bits_of_sint (Sint.neg offset)) t
      else seek_fwd (Uns.bits_of_sint offset) t

    let pos t =
      let col0_delta t = begin
        let rec seek_col0 t = begin
          match t.vind = 0L with
          | true -> t
          | false -> begin
              let cp, t' = prev t in
              match cp with
              | cp when Codepoint.(cp = nl) -> t
              | _ -> seek_col0 t'
            end
        end in
        let rec col_delta tabwidth t0 t1 i = begin
          match cmp t0 t1 with
          | Lt -> begin
              let i' = match rget t0 with
                | cp when Codepoint.(cp = ht) -> i + tabwidth - (i % tabwidth)
                | _ -> Uns.succ i
              in
              col_delta tabwidth (succ t0) t1 i'
            end
          | Eq -> i
          | Gt -> not_reached ()
        end in
        col_delta (tabwidth (container t)) (seek_col0 t) t 0L
      end in
      let col = match Sint.is_negative (Spos.scol t.spos) with
        | false -> Uns.bits_of_sint (Spos.scol t.spos)
        | true -> col0_delta t
      in
      {Pos.line=Spos.line t.spos; col}
  end
  include T
  include Cmpable.Make(T)
end

module Slice = struct
  include Slice.MakeMonoIter(Cursor)

  module StringSeq = struct
    module T = struct
      type t = {
        base: Cursor.t;
        past: Cursor.t;
      }

      let init slice =
        let base, past = cursors slice in
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
    StringSeq.(to_string (init t))

  let pp t formatter =
    formatter |> Fmt.fmt (to_string t)
end
