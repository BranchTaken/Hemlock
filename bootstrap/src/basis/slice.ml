open SliceIntf

module MakeMono (Cursor : CursorIntf.SMono) :
  SMono
  with type container := Cursor.container
  with type cursor := Cursor.t
  with type elm := Cursor.elm = struct
  type t = {
    base: Cursor.t;
    past: Cursor.t;
  }

  let of_cursors ~base ~past =
    {base; past}

  let to_cursors t =
    (t.base, t.past)

  let container t =
    Cursor.container t.base

  let base t =
    t.base

  let past t =
    t.past

  let range t =
    Range.(Cursor.index t.base =:< Cursor.index t.past)

  let of_container ?range container =
    match range with
    | None -> of_cursors ~base:(Cursor.hd container) ~past:(Cursor.tl container)
    | Some r -> begin
        let base = Cursor.seek (Uns.bits_to_sint (Range.base r)) (Cursor.hd container) in
        let past = Cursor.seek (Uns.bits_to_sint (Range.length r)) base in
        of_cursors ~base ~past
      end

  let to_container t =
    Cursor.container t.base

  let base_seek coffset t =
    let base' = Cursor.seek coffset t.base in
    {t with base=base'}

  let base_succ t =
    let base' = Cursor.succ t.base in
    {t with base=base'}

  let base_pred t =
    let base' = Cursor.pred t.base in
    {t with base=base'}

  let past_seek coffset t =
    let past' = Cursor.seek coffset t.past in
    {t with past=past'}

  let past_succ t =
    let past' = Cursor.succ t.past in
    {t with past=past'}

  let past_pred t =
    let past' = Cursor.pred t.past in
    {t with past=past'}
end

module MakePoly (Cursor : CursorIntf.SPoly) :
  SPoly
  with type 'a container := 'a Cursor.container
  with type 'a cursor := 'a Cursor.t
  with type 'a elm := 'a Cursor.elm = struct
  type 'a t = {
    base: 'a Cursor.t;
    past: 'a Cursor.t;
  }

  let of_cursors ~base ~past =
    {base; past}

  let to_cursors t =
    (t.base, t.past)

  let container t =
    Cursor.container t.base

  let base t =
    t.base

  let past t =
    t.past

  let range t =
    Range.(Cursor.index t.base =:< Cursor.index t.past)

  let of_container ?range container =
    match range with
    | None -> of_cursors ~base:(Cursor.hd container) ~past:(Cursor.tl container)
    | Some r -> begin
        let base = Cursor.seek (Uns.bits_to_sint (Range.base r)) (Cursor.hd container) in
        let past = Cursor.seek (Uns.bits_to_sint (Range.length r)) base in
        of_cursors ~base ~past
      end

  let to_container t =
    Cursor.container t.base

  let base_seek coffset t =
    let base' = Cursor.seek coffset t.base in
    {t with base=base'}

  let base_succ t =
    let base' = Cursor.succ t.base in
    {t with base=base'}

  let base_pred t =
    let base' = Cursor.pred t.base in
    {t with base=base'}

  let past_seek coffset t =
    let past' = Cursor.seek coffset t.past in
    {t with past=past'}

  let past_succ t =
    let past' = Cursor.succ t.past in
    {t with past=past'}

  let past_pred t =
    let past' = Cursor.pred t.past in
    {t with past=past'}
end
