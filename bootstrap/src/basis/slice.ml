open SliceIntf

module MakeMonoIter (Cursor : CursorIntf.SMonoIter) :
  SMonoIter
  with type container := Cursor.container
  with type cursor := Cursor.t
  with type elm := Cursor.elm = struct
  type t = {
    container: Cursor.container;
    base: Cursor.t;
    past: Cursor.t;
  }

  let init ?base ?past container =
    let base = match base with
      | Some cursor -> cursor
      | None -> Cursor.hd container
    in
    let past = match past with
      | Some cursor -> cursor
      | None -> Cursor.tl container
    in
    assert Cursor.(base <= past);
    {container; base; past}

  let container t =
    t.container

  let base t =
    t.base

  let past t =
    t.past

  let cursors t =
    t.base, t.past
end

module MakeMonoIndex (Cursor : CursorIntf.SMonoIndex) :
  SMonoIndex
  with type container := Cursor.container
  with type cursor := Cursor.t
  with type elm := Cursor.elm = struct
  type t = {
    container: Cursor.container;
    range: Range.Uns.t;
  }

  let init ?range container =
    match range with
    | None -> {container; range=Range.Uns.(0L =:< Cursor.index (Cursor.tl container))}
    | Some r -> begin
        assert (
          let base = Range.Uns.base r in
          match Range.Uns.limit r with
          | Excl past -> base <= past && past <= (Cursor.index (Cursor.tl container))
          | Incl last -> base < last && last < (Cursor.index (Cursor.tl container))
        );
        {container; range=r}
      end

  let of_cursors ~base ~past =
    let range = Range.Uns.( =:< ) (Cursor.index base) (Cursor.index past) in
    {container=Cursor.container base; range}

  let container t =
    t.container

  let range t =
    t.range

  let length t =
    Range.Uns.length_hlt t.range

  let base t =
    Cursor.seek (Uns.bits_to_sint (Range.Uns.base t.range)) (Cursor.hd t.container)

  let past t =
    let i = match Range.Uns.limit t.range with
      | Excl past -> past
      | Incl last -> Uns.succ last
    in
    Cursor.seek (Uns.bits_to_sint i) (Cursor.hd t.container)

  let cursors t =
    (base t), (past t)
end

module MakePolyIter (Cursor : CursorIntf.SPolyIter) :
  SPolyIter
  with type 'a container := 'a Cursor.container
  with type 'a cursor := 'a Cursor.t
  with type 'a elm := 'a Cursor.elm = struct
  type 'a t = {
    container: 'a Cursor.container;
    base: 'a Cursor.t;
    past: 'a Cursor.t;
  }

  let init ?base ?past container =
    let base = match base with
      | Some cursor -> cursor
      | None -> Cursor.hd container
    in
    let past = match past with
      | Some cursor -> cursor
      | None -> Cursor.tl container
    in
    assert Cursor.(base <= past);
    {container; base; past}

  let container t =
    t.container

  let base t =
    t.base

  let past t =
    t.past

  let cursors t =
    (t.base, t.past)
end

module MakePolyIndex (Cursor : CursorIntf.SPolyIndex) :
  SPolyIndex
  with type 'a container := 'a Cursor.container
  with type 'a cursor := 'a Cursor.t
  with type 'a elm := 'a Cursor.elm = struct
  type 'a t = {
    container: 'a Cursor.container;
    range: Range.Uns.t;
  }

  let init ?range container =
    match range with
    | None -> {container; range=Range.Uns.(0L =:< Cursor.index (Cursor.tl container))}
    | Some r -> begin
        assert (
          let base = Range.Uns.base r in
          match Range.Uns.limit r with
          | Excl past -> base <= past && past <= (Cursor.index (Cursor.tl container))
          | Incl last -> base < last && last < (Cursor.index (Cursor.tl container))
        );
        {container; range=r}
      end

  let of_cursors ~base ~past =
    let range = Range.Uns.( =:< ) (Cursor.index base) (Cursor.index past) in
    {container=Cursor.container base; range}

  let container t =
    t.container

  let range t =
    t.range

  let length t =
    Range.Uns.length_hlt t.range

  let base t =
    Cursor.seek (Uns.bits_to_sint (Range.Uns.base t.range)) (Cursor.hd t.container)

  let past t =
    let i = match Range.Uns.limit t.range with
      | Excl past -> past
      | Incl last -> Uns.succ last
    in
    Cursor.seek (Uns.bits_to_sint i) (Cursor.hd t.container)

  let cursors t =
    (base t), (past t)
end
