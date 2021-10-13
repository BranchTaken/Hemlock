open SliceIntf

module MakeMonoIter (Cursor : CursorIntf.SMonoIter) :
  SMonoIter
  with type container := Cursor.container
  with type cursor := Cursor.t
  with type elm := Cursor.elm

module MakeMonoIndex (Cursor : CursorIntf.SMonoIndex) :
  SMonoIndex
  with type container := Cursor.container
  with type cursor := Cursor.t
  with type elm := Cursor.elm

module MakePolyIter (Cursor : CursorIntf.SPolyIter) :
  SPolyIter
  with type 'a container := 'a Cursor.container
  with type 'a cursor := 'a Cursor.t
  with type 'a elm := 'a Cursor.elm

module MakePolyIndex (Cursor : CursorIntf.SPolyIndex) :
  SPolyIndex
  with type 'a container := 'a Cursor.container
  with type 'a cursor := 'a Cursor.t
  with type 'a elm := 'a Cursor.elm
