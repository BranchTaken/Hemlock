open SliceIntf

module MakeMono (Cursor : CursorIntf.SMono) :
  SMono
  with type container := Cursor.container
  with type cursor := Cursor.t
  with type elm := Cursor.elm

module MakePoly (Cursor : CursorIntf.SPoly) :
  SPoly
  with type 'a container := 'a Cursor.container
  with type 'a cursor := 'a Cursor.t
  with type 'a elm := 'a Cursor.elm
