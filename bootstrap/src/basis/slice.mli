open Slice_intf

module Make_mono (Cursor : Cursor_intf.S_mono) :
  S_mono with type container := Cursor.container
         with type cursor := Cursor.t
         with type elm := Cursor.elm

module Make_poly (Cursor : Cursor_intf.S_poly) :
  S_poly with type 'a container := 'a Cursor.container
         with type 'a cursor := 'a Cursor.t
         with type 'a elm := 'a Cursor.elm
