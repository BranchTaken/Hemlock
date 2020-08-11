open Slice_intf

module Make_mono (Cursor : Cursor_intf.S_mono) :
  S_mono with type container := Cursor.container
          and type cursor := Cursor.t
          and type elm := Cursor.elm

module Make_poly (Cursor : Cursor_intf.S_poly) :
  S_poly with type 'a container := 'a Cursor.container
          and type 'a cursor := 'a Cursor.t
          and type 'a elm := 'a Cursor.elm
