open Slice_intf

module Make_mono (Cursor : Cursor_intf.S_mono) : S_mono with type container := Cursor.container
                                                         and type cursor := Cursor.t
                                                         and type elm := Cursor.elm