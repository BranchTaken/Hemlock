open Container_array_intf
(* Partial Rudiments. *)
module Uint = U63
type uint = Uint.t

(* Polymorphic. *)

module Make_poly_array (T : I_poly_array) : S_poly_array_gen
  with type 'a t := 'a T.t
   and type 'a elm := 'a T.elm = struct
  module Array_seq = struct
    module T = struct
      type 'a t = {
        cursor: 'a T.Cursor.t;
        length: uint;
      }
      type 'a elm = 'a T.elm

      let init container =
        {
          cursor=(T.Cursor.hd container);
          length=(T.length container);
        }

      let length t =
        t.length

      let next t =
        assert (Uint.(length t > 0));
        let elm = T.Cursor.rget t.cursor in
        let cursor' = T.Cursor.succ t.cursor in
        let length' = pred t.length in
        let t' = {cursor=cursor'; length=length'} in
        elm, t'
    end
    include T
    include Array.Seq.Make_poly(T)
  end

  let to_array t =
    Array_seq.to_array (Array_seq.init t)
end

(* Monomorphic. *)

module Make_i_poly_array (T : I_mono_array) : I_poly_array
  with type 'a t = T.t
   and type 'a elm = T.elm = struct
  include Container_common.Make_i_poly(T)
  let length = T.length
end

module Make_mono_array (T : I_mono_array) : S_mono_array
  with type t := T.t
   and type elm := T.elm = struct
  include Make_poly_array(Make_i_poly_array(T))
end
