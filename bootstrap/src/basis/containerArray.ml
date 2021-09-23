open Rudiments0
open ContainerArrayIntf

(* Polymorphic (poly[1]). *)

module MakePolyArray (T : IPolyArray) : SPolyArrayGen
  with type 'a t := 'a T.t
  with type 'a elm := 'a T.elm = struct
  module ArraySeq = struct
    module T = struct
      type 'a t = {
        cursor: 'a T.Cursor.t;
        length: uns;
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
        assert (length t > 0L);
        let elm = T.Cursor.rget t.cursor in
        let cursor' = T.Cursor.succ t.cursor in
        let length' = (Uns.pred t.length) in
        let t' = {cursor=cursor'; length=length'} in
        elm, t'
    end
    include T
    include Array.Seq.MakePoly(T)
  end

  let to_array t =
    ArraySeq.to_array (ArraySeq.init t)
end

(* Monomorphic. *)

module MakeIPolyArray (T : IMonoArray) : IPolyArray
  with type 'a t = T.t
  with type 'a elm = T.elm = struct
  include T
  include ContainerCommon.MakeIPoly(T)
end

module MakeMonoArray (T : IMonoArray) : SMonoArray
  with type t := T.t
  with type elm := T.elm = struct
  include MakePolyArray(MakeIPolyArray(T))
end

(* Polymorphic (poly2). *)

module MakePoly2Array (T : IPoly2Array) : SPoly2ArrayGen
  with type ('a, 'cmp) t := ('a, 'cmp) T.t
  with type 'a elm := 'a T.elm = struct
  module ArraySeq = struct
    module T = struct
      type ('a, 'cmp) t = {
        cursor: ('a, 'cmp) T.Cursor.t;
        length: uns;
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
        assert (length t > 0L);
        let elm = T.Cursor.rget t.cursor in
        let cursor' = T.Cursor.succ t.cursor in
        let length' = (Uns.pred t.length) in
        let t' = {cursor=cursor'; length=length'} in
        elm, t'
    end
    include T
    include Array.Seq.MakePoly2(T)
  end

  let to_array t =
    ArraySeq.to_array (ArraySeq.init t)
end

(* Polymorphic (poly3). *)

module MakePoly3Array (T : IPoly3Array) : SPoly3ArrayGen
  with type ('k, 'v, 'cmp) t := ('k, 'v, 'cmp) T.t
  with type 'k key := 'k T.key
  with type 'v value := 'v T.value = struct
  module ArraySeq = struct
    module T = struct
      type ('k, 'v, 'cmp) t = {
        cursor: ('k, 'v, 'cmp) T.Cursor.t;
        length: uns;
      }
      type 'k key= 'k T.key
      type 'v value = 'v T.value

      let init container =
        {
          cursor=(T.Cursor.hd container);
          length=(T.length container);
        }

      let length t =
        t.length

      let next t =
        assert (length t > 0L);
        let (k, v) = T.Cursor.rget t.cursor in
        let cursor' = T.Cursor.succ t.cursor in
        let length' = (Uns.pred t.length) in
        let t' = {cursor=cursor'; length=length'} in
        (k, v), t'
    end
    include T
    include Array.Seq.MakePoly3(T)
  end

  let to_array t =
    ArraySeq.to_array (ArraySeq.init t)
end
