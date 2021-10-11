open ContainerArrayIntf

(* Polymorphic (poly[1]). *)

module MakePolyArray (T : IPolyArray) : SPolyArrayGen
  with type 'a t := 'a T.t
  with type 'a elm := 'a T.elm = struct

  let to_array t =
    let rec fn a cursor i l = begin
      match i = l with
      | true -> a
      | false -> begin
          let elm, cursor' = T.Cursor.next cursor in
          let () = Stdlib.Array.set a i elm in
          let i' = succ i in
          fn a cursor' i' l
        end
    end in
    match T.length t with
    | 0L -> [||]
    | l -> begin
        let cursor = T.Cursor.hd t in
        let elm0 = T.Cursor.rget cursor in
        let l_int = Int64.to_int l in
        let arr = Stdlib.Array.make l_int elm0 in
        fn arr (T.Cursor.succ cursor) 1 l_int
      end
end

(* Monomorphic. *)

module MakeIPolyArray (T : IMonoArray) : IPolyArray
  with type 'a t = T.t
  with type 'a elm = T.elm = struct
  include T
  include ContainerCommon.MakeIPolyCommon(T)
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

  let to_array t =
    let rec fn a cursor i l = begin
      match i = l with
      | true -> a
      | false -> begin
          let elm, cursor' = T.Cursor.next cursor in
          let () = Stdlib.Array.set a i elm in
          let i' = succ i in
          fn a cursor' i' l
        end
    end in
    match T.length t with
    | 0L -> [||]
    | l -> begin
        let cursor = T.Cursor.hd t in
        let elm0 = T.Cursor.rget cursor in
        let l_int = Int64.to_int l in
        let arr = Stdlib.Array.make l_int elm0 in
        fn arr (T.Cursor.succ cursor) 1 l_int
      end
end

(* Polymorphic (poly3). *)

module MakePoly3Array (T : IPoly3Array) : SPoly3ArrayGen
  with type ('k, 'v, 'cmp) t := ('k, 'v, 'cmp) T.t
  with type 'k key := 'k T.key
  with type 'v value := 'v T.value = struct

  let to_array t =
    let rec fn a cursor i l = begin
      match i = l with
      | true -> a
      | false -> begin
          let elm, cursor' = T.Cursor.next cursor in
          let () = Stdlib.Array.set a i elm in
          let i' = succ i in
          fn a cursor' i' l
        end
    end in
    match T.length t with
    | 0L -> [||]
    | l -> begin
        let cursor = T.Cursor.hd t in
        let elm0 = T.Cursor.rget cursor in
        let l_int = Int64.to_int l in
        let arr = Stdlib.Array.make l_int elm0 in
        fn arr (T.Cursor.succ cursor) 1 l_int
      end
end
