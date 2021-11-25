open ContainerIntf

module MakePoly3Fold (T : IPoly3Fold) : SPoly3IterGen
  with type ('k, 'v, 'cmp) t := ('k, 'v, 'cmp) T.t
  with type 'k key := 'k T.key
  with type 'v value := 'v T.value = struct
  let fold_until = T.fold_until
  let fold_right_until = T.fold_right_until

  let foldi_until ~init ~f t =
    let _, accum = fold_until t ~init:(0L, init)
      ~f:(fun (i, accum) elm ->
        let i' = (Int64.succ i) in
        let accum', until = f i accum elm in
        (i', accum'), until
      ) in
    accum

  let fold ~init ~f t =
    fold_until t ~init ~f:(fun accum elm -> (f accum elm), false)

  let fold_right ~init ~f t =
    fold_right_until t ~init ~f:(fun elm accum -> (f elm accum), false)

  let foldi ~init ~f t =
    foldi_until t ~init ~f:(fun i accum elm -> (f i accum elm), false)

  let iter ~f t =
    fold t ~init:() ~f:(fun _ elm -> f elm)

  let iter_right ~f t =
    fold_right t ~init:() ~f:(fun elm _ -> f elm)

  let iteri ~f t =
    foldi t ~init:() ~f:(fun i _ elm -> f i elm)

  let count ~f t =
    fold t ~init:0L ~f:(fun accum elm ->
      match f elm with
      | false -> accum
      | true -> (Int64.succ accum)
    )

  let for_any ~f t =
    fold_until t ~init:false ~f:(fun _ elm ->
      let any' = f elm in
      any', any'
    )

  let for_all ~f t =
    fold_until t ~init:true ~f:(fun _ elm ->
      let all' = f elm in
      all', (not all')
    )

  let find ~f t =
    fold_until t ~init:None ~f:(fun _ elm ->
      match f elm with
      | false -> None, false
      | true -> Some elm, true
    )

  let find_map ~f t =
    fold_until t ~init:None ~f:(fun _ elm ->
      match f elm with
      | None -> None, false
      | Some a -> Some a, true
    )

  let findi ~f t =
    foldi_until t ~init:None ~f:(fun i _ elm ->
      match f i elm with
      | false -> None, false
      | true -> Some elm, true
    )

  let findi_map ~f t =
    foldi_until t ~init:None ~f:(fun i _ elm ->
      match f i elm with
      | None -> None, false
      | Some a -> Some a, true
    )

  let min_elm ~cmp t =
    fold t ~init:None ~f:(fun accum elm ->
      match accum with
      | None -> Some elm
      | Some e -> begin
          match cmp e elm with
          | Cmp.Lt -> Some e
          | Cmp.Eq
          | Cmp.Gt -> Some elm
        end
    )

  let max_elm ~cmp t =
    fold t ~init:None ~f:(fun accum elm ->
      match accum with
      | None -> Some elm
      | Some e -> begin
          match cmp e elm with
          | Cmp.Lt
          | Cmp.Eq -> Some elm
          | Cmp.Gt -> Some e
        end
    )

  let to_list t =
    fold_right t ~init:[] ~f:(fun elm accum -> elm :: accum)

  let to_list_rev t =
    fold t ~init:[] ~f:(fun accum elm -> elm :: accum)
end

module MakePoly3Array (T : IPoly3Index) : SPoly3ArrayGen
  with type ('k, 'v, 'cmp) t := ('k, 'v, 'cmp) T.t
  with type 'k key := 'k T.key
  with type 'v value := 'v T.value = struct

  let to_array t =
    match T.fold_until t ~init:None ~f:(fun _ elm0 -> Some elm0, true) with
    | None -> [||]
    | Some elm0 -> begin
        let l_int = Int64.to_int (T.length t) in
        let arr = Stdlib.Array.make l_int elm0 in
        let _ = T.fold_until t ~init:(arr, 0) ~f:(fun (arr, i) elm ->
          let () = Stdlib.Array.set arr i elm in
          (arr, (Stdlib.Int.succ i)), false
        ) in
        arr
      end
end

module MakePoly2Fold (T : IPoly2Fold) : SPoly2IterGen
  with type ('a, 'cmp) t := ('a, 'cmp) T.t
  with type 'a elm := 'a T.elm = struct
  let fold_until = T.fold_until
  let fold_right_until = T.fold_right_until

  let foldi_until ~init ~f t =
    let _, accum = fold_until t ~init:(0L, init)
      ~f:(fun (i, accum) elm ->
        let i' = (Int64.succ i) in
        let accum', until = f i accum elm in
        (i', accum'), until
      ) in
    accum

  let fold ~init ~f t =
    fold_until t ~init ~f:(fun accum elm -> (f accum elm), false)

  let fold_right ~init ~f t =
    fold_right_until t ~init ~f:(fun elm accum -> (f elm accum), false)

  let foldi ~init ~f t =
    foldi_until t ~init ~f:(fun i accum elm -> (f i accum elm), false)

  let iter ~f t =
    fold t ~init:() ~f:(fun _ elm -> f elm)

  let iter_right ~f t =
    fold_right t ~init:() ~f:(fun elm _ -> f elm)

  let iteri ~f t =
    foldi t ~init:() ~f:(fun i _ elm -> f i elm)

  let count ~f t =
    fold t ~init:0L ~f:(fun accum elm ->
      match f elm with
      | false -> accum
      | true -> (Int64.succ accum)
    )

  let for_any ~f t =
    fold_until t ~init:false ~f:(fun _ elm ->
      let any' = f elm in
      any', any'
    )

  let for_all ~f t =
    fold_until t ~init:true ~f:(fun _ elm ->
      let all' = f elm in
      all', (not all')
    )

  let find ~f t =
    fold_until t ~init:None ~f:(fun _ elm ->
      match f elm with
      | false -> None, false
      | true -> Some elm, true
    )

  let find_map ~f t =
    fold_until t ~init:None ~f:(fun _ elm ->
      match f elm with
      | None -> None, false
      | Some a -> Some a, true
    )

  let findi ~f t =
    foldi_until t ~init:None ~f:(fun i _ elm ->
      match f i elm with
      | false -> None, false
      | true -> Some elm, true
    )

  let findi_map ~f t =
    foldi_until t ~init:None ~f:(fun i _ elm ->
      match f i elm with
      | None -> None, false
      | Some a -> Some a, true
    )

  let min_elm ~cmp t =
    fold t ~init:None ~f:(fun accum elm ->
      match accum with
      | None -> Some elm
      | Some e -> begin
          match cmp e elm with
          | Cmp.Lt -> Some e
          | Cmp.Eq
          | Cmp.Gt -> Some elm
        end
    )

  let max_elm ~cmp t =
    fold t ~init:None ~f:(fun accum elm ->
      match accum with
      | None -> Some elm
      | Some e -> begin
          match cmp e elm with
          | Cmp.Lt
          | Cmp.Eq -> Some elm
          | Cmp.Gt -> Some e
        end
    )

  let to_list t =
    fold_right t ~init:[] ~f:(fun elm accum -> elm :: accum)

  let to_list_rev t =
    fold t ~init:[] ~f:(fun accum elm -> elm :: accum)
end

module MakePoly2Array (T : IPoly2Index) : SPoly2ArrayGen
  with type ('a, 'cmp) t := ('a, 'cmp) T.t
  with type 'a elm := 'a T.elm = struct

  let to_array t =
    match T.fold_until t ~init:None ~f:(fun _ elm0 -> Some elm0, true) with
    | None -> [||]
    | Some elm0 -> begin
        let l_int = Int64.to_int (T.length t) in
        let arr = Stdlib.Array.make l_int elm0 in
        let _ = T.fold_until t ~init:(arr, 0) ~f:(fun (arr, i) elm ->
          let () = Stdlib.Array.set arr i elm in
          (arr, (Stdlib.Int.succ i)), false
        ) in
        arr
      end
end

module MakePolyLength (T : IPolyIter) : SPolyLengthGen
  with type 'a t := 'a T.t
  with type 'a elm := 'a T.elm = struct
  let length t =
    let rec fn index cursor = begin
      match T.Cursor.(cursor = (tl t)) with
      | true -> index
      | false -> fn (Int64.succ index) (T.Cursor.succ cursor)
    end in
    fn 0L (T.Cursor.hd t)

  let is_empty t =
    (length t) = 0L
end

module MakeIPoly2Fold (T : IPolyFold) : IPoly2Fold
  with type ('a, 'cmp) t = 'a T.t
  with type 'a elm = 'a T.elm = struct
  type ('a, 'cmp) t = 'a T.t
  type 'a elm = 'a T.elm

  let fold_until = T.fold_until
  let fold_right_until = T.fold_right_until
end

module MakePolyFold (T : IPolyFold) : SPolyIterGen
  with type 'a t := 'a T.t
  with type 'a elm := 'a T.elm = struct
  include MakePoly2Fold(MakeIPoly2Fold(T))
end

module MakePolyIter (T : IPolyIter) : SPolyIterGen
  with type 'a t := 'a T.t
  with type 'a elm := 'a T.elm = struct
  module U = struct
    type 'a t = 'a T.t
    type 'a elm = 'a T.elm

    let fold_until ~init ~f t =
      let rec fn accum cursor tl = begin
        match T.Cursor.(=) cursor tl with
        | true -> accum
        | false -> begin
            let elm = T.Cursor.rget cursor in
            let (accum', until) = f accum elm in
            match until with
            | true -> accum'
            | false -> fn accum' (T.Cursor.succ cursor) tl
          end
      end in
      fn init (T.Cursor.hd t) (T.Cursor.tl t)

    let fold_right_until ~init ~f t =
      let rec fn t ~f accum hd cursor = begin
        match T.Cursor.(=) hd cursor with
        | true -> accum
        | false -> begin
            let elm = T.Cursor.lget cursor in
            let (accum', until) = f elm accum in
            match until with
            | true -> accum'
            | false -> fn t ~f accum' hd (T.Cursor.pred cursor)
          end
      end in
      fn t ~f init (T.Cursor.hd t) (T.Cursor.tl t)
  end
  include MakePolyFold(U)
end

module MakePolyArray (T : IPolyIndex) : SPolyArrayGen
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

module MakePolyMem (T : IPolyMem) : SPolyMemGen
  with type 'a t := 'a T.t
  with type 'a elm := 'a T.elm = struct
  let mem elm t =
    let rec fn cursor = begin
      match T.Cursor.(cursor = (tl t)) with
      | true -> false
      | false -> begin
          let e = T.Cursor.rget cursor in
          match T.cmp_elm e elm with
          | Eq -> true
          | Lt
          | Gt -> fn (T.Cursor.succ cursor)
        end
    end in
    fn (T.Cursor.hd t)
end

module MakeIPolyFold (T : IMonoFold) : IPolyFold
  with type 'a t = T.t
  with type 'a elm = T.elm = struct
  type 'a t = T.t
  type 'a elm = T.elm

  let fold_until = T.fold_until
  let fold_right_until = T.fold_right_until
end

module MakeIPolyIter (T : IMonoIter) : IPolyIter
  with type 'a t = T.t
  with type 'a elm = T.elm = struct
  type 'a t = T.t
  type 'a elm = T.elm

  module Cursor = struct
    module V = struct
      type 'a t = T.Cursor.t

      let cmp = T.Cursor.cmp
    end
    include V
    include Cmpable.MakePoly(V)

    let hd = T.Cursor.hd
    let tl = T.Cursor.tl
    let pred = T.Cursor.pred
    let succ = T.Cursor.succ
    let lget = T.Cursor.lget
    let rget = T.Cursor.rget
    let prev = T.Cursor.prev
    let next = T.Cursor.next
  end
end

module MakeIPolyArray (T : IMonoIndex) : IPolyIndex
  with type 'a t = T.t
  with type 'a elm = T.elm = struct
  include T
  include MakeIPolyIter(T)
end

module MakeMonoArray (T : IMonoIndex) : SMonoArray
  with type t := T.t
  with type elm := T.elm = struct
  include MakePolyArray(MakeIPolyArray(T))
end

module MakePolyIndex (T : IPolyIndex) : SPolyIndexGen
  with type 'a t := 'a T.t
  with type 'a elm := 'a T.elm = struct
  include T

  let is_empty t =
    Stdlib.((Int64.compare (T.length t) 0L) = 0)

  include MakePolyIter(T)
  include MakePolyArray(T)
end

module MakeIPolyIndex (T : IMonoIndex) : IPolyIndex
  with type 'a t = T.t
  with type 'a elm = T.elm = struct
  type 'a t = T.t
  type 'a elm = T.elm

  let length = T.length

  module Cursor = struct
    module V = struct
      type 'a t = T.Cursor.t

      let cmp = T.Cursor.cmp
    end
    include V
    include Cmpable.MakePoly(V)

    let hd = T.Cursor.hd
    let tl = T.Cursor.tl
    let pred = T.Cursor.pred
    let succ = T.Cursor.succ
    let lget = T.Cursor.lget
    let rget = T.Cursor.rget
    let prev = T.Cursor.prev
    let next = T.Cursor.next
  end
end

module MakeMonoLength (T : IMonoIter) : SMonoLength
  with type t := T.t
  with type elm := T.elm = struct
  include MakePolyLength(MakeIPolyIter(T))
end

module MakeMonoFold (T : IMonoFold) : SMonoIter
  with type t := T.t
  with type elm := T.elm = struct
  include MakePolyFold(MakeIPolyFold(T))
end

module MakeMonoIter (T : IMonoIter) : SMonoIter
  with type t := T.t
  with type elm := T.elm = struct
  include MakePolyIter(MakeIPolyIter(T))
end

module MakeIPolyMem (T : IMonoMem) : IPolyMem
  with type 'a t = T.t
  with type 'a elm = T.elm = struct
  include MakeIPolyIter(T)
  let cmp_elm = T.cmp_elm
end

module MakeMonoMem (T : IMonoMem) : SMonoMem
  with type t := T.t
  with type elm := T.elm = struct
  include MakePolyMem(MakeIPolyMem(T))
end

module MakeMonoIndex (T : IMonoIndex) : SMonoIndex
  with type t := T.t
  with type elm := T.elm
= struct
  include MakePolyIndex(MakeIPolyIndex(T))
end
