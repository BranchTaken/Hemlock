open Rudiments0
open ContainerCommonIntf

(* Polymorphic. *)

(* poly[1]. *)

module MakePolyLength (T : IPoly) : SPolyLengthGen
  with type 'a t := 'a T.t
  with type 'a elm := 'a T.elm = struct
  let length t =
    let rec fn index cursor = begin
      match T.Cursor.(cursor = (tl t)) with
      | true -> index
      | false -> fn (Uns.succ index) (T.Cursor.succ cursor)
    end in
    fn 0 (T.Cursor.hd t)

  let is_empty t =
    (length t) = 0
end

module MakePolyFold (T : IPoly) : SPolyFoldGen
  with type 'a t := 'a T.t
  with type 'a elm := 'a T.elm = struct
  let fold_until ~init ~f t =
    let rec fn accum cursor = begin
      match T.Cursor.(cursor = (tl t)) with
      | true -> accum
      | false -> begin
          let elm = T.Cursor.rget cursor in
          let (accum', until) = f accum elm in
          match until with
          | true -> accum'
          | false -> fn accum' (T.Cursor.succ cursor)
        end
    end in
    fn init (T.Cursor.hd t)

  let fold_right_until ~init ~f t =
    let rec fn t ~f accum cursor = begin
      match T.Cursor.(cursor = (T.Cursor.hd t)) with
      | true -> accum
      | false -> begin
          let elm = T.Cursor.lget cursor in
          let (accum', until) = f elm accum in
          match until with
          | true -> accum'
          | false -> fn t ~f accum' (T.Cursor.pred cursor)
        end
    end in
    fn t ~f init (T.Cursor.tl t)

  let foldi_until ~init ~f t =
    let _, accum = fold_until t ~init:(0, init)
      ~f:(fun (i, accum) elm ->
        let i' = (Uns.succ i) in
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

  let iteri ~f t =
    foldi t ~init:() ~f:(fun i _ elm -> f i elm)

  let count ~f t =
    fold t ~init:0 ~f:(fun accum elm ->
      match f elm with
      | false -> accum
      | true -> (Uns.succ accum)
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

module MakeIPoly (T : IMono) : IPoly
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
    let succ = T.Cursor.succ
    let pred = T.Cursor.pred
    let lget = T.Cursor.lget
    let rget = T.Cursor.rget
    let prev = T.Cursor.prev
    let next = T.Cursor.next
  end
end

(* Monomorphic. *)

module MakeMonoLength (T : IMono) : SMonoLength
  with type t := T.t
  with type elm := T.elm = struct
  include MakePolyLength(MakeIPoly(T))
end

module MakeMonoFold (T : IMono) : SMonoFold
  with type t := T.t
  with type elm := T.elm = struct
  include MakePolyFold(MakeIPoly(T))
end

module MakeIPolyMem (T : IMonoMem) : IPolyMem
  with type 'a t = T.t
  with type 'a elm = T.elm = struct
  include MakeIPoly(T)
  let cmp_elm = T.cmp_elm
end

module MakeMonoMem (T : IMonoMem) : SMonoMem
  with type t := T.t
  with type elm := T.elm = struct
  include MakePolyMem(MakeIPolyMem(T))
end
