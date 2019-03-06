open Container_intf

(* Polymorphic. *)

module Make_poly_length (T : I_poly) : S_poly_length
  with type 'a t := 'a T.t
   and type 'a elm := 'a T.elm =
struct
  let length t =
    let rec fn index cursor = begin
      match T.Cursor.(cursor = (tl t)) with
      | true -> index
      | false -> fn (succ index) (T.Cursor.succ cursor)
    end in
    fn 0 (T.Cursor.hd t)

  let is_empty t =
    Int.((length t) = 0)
end

module Make_poly_fold (T : I_poly) : S_poly_fold with type 'a t := 'a T.t
                                                  and type 'a elm := 'a T.elm =
struct
  let fold_until t ~init ~f =
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

  let fold_right_until t ~init ~f =
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

  let foldi_until t ~init ~f =
    let _, accum = fold_until t ~init:(0, init) ~f:(fun (i, accum) elm ->
      let i' = succ i in
      let accum', until = f i accum elm in
      (i', accum'), until
    ) in
    accum

  let fold t ~init ~f =
    fold_until t ~init ~f:(fun accum elm -> (f accum elm), false)

  let fold_right t ~init ~f =
    fold_right_until t ~init ~f:(fun elm accum -> (f elm accum), false)

  let foldi t ~init ~f =
    foldi_until t ~init ~f:(fun i accum elm -> (f i accum elm), false)

  let iter t ~f =
    fold t ~init:() ~f:(fun _ elm -> f elm)

  let iteri t ~f =
    foldi t ~init:() ~f:(fun i _ elm -> f i elm)

  let count t ~f =
    fold t ~init:0 ~f:(fun accum elm ->
      match f elm with
        | false -> accum
        | true -> succ accum
    )

  let for_any t ~f =
    fold_until t ~init:false ~f:(fun _ elm ->
      let any' = f elm in
      any', any'
    )

  let for_all t ~f =
    fold_until t ~init:true ~f:(fun _ elm ->
      let all' = f elm in
      all', (not all')
    )

  let find t ~f =
    fold_until t ~init:None ~f:(fun accum elm ->
      match f elm with
      | false -> accum, false
      | true -> Some elm, true
    )

  let find_map t ~f =
    fold_until t ~init:None ~f:(fun accum elm ->
      match f elm with
      | None -> accum, false
      | Some a -> Some a, true
    )

  let min_elm t ~cmp =
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

  let max_elm t ~cmp =
    fold t ~init:None ~f:(fun accum elm ->
      match accum with
      | None -> Some elm
      | Some e -> begin
          match cmp e elm with
          | Cmp.Lt -> Some elm
          | Cmp.Eq
          | Cmp.Gt -> Some e
        end
    )

  let to_list t =
    fold_right t ~init:[] ~f:(fun elm accum -> elm :: accum)

  let to_list_rev t =
    fold t ~init:[] ~f:(fun accum elm -> elm :: accum)
end

module Make_poly_mem (T : I_poly_mem) : S_poly_mem with type 'a t := 'a T.t
                                                    and type 'a elm := 'a T.elm
= struct
  let mem t elm =
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

module Make_poly_array (T : I_poly_array) : S_poly_array
  with type 'a t := 'a T.t
   and type 'a elm := 'a T.elm = struct
  module Array_seq = struct
    module T = struct
      type 'a t = {
        cursor: 'a T.Cursor.t;
        length: int;
      }
      type 'a elm = 'a T.elm
      (*
        {
        elm: 'a
      }
         *)
      (*
      type 'a elm = 'a
         *)
      (*
      type 'a outer_elm = 'a T.elm
      type 'a elm = 'a outer_elm
         *)
        (*
      type nonrec 'a elm = 'a elm
           *)

      let init container =
        {
          cursor=(T.Cursor.hd container);
          length=(T.length container);
        }

      let length t =
        t.length

      let next t =
        assert (Int.(length t > 0));
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

module Make_i_poly (T : I_mono) : I_poly with type 'a t = T.t
                                          and type 'a elm = T.elm
= struct
  type 'a t = T.t
  type 'a elm = T.elm

  module Cursor = struct
    module V = struct
      type 'a t = T.Cursor.t
      type 'a elm = T.elm

      let cmp = T.Cursor.cmp
    end
    include V
    include Cmpable.Make_poly(V)

    let hd = T.Cursor.hd
    let tl = T.Cursor.tl
    let succ = T.Cursor.succ
    let pred = T.Cursor.pred
    let lget = T.Cursor.lget
    let rget = T.Cursor.rget
  end
end

module Make_mono_length (T : I_mono) : S_mono_length with type t := T.t
                                                      and type elm := T.elm =
struct
  include Make_poly_length(Make_i_poly(T))
end

module Make_mono_fold (T : I_mono) : S_mono_fold with type t := T.t
                                                  and type elm := T.elm = struct
  include Make_poly_fold(Make_i_poly(T))
end

module Make_i_poly_mem (T : I_mono_mem) : I_poly_mem with type 'a t = T.t
                                                      and type 'a elm = T.elm
= struct
  include Make_i_poly(T)
  let cmp_elm = T.cmp_elm
end

module Make_mono_mem (T : I_mono_mem) : S_mono_mem with type t := T.t
                                                    and type elm := T.elm =
struct
  include Make_poly_mem(Make_i_poly_mem(T))
end

module Make_i_poly_array (T : I_mono_array) : I_poly_array
  with type 'a t = T.t
   and type 'a elm = T.elm = struct
  include Make_i_poly(T)
  let length = T.length
end

module Make_mono_array (T : I_mono_array) : S_mono_array
  with type t := T.t
   and type elm := T.elm = struct
  include Make_poly_array(Make_i_poly_array(T))
end
