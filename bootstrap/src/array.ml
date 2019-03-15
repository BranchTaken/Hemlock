open Rudiments

module T = struct
  type 'a t = 'a array
  type 'a elm = 'a

  let get t i =
    Stdlib.Array.get t i

  let length t =
    Stdlib.Array.length t

  module Cursor = struct
    module T = struct
      type 'a container = 'a t
      type 'a t = {
        array: 'a container;
        index: uint;
      }

      let cmp t0 t1 =
        (* == is excessively vague in OCaml. *)
        assert ((t0.array == t1.array)
                || (Stdlib.Pervasives.( = ) t0.array t1.array));
        Uint.cmp t0.index t1.index

      let hd array =
        {array; index=0}

      let tl array =
        {array; index=(length array)}

      let succ t =
        {t with index=(succ t.index)}

      let pred t =
        {t with index=(pred t.index)}

      let lget t =
        get t.array Uint.(pred t.index)

      let rget t =
        get t.array t.index
    end
    include T
    include Cmpable.Make_poly(T)
  end

  let cmp cmp_elm t0 t1 =
    let rec fn cursor0 cursor1 = begin
      match Cursor.(cursor0 = (tl t0)), Cursor.(cursor1 = (tl t1)) with
      | true, true -> Cmp.Eq
      | true, false -> Cmp.Lt
      | false, true -> Cmp.Gt
      | false, false -> begin
          match cmp_elm (Cursor.rget cursor0) (Cursor.rget cursor1) with
          | Cmp.Lt -> Cmp.Lt
          | Cmp.Eq -> fn (Cursor.succ cursor0) (Cursor.succ cursor1)
          | Cmp.Gt -> Cmp.Gt
        end
    end in
    fn (Cursor.hd t0) (Cursor.hd t1)
end
include T
include Container_common.Make_poly_fold(T)

module Seq = struct
  type 'a outer = 'a t
  module type S_mono = sig
    type t
    type elm
    val to_array: t -> elm outer
  end
  module type S_poly = sig
    type 'a t
    type 'a elm
    val to_array: 'a t -> 'a elm outer
  end

  module Make_mono (T : Seq_intf.I_mono_def) : S_mono with type t := T.t
                                                       and type elm := T.elm =
  struct
    let to_array t =
      let l = T.length t in
      match l with
      | 0 -> [||]
      | _ -> begin
          let rec fn t a i = begin
            match Uint.(i = l) with
            | true -> a
            | false -> begin
                let elm, t' = T.next t in
                let () = Stdlib.Array.set a i elm in
                let i' = succ i in
                fn t' a i'
              end
          end in
          let elm0, t' = T.next t in
          let a = Stdlib.Array.make l elm0 in
          fn t' a 1
        end
  end

  module Make_mono_rev (T : Seq_intf.I_mono_def) : S_mono with type t := T.t
                                                           and type elm := T.elm
  = struct
    let to_array t =
      let l = T.length t in
      match l with
      | 0 -> [||]
      | _ -> begin
          let rec fn t a i = begin
            match Uint.(i = 0) with
            | true -> a
            | false -> begin
                let elm, t' = T.next t in
                let i' = pred i in
                let () = Stdlib.Array.set a i' elm in
                fn t' a i'
              end
          end in
          let elm, t' = T.next t in
          let a = Stdlib.Array.make l elm in
          fn t' a (pred l)
        end
  end

  module Make_poly (T : Seq_intf.I_poly_def) : S_poly
    with type 'a t := 'a T.t
     and type 'a elm := 'a T.elm = struct
    let to_array t =
      let l = T.length t in
      match l with
      | 0 -> [||]
      | _ -> begin
          let rec fn t a i = begin
            match Uint.(i = l) with
            | true -> a
            | false -> begin
                let elm, t' = T.next t in
                let () = Stdlib.Array.set a i elm in
                let i' = succ i in
                fn t' a i'
              end
          end in
          let elm0, t' = T.next t in
          let a = Stdlib.Array.make l elm0 in
          fn t' a 1
        end
  end

  module Make_poly_rev (T : Seq_intf.I_poly_def) : S_poly
    with type 'a t := 'a T.t
     and type 'a elm := 'a T.elm = struct
    let to_array t =
      let l = T.length t in
      match l with
      | 0 -> [||]
      | _ -> begin
          let rec fn t a i = begin
            match Uint.(i = 0) with
            | true -> a
            | false -> begin
                let elm, t' = T.next t in
                let i' = pred i in
                let () = Stdlib.Array.set a i' elm in
                fn t' a i'
              end
          end in
          let elm, t' = T.next t in
          let a = Stdlib.Array.make l elm in
          fn t' a (pred l)
        end
  end

  (* Special-purpose, for fold[i]_map . *)
  module Make_poly2 (T : sig
      type ('a, 'accum, 'b) t
      val length: ('a,'accum,'b) t -> uint
      val next: ('a,'accum,'b) t -> 'accum  -> 'b  * ('a,'accum,'b) t * 'accum
    end) : sig
      type ('a, 'accum, 'b) t
      val to_accum_array: ('a,'accum,'b) t -> init:'accum -> 'accum * 'b outer
    end with type ('a,'accum,'b) t := ('a,'accum,'b) T.t = struct
    let to_accum_array t ~init =
      let l = T.length t in
      match l with
      | 0 -> init, [||]
      | _ -> begin
          let rec fn t accum a i = begin
            match Uint.(i = l) with
            | true -> accum, a
            | false -> begin
                let elm, t', accum' = T.next t accum in
                let () = Stdlib.Array.set a i elm in
                let i' = succ i in
                fn t' accum' a i'
              end
          end in
          let elm0, t', accum = T.next t init in
          let a = Stdlib.Array.make l elm0 in
          fn t' accum a 1
        end
  end

  (* Special-purpose, for fold[i]2_map . *)
  module Make_poly3 (T : sig
      type ('a, 'b, 'accum, 'c) t
      val length: ('a,'b,'accum,'c) t -> uint
      val next: ('a,'b,'accum,'c) t -> 'accum
        -> 'c  * ('a,'b,'accum,'c) t * 'accum
    end) : sig
      type ('a, 'b, 'accum, 'c) t
      val to_accum_array: ('a,'b,'accum,'c) t -> init:'accum
        -> 'accum * 'c outer
    end with type ('a,'b,'accum,'c) t := ('a,'b,'accum,'c) T.t = struct
    let to_accum_array t ~init =
      let l = T.length t in
      match l with
      | 0 -> init, [||]
      | _ -> begin
          let rec fn t accum a i = begin
            match Uint.(i = l) with
            | true -> accum, a
            | false -> begin
                let elm, t', accum' = T.next t accum in
                let () = Stdlib.Array.set a i elm in
                let i' = succ i in
                fn t' accum' a i'
              end
          end in
          let elm0, t', accum = T.next t init in
          let a = Stdlib.Array.make l elm0 in
          fn t' accum a 1
        end
  end
end

module Array_init = struct
  module T = struct
    type 'a t = {
      f: uint -> 'a;
      index: uint;
      length: uint;
    }
    type 'a elm = 'a

    let init length ~f =
      {f; index=0; length}

    let length t =
      t.length

    let next t =
      let elm = t.f t.index in
      let t' = {t with index=Uint.(succ t.index)} in
      elm, t'
  end
  include T
  include Seq.Make_poly(T)
end

let init n ~f =
  Array_init.(to_array (init n ~f))

module Array_of_list_common = struct
  type 'a t = {
    list: 'a list;
    length: uint;
  }
  type 'a elm = 'a

  let init length list =
    {list; length}

  let length t =
    t.length

  let next t =
    match t.list with
    | [] -> not_reached ()
    | elm :: list' -> begin
        let t' = {t with list=list'} in
        elm, t'
      end
end

module Array_of_list = struct
  include Array_of_list_common
  include Seq.Make_poly(Array_of_list_common)
end

module Array_of_list_rev = struct
  include Array_of_list_common
  include Seq.Make_poly_rev(Array_of_list_common)
end

let of_list ?length list =
  let length = match length with
    | None -> List.length list
    | Some length -> length
  in
  Array_of_list.to_array (Array_of_list.init length list)

let of_list_rev ?length list =
  let length = match length with
    | None -> List.length list
    | Some length -> length
  in
  Array_of_list_rev.to_array (Array_of_list_rev.init length list)

let is_empty t =
  Uint.((length t) = 0)

let set_inplace t i elm =
  Stdlib.Array.set t i elm

let copy t =
  init (length t) ~f:(fun i -> get t i)

let pare t ~base ~past =
  assert Uint.(base <= past);
  init Uint.(past - base) ~f:(fun i -> get t Uint.(base + i))

let set t i elm =
  let t' = copy t in
  set_inplace t' i elm;
  t'

module Array_concat = struct
  module T = struct
    type 'a outer = 'a t
    type 'a t = {
      arrays: 'a outer list;
      length: uint;
      array: 'a outer;
      index: uint;
    }
    type 'a elm = 'a

    let init length arrays =
      {arrays; length; array=[||]; index=0}

    let next t =
      let rec fn t = begin
        match Uint.( < ) t.index (length t.array) with
        | false -> begin
            match t.arrays with
            | [] -> not_reached ()
            | array' :: arrays' ->
              fn {t with arrays=arrays'; array=array'; index=0}
          end
        | true -> begin
            let elm = get t.array t.index in
            let t' = {t with index=Uint.(succ t.index)} in
            elm, t'
          end
      end in
      fn t

    let length t =
      t.length
  end
  include T
  include Seq.Make_poly(T)
end

let concat_list tlist =
  let tlist_length = List.fold_left (fun accum list ->
    Uint.( + ) accum (length list)
  ) 0 tlist in
  Array_concat.(to_array (init tlist_length tlist))

let concat t0 t1 =
  let length_t0 = length t0 in
  let length_t1 = length t1 in
  let length = Uint.(length_t0 + length_t1) in
  init length ~f:(fun i ->
    if i < length_t0 then get t0 i
    else get t1 Uint.(i - length_t0)
  )

let append t elm =
  let tlen = length t in
  init Uint.(succ tlen) ~f:(fun i ->
    if Uint.(i < tlen) then get t i
    else elm
  )

let prepend elm t =
  init Uint.(succ (length t)) ~f:(fun i ->
    if Uint.(i = 0) then elm
    else get t Uint.(pred i)
  )

let insert t i elm =
  if Uint.(i = 0) then
    prepend elm t
  else begin
    let len = length t in
    if Uint.(i < len) then
      init Uint.(succ len) ~f:(fun index ->
        match Uint.cmp index i with
        | Lt -> get t index
        | Eq -> elm
        | Gt -> get t Uint.(pred index)
      )
    else
      append t elm
  end

let remove t i =
  let len = length t in
  assert Uint.(len > 0);
  match len with
  | 1 ->
    assert Uint.(i = 0);
    [||]
  | _ -> begin
      if Uint.(i = 0) then
        pare t ~base:1 ~past:len
      else if Uint.(i < len) then
        init Uint.(pred len) ~f:(fun index ->
          if Uint.(index < i) then get t index
          else get t Uint.(succ index)
        )
      else
        pare t ~base:0 ~past:(pred len)
    end

let reduce t ~f =
  let rec fn i accum = begin
    match Uint.(i = (length t)) with
    | true -> accum
    | false -> fn (succ i) (f accum (get t i))
  end in
  match length t with
  | 0 -> None
  | _ -> Some (fn 1 (get t 0))

let reduce_hlt t ~f =
  match reduce t ~f with
  | None -> halt "Empty array"
  | Some x -> x

let swap_inplace t i0 i1 =
  let elm0 = get t i0 in
  let elm1 = get t i1 in
  set_inplace t i0 elm1;
  set_inplace t i1 elm0

let swap t i0 i1 =
  let t' = copy t in
  swap_inplace t' i0 i1;
  t'

let rev_inplace t =
  let rec fn i0 i1 = begin
    match Uint.(i0 >= i1) with
    | true -> ()
    | false -> begin
        swap_inplace t i0 i1;
        fn (Uint.succ i0) (Uint.pred i1)
      end
  end in
  let len = length t in
  match len with
  | 0 -> ()
  | _ -> fn 0 Uint.(pred len)

let rev t =
  let t' = copy t in
  rev_inplace t';
  t'

(* Used directly for non-overlapping blits. *)
let blit_ascending t0 i0 t1 i1 len =
  for i = 0 to (Uint.pred len) do
    set_inplace t1 Uint.(i1 + i) (get t0 Uint.(i0 + i))
  done
let blit_descending t0 i0 t1 i1 len =
  for i = (Uint.pred len) downto 0 do
    set_inplace t1 Uint.(i1 + i) (get t0 Uint.(i0 + i))
  done

let blit t0 i0 t1 i1 len =
  (* Choose direction such that overlapping ranges don't corrupt the source. *)
  match Uint.(i0 < i1) with
  | true -> blit_descending t0 i0 t1 i1 len
  | false -> blit_ascending t0 i0 t1 i1 len

let is_sorted ?(strict=false) t ~cmp =
  let len = length t in
  let rec fn elm i = begin
    match Uint.(i = len) with
    | true -> true
    | false -> begin
        let elm' = get t i in
        match (cmp elm elm'), strict with
        | Cmp.Lt, _
        | Cmp.Eq, false -> fn elm' (Uint.succ i)
        | Cmp.Eq, true
        | Cmp.Gt, _ -> false
      end
  end in
  match len with
  | 0 -> true
  | _ -> fn (get t 0) 1

type order =
| Increasing
| Decreasing
| Either
type run = {
  base: uint;
  past: uint;
}
let sort_impl ?(stable=false) t ~cmp ~inplace =
  (* Merge a pair of adjacent runs.  Input runs may be in increasing or
   * decreasing order; the output is always in increasing order. *)
  let merge_pair ~cmp src run0 order0 run1 order1 dst = begin
    assert Uint.(run0.past = run1.base);
    let rblit t0 i0 t1 i1 len = begin
      for i = 0 to (Uint.pred len) do
        set_inplace t1 Uint.(i1 + i) (get t0 Uint.(i0 + len - (i + 1)))
      done
    end in
    let merge_pair_oo ~cmp src run0 order0 run1 order1 dst = begin
      let rec fn ~cmp src run0 order0 run1 order1 dst run = begin
        match Uint.(run0.base = run0.past), Uint.(run1.base = run1.past) with
        | false, false -> begin
            let elm0 = match order0 with
              | Increasing -> get src run0.base
              | Decreasing -> get src (Uint.pred run0.past)
              | Either -> not_reached ()
            in
            let elm1 = match order1 with
              | Increasing -> get src run1.base
              | Decreasing -> get src (Uint.pred run1.past)
              | Either -> not_reached ()
            in
            match cmp elm0 elm1 with
            | Cmp.Lt
            | Cmp.Eq -> begin
                match order0 with
                | Increasing -> begin
                    set_inplace dst run.past elm0;
                    let run0' = {run0 with base=Uint.succ run0.base} in
                    let run' = {run with past=Uint.succ run.past} in
                    fn ~cmp src run0' order0 run1 order1 dst run'
                  end
                | Decreasing -> begin
                    set_inplace dst run.past elm0;
                    let run0' = {run0 with past=Uint.pred run0.past} in
                    let run' = {run with past=Uint.succ run.past} in
                    fn ~cmp src run0' order0 run1 order1 dst run'
                  end
                | Either -> not_reached ()
              end
            | Cmp.Gt -> begin
                match order1 with
                | Increasing -> begin
                    set_inplace dst run.past elm1;
                    let run1' = {run1 with base=Uint.succ run1.base} in
                    let run' = {run with past=Uint.succ run.past} in
                    fn ~cmp src run0 order0 run1' order1 dst run'
                  end
                | Decreasing -> begin
                    set_inplace dst run.past elm1;
                    let run1' = {run1 with past=Uint.pred run1.past} in
                    let run' = {run with past=Uint.succ run.past} in
                    fn ~cmp src run0 order0 run1' order1 dst run'
                  end
                | Either -> not_reached ()
              end
          end
        | true, false -> begin
            let len = Uint.(run1.past - run1.base) in
            let () = match order1 with
              | Increasing -> blit_ascending src run1.base dst run.past len;
              | Decreasing -> rblit src run1.base dst run.past len;
              | Either -> not_reached ()
            in
            {run with past=Uint.(run.past + len)}
          end
        | false, true -> begin
            let len = Uint.(run0.past - run0.base) in
            let () = match order0 with
              | Increasing -> blit_ascending src run0.base dst run.past len;
              | Decreasing -> rblit src run0.base dst run.past len;
              | Either -> not_reached ()
            in
            {run with past=Uint.(run.past + len)}
          end
        | true, true -> not_reached ()
      end in
      fn ~cmp src run0 order0 run1 order1 dst {base=run0.base; past=run0.base}
    end in
    let order0', order1' = match order0, order1 with
      | Increasing, Increasing
      | Increasing, Either
      | Either, Increasing
      | Either, Either -> Increasing, Increasing
      | Increasing, Decreasing -> Increasing, Decreasing
      | Decreasing, Increasing -> Decreasing, Increasing
      | Decreasing, Decreasing
      | Decreasing, Either
      | Either, Decreasing -> Decreasing, Decreasing
    in
    merge_pair_oo ~cmp src run0 order0' run1 order1' dst
  end in
  (* Select monotonic runs and merge run pairs. *)
  let rec select ~stable ~cmp elm base i order run0_opt order0 src dst runs =
    begin
    match Uint.(i = (length src)) with
    | true -> begin
        let run0, order0, run1, order1 = match run0_opt with
          | None -> begin
              (* Copy odd run. *)
              let run0 = {base; past=i} in
              let run1 = {base=i; past=i} in
              run0, order, run1, Increasing
            end
          | Some run0 -> run0, order0, {base; past=i}, order
        in
        let run = merge_pair ~cmp src run0 order0 run1 order1 dst in
        run :: runs
      end
    | false -> begin
        let elm' = get src i in
        let i' = Uint.succ i in
        match cmp elm elm', order, stable with
        | Cmp.Lt, Either, _ ->
          select ~stable ~cmp elm' base i' Increasing run0_opt order0 src dst
            runs
        | Cmp.Gt, Either, _ ->
          select ~stable ~cmp elm' base i' Decreasing run0_opt order0 src dst
            runs
        | Cmp.Lt, Increasing, _
        | Cmp.Eq, Increasing, true
        | Cmp.Eq, Either, true ->
          select ~stable ~cmp elm' base i' Increasing run0_opt order0 src dst
            runs
        | Cmp.Eq, _, false
        | Cmp.Gt, Decreasing, _ ->
          select ~stable ~cmp elm' base i' order run0_opt order0 src dst runs
        | Cmp.Lt, Decreasing, _
        | Cmp.Eq, Decreasing, true
        | Cmp.Gt, Increasing, _ -> begin
            let run0_opt', order0', runs' = match run0_opt with
              | None -> Some {base; past=i}, order, runs
              | Some run0 -> begin
                  let run1 = {base; past=i} in
                  let run =
                    merge_pair ~cmp src run0 order0 run1 order dst in
                  None, Either, (run :: runs)
                end
            in
            select ~stable ~cmp elm' i i' Either run0_opt' order0' src dst runs'
          end
      end
  end in
  let aux = copy t in
  let runs = match length t with
    | 0 -> [{base=0; past=0}]
    | _ -> select ~stable ~cmp (get t 0) 0 1 Either None Either t aux []
  in

  (* Repeatedly sweep through runs and merge pairs until only one run remains.
   * Take care to read/write linearly up/down based on the sweep direction, in
   * order to improve cache locality. *)
  let rec merge ~cmp ~inplace src to_merge up dst merged = begin
    match to_merge, up, merged with
    | run0 :: run1 :: to_merge', true, _
    | run1 :: run0 :: to_merge', false, _ -> begin
        let run = merge_pair ~cmp src run0 Increasing run1 Increasing dst in
        merge ~cmp ~inplace src to_merge' up dst (run :: merged)
      end
    | [], _, _ :: _ :: _ -> begin
        (* Start another sweep. *)
        merge ~cmp ~inplace dst merged (not up) src []
      end
    | run :: [], false, _ -> begin
        (* Copy odd run to dst, then start another sweep. *)
        blit_descending src run.base dst run.base Uint.(run.past - run.base);
        merge ~cmp ~inplace dst (run :: merged) (not up) src []
      end
    | run :: [], true, _ :: _ -> begin
        (* Copy odd run to dst, then start another sweep. *)
        blit_ascending src run.base dst run.base Uint.(run.past - run.base);
        merge ~cmp ~inplace dst (run :: merged) (not up) src []
      end
    | [], true, _ :: []
    | _ :: [], true, [] -> begin
        match inplace with
        | true -> begin
            (* Odd number of sweeps performed; copy result back into t. *)
            blit_ascending dst 0 src 0 (length dst);
            src
          end
        | false -> dst
      end
    | [], false, _ :: [] -> dst
    | [], _, [] -> not_reached ()
  end in
  merge ~cmp ~inplace aux runs false t []

let sort_inplace ?stable t ~cmp =
  let _ = sort_impl ?stable t ~cmp ~inplace:true in
  ()

let sort ?stable t ~cmp =
  sort_impl ?stable (copy t) ~cmp ~inplace:false

let search_impl ?base ?past t key ~cmp mode =
  let base = match base with
    | None -> 0
    | Some base -> base
  in
  let past = match past with
    | None -> length t
    | Some past -> past
  in
  assert Uint.(base <= past);
  assert Uint.(past <= length t);
  let rec fn ~base ~past = begin
    assert Uint.(base <= past);
    match Uint.(base = past) with
    | true -> begin
        (* Empty range. *)
        match mode with
        | Cmp.Lt -> begin
            match Uint.(base = 0), Uint.(base = (length t)) with
            | true, true -> None (* Empty; key < elms. *)
            | _, false -> begin (* At beginning, or interior. *)
                match cmp key (get t base) with
                | Cmp.Lt -> begin (* At successor. *)
                    match Uint.(base = 0) with
                    | true -> Some (Cmp.Lt, 0) (* At beginning; key < elms. *)
                    | false ->
                      Some (Cmp.Gt, (Uint.pred base)) (* In interior;
                                                         key > predecessor. *)
                  end
                | Cmp.Eq -> Some (Cmp.Eq, base) (* base at leftmost match. *)
                | Cmp.Gt -> not_reached ()
              end
            | false, true ->
              Some (Cmp.Gt, (Uint.pred base)) (* Past end; key > elms. *)
          end
        | Cmp.Eq -> None (* No match. *)
        | Cmp.Gt -> begin
            match Uint.(base = 0), Uint.(base = (length t)) with
            | true, true -> None (* Empty; key > elms. *)
            | true, false -> Some (Cmp.Lt, 0) (* At beginning; key < elms. *)
            | false, _ -> begin (* In interior, or past end. *)
                let probe = Uint.pred base in
                match cmp key (get t probe) with
                | Cmp.Lt -> not_reached ()
                | Cmp.Eq -> Some (Cmp.Eq, probe) (* probe at rightmost match. *)
                | Cmp.Gt -> begin
                    match Uint.(base = (length t)) with
                    | false -> Some (Cmp.Lt, base) (* In interior;
                                                      key < successor. *)
                    | true -> Some (Cmp.Gt, probe) (* Past end; key > elms. *)
                  end
              end
          end
      end
    | false -> begin
        let mid = Uint.((base + past) / 2) in
        match (cmp key (get t mid)), mode with
        | Cmp.Lt, _
        | Cmp.Eq, Cmp.Lt -> fn ~base ~past:mid
        | Cmp.Eq, Cmp.Eq -> Some (Cmp.Eq, mid)
        | Cmp.Eq, Cmp.Gt
        | Cmp.Gt, _ -> fn ~base:(Uint.succ mid) ~past
      end
  end in
  fn ~base ~past

let psearch ?base ?past t key ~cmp =
  search_impl ?base ?past t key ~cmp Cmp.Lt

let search ?base ?past t key ~cmp =
  match search_impl ?base ?past t key ~cmp Cmp.Eq with
  | Some (_, i) -> Some i
  | _ -> None

let nsearch ?base ?past t key ~cmp =
  search_impl ?base ?past t key ~cmp Cmp.Gt

let map t ~f =
  init (length t) ~f:(fun i -> f (get t i))

let mapi t ~f =
  init (length t) ~f:(fun i -> f i (get t i))

module Array_foldi_map = struct
  module T = struct
    type 'a outer = 'a t
    type ('a, 'accum, 'b) t = {
      arr: 'a outer;
      f: uint -> 'accum -> 'a -> 'accum * 'b;
      index: uint;
      length: uint;
    }

    let init arr ~f length =
      {arr; f; length; index=0}

    let length t =
      t.length

    let next t accum =
      let accum', elm' = t.f t.index accum (get t.arr t.index) in
      let t' = {t with index=Uint.succ t.index} in
      elm', t', accum'
  end
  include T
  include Seq.Make_poly2(T)
end

let fold_map t ~init ~f =
    Array_foldi_map.to_accum_array
      (Array_foldi_map.init t ~f:(fun _ accum elm -> f accum elm) (length t))
      ~init

let foldi_map t ~init ~f =
    Array_foldi_map.to_accum_array (Array_foldi_map.init t ~f (length t)) ~init

let filter t ~f =
  let _, t' = Array_foldi_map.to_accum_array
      (Array_foldi_map.init t ~f:(fun _ i _ ->
         let rec fn i elm = begin
           let i' = Uint.succ i in
           match f elm with
           | true -> i', elm
           | false -> fn i' (get t i')
         end in
         fn i (get t i)
       ) (count t ~f)) ~init:0 in
  t'

let filteri t ~f =
  let n = foldi t ~init:0 ~f:(fun i accum elm ->
    match f i elm with
    | false -> accum
    | true -> Uint.succ accum
  ) in
  let _, t' = Array_foldi_map.to_accum_array
      (Array_foldi_map.init t ~f:(fun _ i _ ->
         let rec fn i elm = begin
           let i' = Uint.succ i in
           match f i elm with
           | true -> i', elm
           | false -> fn i' (get t i')
         end in
         fn i (get t i)
       ) n)
      ~init:0 in
  t'

let foldi2_until t0 t1 ~init ~f =
  assert Uint.((length t0) = (length t1));
  let len = length t0 in
  let rec fn i accum = begin
    match Uint.(i = len) with
    | true -> accum
    | false -> begin
        let accum', until = f i accum (get t0 i) (get t1 i) in
        match until with
        | true -> accum'
        | false -> fn (Uint.succ i) accum'
      end
  end in
  fn 0 init

let fold2_until t0 t1 ~init ~f =
  foldi2_until t0 t1 ~init ~f:(fun _ accum a b -> f accum a b)

let fold2 t0 t1 ~init ~f =
  fold2_until t0 t1 ~init ~f:(fun accum a b -> (f accum a b), false)

let foldi2 t0 t1 ~init ~f =
  foldi2_until t0 t1 ~init ~f:(fun i accum a b -> (f i accum a b), false)

let iter2 t0 t1 ~f =
  assert Uint.((length t0) = (length t1));
  for i = 0 to Uint.(pred (length t0)) do
    f (get t0 i) (get t1 i)
  done

let iteri2 t0 t1 ~f =
  assert Uint.((length t0) = (length t1));
  for i = 0 to Uint.(pred (length t0)) do
    f i (get t0 i) (get t1 i)
  done

let map2 t0 t1 ~f =
  assert Uint.((length t0) = (length t1));
  init (length t0) ~f:(fun i -> f (get t0 i) (get t1 i))

let mapi2 t0 t1 ~f =
  assert Uint.((length t0) = (length t1));
  init (length t0) ~f:(fun i -> f i (get t0 i) (get t1 i))

module Array_foldi2_map = struct
  module T = struct
    type 'a outer = 'a t
    type ('a, 'b, 'accum, 'c) t = {
      arr0: 'a outer;
      arr1: 'b outer;
      f: uint -> 'accum -> 'a -> 'b -> 'accum * 'c;
      index: uint;
    }

    let init arr0 arr1 ~f =
      {arr0; arr1; f; index=0}

    let length t =
      length t.arr0

    let next t accum =
      let accum', elm' =
        t.f t.index accum (get t.arr0 t.index) (get t.arr1 t.index) in
      let t' = {t with index=Uint.succ t.index} in
      elm', t', accum'
  end
  include T
  include Seq.Make_poly3(T)
end

let fold2_map t0 t1 ~init ~f =
    Array_foldi2_map.to_accum_array
      (Array_foldi2_map.init t0 t1 ~f:(fun _ accum elm -> f accum elm)) ~init

let foldi2_map t0 t1 ~init ~f =
    Array_foldi2_map.to_accum_array (Array_foldi2_map.init t0 t1 ~f) ~init

let zip t0 t1 =
  map2 t0 t1 ~f:(fun a b -> a, b)

let unzip t =
  let t0 = map t ~f:(fun (a, _) -> a) in
  let t1 = map t ~f:(fun (_, b) -> b) in
  t0, t1

let sexp_of_t t =
  Sexplib.Std.sexp_of_array t

let t_of_sexp sexp =
  Sexplib.Std.array_of_sexp sexp

(*******************************************************************************
 * Begin tests.
 *)

let%expect_test "get,length,is_empty" =
  let open Printf in
  let test_length arr = begin
    printf "[|";
    for i = 0 to Uint.(pred (length arr)) do
      if Uint.(i > 0) then printf "; ";
      printf "%u" (get arr i)
    done;
    printf "|]: length=%u, is_empty=%B\n" (length arr) (is_empty arr)
  end in
  test_length [||];
  test_length [|0|];
  test_length [|0; 1|];
  test_length [|0; 1; 2|];

  [%expect{|
    [||]: length=0, is_empty=true
    [|0|]: length=1, is_empty=false
    [|0; 1|]: length=2, is_empty=false
    [|0; 1; 2|]: length=3, is_empty=false
    |}]

let%expect_test "set,set_inplace" =
  let open Printf in
  let print_uint_array arr = begin
    printf "[|";
    iteri arr ~f:(fun i elm ->
      if Uint.(i > 0) then printf "; ";
      printf "%u" elm
    );
    printf "|]"
  end in
  let test_set len = begin
    for i = 0 to Uint.(pred len) do
      let arr = init len ~f:(fun _ -> 0) in
      let arr' = set arr i 1 in
      printf "set %u: "i;
      print_uint_array arr;
      printf " -> ";
      print_uint_array arr';
      printf " -> set_inplace: ";
      set_inplace arr i 1;
      print_uint_array arr;
      printf " -> copy,set_inplace: ";
      let arr'' = copy arr in
      set_inplace arr'' i 2;
      print_uint_array arr;
      printf " -> ";
      print_uint_array arr'';
      printf "\n";
    done
  end in
  test_set 1;
  test_set 2;
  test_set 3;

  [%expect{|
    set 0: [|0|] -> [|1|] -> set_inplace: [|1|] -> copy,set_inplace: [|1|] -> [|2|]
    set 0: [|0; 0|] -> [|1; 0|] -> set_inplace: [|1; 0|] -> copy,set_inplace: [|1; 0|] -> [|2; 0|]
    set 1: [|0; 0|] -> [|0; 1|] -> set_inplace: [|0; 1|] -> copy,set_inplace: [|0; 1|] -> [|0; 2|]
    set 0: [|0; 0; 0|] -> [|1; 0; 0|] -> set_inplace: [|1; 0; 0|] -> copy,set_inplace: [|1; 0; 0|] -> [|2; 0; 0|]
    set 1: [|0; 0; 0|] -> [|0; 1; 0|] -> set_inplace: [|0; 1; 0|] -> copy,set_inplace: [|0; 1; 0|] -> [|0; 2; 0|]
    set 2: [|0; 0; 0|] -> [|0; 0; 1|] -> set_inplace: [|0; 0; 1|] -> copy,set_inplace: [|0; 0; 1|] -> [|0; 0; 2|]
    |}]

let%expect_test "pare" =
  let open Printf in
  let print_uint_array arr = begin
    printf "[|";
    iteri arr ~f:(fun i elm ->
      if Uint.(i > 0) then printf "; ";
      printf "%u" elm
    );
    printf "|]"
  end in
  let test arr = begin
    printf "pare ";
    print_uint_array arr;
    printf " ->";
    for i = 0 to length arr do
      for j =  i to length arr do
        let arr' = pare arr ~base:i ~past:j in
        printf " [%u,%u)=" i j;
        print_uint_array arr'
      done
    done;
    printf "\n"
  end in
  test [||];
  test [|0|];
  test [|0; 1|];
  test [|0; 1; 2|];

  [%expect{|
    pare [||] -> [0,0)=[||]
    pare [|0|] -> [0,0)=[||] [0,1)=[|0|] [1,1)=[||]
    pare [|0; 1|] -> [0,0)=[||] [0,1)=[|0|] [0,2)=[|0; 1|] [1,1)=[||] [1,2)=[|1|] [2,2)=[||]
    pare [|0; 1; 2|] -> [0,0)=[||] [0,1)=[|0|] [0,2)=[|0; 1|] [0,3)=[|0; 1; 2|] [1,1)=[||] [1,2)=[|1|] [1,3)=[|1; 2|] [2,2)=[||] [2,3)=[|2|] [3,3)=[||]
    |}]

let%expect_test "concat_list" =
  let open Printf in
  let print_uint_array arr = begin
    printf "[|";
    iteri arr ~f:(fun i elm ->
      if Uint.(i > 0) then printf "; ";
      printf "%u" elm
    );
    printf "|]"
  end in
  let test arrs = begin
    printf "concat_list [";
    List.iteri (fun i arr ->
      if Int.(i > 0) then printf "; ";
      print_uint_array arr
    ) arrs;
    printf "] -> ";
    let arr = concat_list arrs in
    print_uint_array arr;
    printf "\n"
  end in
  test [];
  test [[||]];
  test [[||]; [||]];
  test [[||]; [||]; [||]];

  test [[|0|]];

  test [[|0|]; [||]];
  test [[||]; [|0|]];
  test [[|0|]; [|1|]];

  test [[|0|]; [||]; [||]];
  test [[||]; [|0|]; [||]];
  test [[||]; [||]; [|0|]];
  test [[|0|]; [|1|]; [||]];
  test [[|0|]; [||]; [|1|]];
  test [[|0|]; [|1|]; [|2|]];

  [%expect{|
    concat_list [] -> [||]
    concat_list [[||]] -> [||]
    concat_list [[||]; [||]] -> [||]
    concat_list [[||]; [||]; [||]] -> [||]
    concat_list [[|0|]] -> [|0|]
    concat_list [[|0|]; [||]] -> [|0|]
    concat_list [[||]; [|0|]] -> [|0|]
    concat_list [[|0|]; [|1|]] -> [|0; 1|]
    concat_list [[|0|]; [||]; [||]] -> [|0|]
    concat_list [[||]; [|0|]; [||]] -> [|0|]
    concat_list [[||]; [||]; [|0|]] -> [|0|]
    concat_list [[|0|]; [|1|]; [||]] -> [|0; 1|]
    concat_list [[|0|]; [||]; [|1|]] -> [|0; 1|]
    concat_list [[|0|]; [|1|]; [|2|]] -> [|0; 1; 2|]
    |}]

let%expect_test "concat" =
  let open Printf in
  let print_uint_array arr = begin
    printf "[|";
    iteri arr ~f:(fun i elm ->
      if Uint.(i > 0) then printf "; ";
      printf "%u" elm
    );
    printf "|]"
  end in
  let test arr0 arr1 = begin
    printf "concat ";
    print_uint_array arr0;
    printf " ";
    print_uint_array arr1;
    printf " -> ";
    let arr = concat arr0 arr1 in
    print_uint_array arr;
    printf "\n"
  end in
  test [||] [||];
  test [|0|] [||];
  test [||] [|0|];
  test [|0|] [|1|];
  test [|0; 1|] [|2|];
  test [|0|] [|1; 2|];
  test [|0; 1|] [|2; 3|];

  [%expect{|
    concat [||] [||] -> [||]
    concat [|0|] [||] -> [|0|]
    concat [||] [|0|] -> [|0|]
    concat [|0|] [|1|] -> [|0; 1|]
    concat [|0; 1|] [|2|] -> [|0; 1; 2|]
    concat [|0|] [|1; 2|] -> [|0; 1; 2|]
    concat [|0; 1|] [|2; 3|] -> [|0; 1; 2; 3|]
    |}]

let%expect_test "append,prepend" =
  let open Printf in
  let print_uint_array arr = begin
    printf "[|";
    iteri arr ~f:(fun i elm ->
      if Uint.(i > 0) then printf "; ";
      printf "%u" elm
    );
    printf "|]"
  end in
  let test arr x = begin
    print_uint_array arr;
    printf " %u: append -> " x;
    let arr_x = append arr x in
    print_uint_array arr_x;
    printf ", prepend -> ";
    let x_arr = prepend x arr in
    print_uint_array x_arr;
    printf "\n"
  end in
  test [||] 0;
  test [|0|] 1;
  test [|0; 1|] 2;
  test [|0; 1; 2|] 3;

  [%expect{|
    [||] 0: append -> [|0|], prepend -> [|0|]
    [|0|] 1: append -> [|0; 1|], prepend -> [|1; 0|]
    [|0; 1|] 2: append -> [|0; 1; 2|], prepend -> [|2; 0; 1|]
    [|0; 1; 2|] 3: append -> [|0; 1; 2; 3|], prepend -> [|3; 0; 1; 2|]
    |}]

let%expect_test "insert" =
  let open Printf in
  let print_uint_array arr = begin
    printf "[|";
    iteri arr ~f:(fun i elm ->
      if Uint.(i > 0) then printf "; ";
      printf "%u" elm
    );
    printf "|]"
  end in
  let test arr x = begin
    printf "insert ";
    print_uint_array arr;
    printf " %u ->" x;
    for i = 0 to length arr do
      let arr' = insert arr i x in
      printf " ";
      print_uint_array arr'
    done;
    printf "\n"
  end in
  test [||] 0;
  test [|0|] 1;
  test [|0; 1|] 2;
  test [|0; 1; 2|] 3;

  [%expect{|
    insert [||] 0 -> [|0|]
    insert [|0|] 1 -> [|1; 0|] [|0; 1|]
    insert [|0; 1|] 2 -> [|2; 0; 1|] [|0; 2; 1|] [|0; 1; 2|]
    insert [|0; 1; 2|] 3 -> [|3; 0; 1; 2|] [|0; 3; 1; 2|] [|0; 1; 3; 2|] [|0; 1; 2; 3|]
    |}]

let%expect_test "remove" =
  let open Printf in
  let print_uint_array arr = begin
    printf "[|";
    iteri arr ~f:(fun i elm ->
      if Uint.(i > 0) then printf "; ";
      printf "%u" elm
    );
    printf "|]"
  end in
  let test arr = begin
    printf "remove ";
    print_uint_array arr;
    printf " ->";
    for i = 0 to Uint.(pred (length arr)) do
      let arr' = remove arr i in
      printf " ";
      print_uint_array arr'
    done;
    printf "\n"
  end in
  test [|0|];
  test [|0; 1|];
  test [|0; 1; 2|];

  [%expect{|
    remove [|0|] -> [||]
    remove [|0; 1|] -> [|1|] [|0|]
    remove [|0; 1; 2|] -> [|1; 2|] [|0; 2|] [|0; 1|]
    |}]

let%expect_test "reduce" =
  let open Printf in
  let print_uint_array arr = begin
    printf "[|";
    iteri arr ~f:(fun i elm ->
      if Uint.(i > 0) then printf "; ";
      printf "%u" elm
    );
    printf "|]"
  end in
  let test_reduce arr ~f = begin
    printf "reduce ";
    print_uint_array arr;
    match reduce arr ~f with
    | None -> printf " -> None\n"
    | Some x -> printf " -> %u\n" x
  end in
  let f a b =
    Uint.(a + b)
  in
  test_reduce [||] ~f;
  test_reduce [|0; 1; 2; 3; 4|] ~f;

  [%expect{|
    reduce [||] -> None
    reduce [|0; 1; 2; 3; 4|] -> 10
    |}]

let%expect_test "swap,swap_inplace" =
  let open Printf in
  let print_uint_array arr = begin
    printf "[|";
    iteri arr ~f:(fun i elm ->
      if Uint.(i > 0) then printf "; ";
      printf "%u" elm
    );
    printf "|]"
  end in
  let test_swap arr = begin
    for i = 0 to Uint.(pred (length arr)) do
      for j = i to Uint.(pred (length arr)) do
        let arr' = copy arr in
        printf "%u %u: swap " i j;
        print_uint_array arr';
        printf " -> ";
        print_uint_array (swap arr' i j);

        printf " -> swap_inplace ";
        print_uint_array arr';
        swap_inplace arr' i j;
        printf " -> ";
        print_uint_array arr';
        printf "\n"
      done
    done
  end in
  test_swap [|0|];
  test_swap [|0; 1|];
  test_swap [|0; 1; 2|];
  test_swap [|0; 1; 2; 3|];

  [%expect{|
    0 0: swap [|0|] -> [|0|] -> swap_inplace [|0|] -> [|0|]
    0 0: swap [|0; 1|] -> [|0; 1|] -> swap_inplace [|0; 1|] -> [|0; 1|]
    0 1: swap [|0; 1|] -> [|1; 0|] -> swap_inplace [|0; 1|] -> [|1; 0|]
    1 1: swap [|0; 1|] -> [|0; 1|] -> swap_inplace [|0; 1|] -> [|0; 1|]
    0 0: swap [|0; 1; 2|] -> [|0; 1; 2|] -> swap_inplace [|0; 1; 2|] -> [|0; 1; 2|]
    0 1: swap [|0; 1; 2|] -> [|1; 0; 2|] -> swap_inplace [|0; 1; 2|] -> [|1; 0; 2|]
    0 2: swap [|0; 1; 2|] -> [|2; 1; 0|] -> swap_inplace [|0; 1; 2|] -> [|2; 1; 0|]
    1 1: swap [|0; 1; 2|] -> [|0; 1; 2|] -> swap_inplace [|0; 1; 2|] -> [|0; 1; 2|]
    1 2: swap [|0; 1; 2|] -> [|0; 2; 1|] -> swap_inplace [|0; 1; 2|] -> [|0; 2; 1|]
    2 2: swap [|0; 1; 2|] -> [|0; 1; 2|] -> swap_inplace [|0; 1; 2|] -> [|0; 1; 2|]
    0 0: swap [|0; 1; 2; 3|] -> [|0; 1; 2; 3|] -> swap_inplace [|0; 1; 2; 3|] -> [|0; 1; 2; 3|]
    0 1: swap [|0; 1; 2; 3|] -> [|1; 0; 2; 3|] -> swap_inplace [|0; 1; 2; 3|] -> [|1; 0; 2; 3|]
    0 2: swap [|0; 1; 2; 3|] -> [|2; 1; 0; 3|] -> swap_inplace [|0; 1; 2; 3|] -> [|2; 1; 0; 3|]
    0 3: swap [|0; 1; 2; 3|] -> [|3; 1; 2; 0|] -> swap_inplace [|0; 1; 2; 3|] -> [|3; 1; 2; 0|]
    1 1: swap [|0; 1; 2; 3|] -> [|0; 1; 2; 3|] -> swap_inplace [|0; 1; 2; 3|] -> [|0; 1; 2; 3|]
    1 2: swap [|0; 1; 2; 3|] -> [|0; 2; 1; 3|] -> swap_inplace [|0; 1; 2; 3|] -> [|0; 2; 1; 3|]
    1 3: swap [|0; 1; 2; 3|] -> [|0; 3; 2; 1|] -> swap_inplace [|0; 1; 2; 3|] -> [|0; 3; 2; 1|]
    2 2: swap [|0; 1; 2; 3|] -> [|0; 1; 2; 3|] -> swap_inplace [|0; 1; 2; 3|] -> [|0; 1; 2; 3|]
    2 3: swap [|0; 1; 2; 3|] -> [|0; 1; 3; 2|] -> swap_inplace [|0; 1; 2; 3|] -> [|0; 1; 3; 2|]
    3 3: swap [|0; 1; 2; 3|] -> [|0; 1; 2; 3|] -> swap_inplace [|0; 1; 2; 3|] -> [|0; 1; 2; 3|]
    |}]

let%expect_test "rev,rev_inplace" =
  let open Printf in
  let print_uint_array arr = begin
    printf "[|";
    iteri arr ~f:(fun i elm ->
      if Uint.(i > 0) then printf "; ";
      printf "%u" elm
    );
    printf "|]"
  end in
  let test_rev arr = begin
    printf "rev ";
    print_uint_array arr;
    printf " -> ";
    print_uint_array (rev arr);

    printf " -> rev_inplace ";
    print_uint_array arr;
    printf " -> ";
    rev_inplace arr;
    print_uint_array arr;

    printf "\n"
  end in
  test_rev [|0|];
  test_rev [|0; 1|];
  test_rev [|0; 1; 2|];
  test_rev [|0; 1; 2; 3|];

  [%expect{|
    rev [|0|] -> [|0|] -> rev_inplace [|0|] -> [|0|]
    rev [|0; 1|] -> [|1; 0|] -> rev_inplace [|0; 1|] -> [|1; 0|]
    rev [|0; 1; 2|] -> [|2; 1; 0|] -> rev_inplace [|0; 1; 2|] -> [|2; 1; 0|]
    rev [|0; 1; 2; 3|] -> [|3; 2; 1; 0|] -> rev_inplace [|0; 1; 2; 3|] -> [|3; 2; 1; 0|]
    |}]

let%expect_test "blit" =
  let open Printf in
  let print_uint_array arr = begin
    printf "[|";
    iteri arr ~f:(fun i elm ->
      if Uint.(i > 0) then printf "; ";
      printf "%u" elm
    );
    printf "|]"
  end in
  let test_blit arr0 i0 arr1 i1 len = begin
    printf "blit ";
    print_uint_array arr0;
    printf " %u " i0;
    print_uint_array arr1;
    printf " %u %u -> " i1 len;
    blit arr0 i0 arr1 i1 len;
    print_uint_array arr1;
    printf "\n"
  end in
  test_blit [||] 0 [||] 0 0;
  test_blit [|0|] 0 [|1|] 0 1;
  test_blit [|0; 1|] 1 [|2|] 0 1;
  test_blit [|0|] 0 [|1; 2|] 1 1;
  test_blit [|0; 1|] 0 [|2; 3|] 0 2;
  test_blit [|0; 1; 2|] 1 [|3; 4; 5|] 0 2;
  test_blit [|0; 1; 2|] 0 [|3; 4; 5|] 0 3;

  [%expect{|
    blit [||] 0 [||] 0 0 -> [||]
    blit [|0|] 0 [|1|] 0 1 -> [|0|]
    blit [|0; 1|] 1 [|2|] 0 1 -> [|1|]
    blit [|0|] 0 [|1; 2|] 1 1 -> [|1; 0|]
    blit [|0; 1|] 0 [|2; 3|] 0 2 -> [|0; 1|]
    blit [|0; 1; 2|] 1 [|3; 4; 5|] 0 2 -> [|1; 2; 5|]
    blit [|0; 1; 2|] 0 [|3; 4; 5|] 0 3 -> [|0; 1; 2|]
    |}]

let%expect_test "is_sorted" =
  let open Printf in
  let print_uint_array arr = begin
    printf "[|";
    iteri arr ~f:(fun i elm ->
      if Uint.(i > 0) then printf "; ";
      printf "%u" elm
    );
    printf "|]"
  end in
  let test_is_sorted arr = begin
    printf "is_sorted ";
    print_uint_array arr;
    printf ": not strict -> %B, strict -> %B\n"
      (is_sorted arr ~cmp:Uint.cmp) (is_sorted ~strict:true arr ~cmp:Uint.cmp)
  end in
  test_is_sorted [||];
  test_is_sorted [|0|];
  test_is_sorted [|0; 0|];
  test_is_sorted [|0; 1|];
  test_is_sorted [|1; 0|];
  test_is_sorted [|0; 1; 1|];
  test_is_sorted [|0; 1; 2|];
  test_is_sorted [|0; 2; 1|];

  [%expect{|
    is_sorted [||]: not strict -> true, strict -> true
    is_sorted [|0|]: not strict -> true, strict -> true
    is_sorted [|0; 0|]: not strict -> true, strict -> false
    is_sorted [|0; 1|]: not strict -> true, strict -> true
    is_sorted [|1; 0|]: not strict -> false, strict -> false
    is_sorted [|0; 1; 1|]: not strict -> true, strict -> false
    is_sorted [|0; 1; 2|]: not strict -> true, strict -> true
    is_sorted [|0; 2; 1|]: not strict -> false, strict -> false
    |}]

type sort_elm = {
  key: uint; (* Random, possibly non-unique. *)
  sn: uint; (* Sequential in initial array. *)
}
let%expect_test "sort" =
  let gen_array len = begin
    let key_limit =
      if Uint.(len > 0) then len
      else 1
    in
    init len ~f:(fun i -> {key=(Stdlib.Random.int key_limit); sn=i})
  end in
  let cmp elm0 elm1 =
    Uint.cmp elm0.key elm1.key
  in
  let test_sort arr = begin
    let arr' = sort arr ~cmp in
    assert (is_sorted arr' ~cmp);

    let arr' = sort ~stable:true arr ~cmp in
    assert (is_sorted ~strict:true arr' ~cmp:(fun elm0 elm1 ->
      match cmp elm0 elm1 with
      | Cmp.Lt -> Cmp.Lt
      | Cmp.Eq -> Uint.cmp elm0.sn elm1.sn
      | Cmp.Gt -> Cmp.Gt
    ));
  end in
  Stdlib.Random.init 0;
  for len = 0 to 257 do
    for _ = 1 to 10 do
      test_sort (gen_array len)
    done
  done;

  [%expect{|
    |}]

let%expect_test "search" =
  let open Printf in
  let print_uint_array arr = begin
    printf "[|";
    iteri arr ~f:(fun i elm ->
      if Uint.(i > 0) then printf "; ";
      printf "%u" elm
    );
    printf "|]"
  end in
  let test_search arr key_max = begin
    print_uint_array arr;
    printf "\n";
    for probe = 0 to key_max do
      printf "  %u -> %s, %s, %s\n" probe
        (match psearch arr probe ~cmp:Uint.cmp with
         | None -> "<"
         | Some (Cmp.Lt, i) -> sprintf "<[%u]=%u" i (get arr i)
         | Some (Cmp.Eq, i) -> sprintf "=[%u]=%u" i (get arr i)
         | Some (Cmp.Gt, i) -> sprintf ">[%u]=%u" i (get arr i)
        )
        (match search arr probe ~cmp:Uint.cmp with
         | None -> "<>"
         | Some i -> sprintf "=%u" (get arr i)
        )
        (match nsearch arr probe ~cmp:Uint.cmp with
         | Some (Cmp.Lt, i) -> sprintf "<[%u]=%u" i (get arr i)
         | Some (Cmp.Eq, i) -> sprintf "=[%u]=%u" i (get arr i)
         | Some (Cmp.Gt, i) -> sprintf ">[%u]=%u" i (get arr i)
         | None -> ">"
        );
    done
  end in
  for len = 0 to 3 do
    let arr = init len ~f:(fun i -> Uint.(i * 2 + 1)) in
    let key_max = Uint.(len * 2) in
    test_search arr key_max
  done;
  for hlen = 1 to 3 do
    let len = Uint.(hlen * 2) in
    let arr = init len ~f:(fun i -> Uint.(i + ((i+1) % 2))) in
    let key_max = len in
    test_search arr key_max
  done;

  [%expect{|
    [||]
      0 -> <, <>, >
    [|1|]
      0 -> <[0]=1, <>, <[0]=1
      1 -> =[0]=1, =1, =[0]=1
      2 -> >[0]=1, <>, >[0]=1
    [|1; 3|]
      0 -> <[0]=1, <>, <[0]=1
      1 -> =[0]=1, =1, =[0]=1
      2 -> >[0]=1, <>, <[1]=3
      3 -> =[1]=3, =3, =[1]=3
      4 -> >[1]=3, <>, >[1]=3
    [|1; 3; 5|]
      0 -> <[0]=1, <>, <[0]=1
      1 -> =[0]=1, =1, =[0]=1
      2 -> >[0]=1, <>, <[1]=3
      3 -> =[1]=3, =3, =[1]=3
      4 -> >[1]=3, <>, <[2]=5
      5 -> =[2]=5, =5, =[2]=5
      6 -> >[2]=5, <>, >[2]=5
    [|1; 1|]
      0 -> <[0]=1, <>, <[0]=1
      1 -> =[0]=1, =1, =[1]=1
      2 -> >[1]=1, <>, >[1]=1
    [|1; 1; 3; 3|]
      0 -> <[0]=1, <>, <[0]=1
      1 -> =[0]=1, =1, =[1]=1
      2 -> >[1]=1, <>, <[2]=3
      3 -> =[2]=3, =3, =[3]=3
      4 -> >[3]=3, <>, >[3]=3
    [|1; 1; 3; 3; 5; 5|]
      0 -> <[0]=1, <>, <[0]=1
      1 -> =[0]=1, =1, =[1]=1
      2 -> >[1]=1, <>, <[2]=3
      3 -> =[2]=3, =3, =[3]=3
      4 -> >[3]=3, <>, <[4]=5
      5 -> =[4]=5, =5, =[5]=5
      6 -> >[5]=5, <>, >[5]=5
    |}]

let%expect_test "map,mapi" =
  let open Printf in
  let print_uint_array arr = begin
    printf "[|";
    iteri arr ~f:(fun i elm ->
      if Uint.(i > 0) then printf "; ";
      printf "%u" elm
    );
    printf "|]"
  end in
  let print_string_array arr = begin
    printf "[|";
    iteri arr ~f:(fun i elm ->
      if Uint.(i > 0) then printf "; ";
      printf "\"%s\"" elm
    );
    printf "|]"
  end in
  let test_map uarr = begin
    print_uint_array uarr;
    printf " -> map ";
    let sarr = map uarr ~f:(fun elm -> sprintf "%u" elm) in
    print_string_array sarr;
    printf " -> mapi ";
    let sarr = mapi uarr ~f:(fun i elm -> sprintf "[%u]=%u" i elm) in
    print_string_array sarr;
    printf "\n"
  end in
  test_map [||];
  test_map [|0|];
  test_map [|1; 0|];
  test_map [|2; 1; 0|];

  [%expect{|
    [||] -> map [||] -> mapi [||]
    [|0|] -> map [|"0"|] -> mapi [|"[0]=0"|]
    [|1; 0|] -> map [|"1"; "0"|] -> mapi [|"[0]=1"; "[1]=0"|]
    [|2; 1; 0|] -> map [|"2"; "1"; "0"|] -> mapi [|"[0]=2"; "[1]=1"; "[2]=0"|]
    |}]

let%expect_test "fold_map,foldi_map" =
  let open Printf in
  let print_uint_array arr = begin
    printf "[|";
    iteri arr ~f:(fun i elm ->
      if Uint.(i > 0) then printf "; ";
      printf "%u" elm
    );
    printf "|]"
  end in
  let print_string_array arr = begin
    printf "[|";
    iteri arr ~f:(fun i elm ->
      if Uint.(i > 0) then printf "; ";
      printf "\"%s\"" elm
    );
    printf "|]"
  end in
  let test_fold_map uarr = begin
    print_uint_array uarr;
    printf " -> fold_map";
    let accum, sarr = fold_map uarr ~init:0 ~f:(fun accum elm ->
      Uint.(accum + elm), (sprintf "%u" elm)
    ) in
    printf " %u " accum;
    print_string_array sarr;
    printf " -> foldi_map";
    let accum, sarr = foldi_map uarr ~init:0 ~f:(fun i accum elm ->
      Uint.(accum + i + elm), (sprintf "[%u]=%u" i elm)
    ) in
    printf " %u " accum;
    print_string_array sarr;
    printf "\n"
  end in
  test_fold_map [||];
  test_fold_map [|0|];
  test_fold_map [|1; 0|];
  test_fold_map [|2; 1; 0|];

  [%expect{|
    [||] -> fold_map 0 [||] -> foldi_map 0 [||]
    [|0|] -> fold_map 0 [|"0"|] -> foldi_map 0 [|"[0]=0"|]
    [|1; 0|] -> fold_map 1 [|"1"; "0"|] -> foldi_map 2 [|"[0]=1"; "[1]=0"|]
    [|2; 1; 0|] -> fold_map 3 [|"2"; "1"; "0"|] -> foldi_map 6 [|"[0]=2"; "[1]=1"; "[2]=0"|] |}]

let%expect_test "filter,filteri" =
  let open Printf in
  let print_uint_array arr = begin
    printf "[|";
    iteri arr ~f:(fun i elm ->
      if Uint.(i > 0) then printf "; ";
      printf "%u" elm
    );
    printf "|]"
  end in
  let test_filter arr = begin
    print_uint_array arr;
    printf " -> filter ";
    let farr = filter arr ~f:(fun elm -> Uint.(elm % 2 = 0)) in
    print_uint_array farr;
    printf " -> filteri ";
    let farr = filteri arr ~f:(fun _ elm -> Uint.(elm % 2 = 0)) in
    print_uint_array farr;
    printf " ";
    let farr = filteri arr ~f:(fun i _ -> Uint.(i % 2 = 0)) in
    print_uint_array farr;
    printf "\n"
  end in
  test_filter [||];
  test_filter [|0|];
  test_filter [|1; 0|];
  test_filter [|2; 1; 0|];
  test_filter [|3; 2; 1; 0|];

  [%expect{|
    [||] -> filter [||] -> filteri [||] [||]
    [|0|] -> filter [|0|] -> filteri [|0|] [|0|]
    [|1; 0|] -> filter [|0|] -> filteri [|0|] [|1|]
    [|2; 1; 0|] -> filter [|2; 0|] -> filteri [|2; 0|] [|2; 0|]
    [|3; 2; 1; 0|] -> filter [|2; 0|] -> filteri [|2; 0|] [|3; 1|] |}]

let%expect_test "fold2_until,foldi2_until" =
  let open Printf in
  let print_uint_array arr = begin
    printf "[|";
    iteri arr ~f:(fun i elm ->
      if Uint.(i > 0) then printf "; ";
      printf "%u" elm
    );
    printf "|]"
  end in
  let test_fold2_until uarr0 uarr1 = begin
    print_uint_array uarr0;
    printf " ";
    print_uint_array uarr1;
    let accum = fold2_until uarr0 uarr1 ~init:0 ~f:(fun accum elm0 elm1 ->
      Uint.(accum + elm0 + elm1), Uint.(accum > 10)
    ) in
    printf " -> fold2_until %u" accum;
    let accum = foldi2_until uarr0 uarr1 ~init:0 ~f:(fun i accum elm0 elm1 ->
      Uint.(accum + i + elm0 + elm1), Uint.((i + 2) >= (length uarr0))
    ) in
    printf " -> foldi2_until %u\n" accum
  end in
  test_fold2_until [||] [||];
  test_fold2_until [|1|] [|0|];
  test_fold2_until [|3; 2|] [|1; 0|];
  test_fold2_until [|5; 4; 3|] [|2; 1; 0|];

  [%expect{|
    [||] [||] -> fold2_until 0 -> foldi2_until 0
    [|1|] [|0|] -> fold2_until 1 -> foldi2_until 1
    [|3; 2|] [|1; 0|] -> fold2_until 6 -> foldi2_until 4
    [|5; 4; 3|] [|2; 1; 0|] -> fold2_until 15 -> foldi2_until 13 |}]

let%expect_test "fold2,foldi2" =
  let open Printf in
  let print_uint_array arr = begin
    printf "[|";
    iteri arr ~f:(fun i elm ->
      if Uint.(i > 0) then printf "; ";
      printf "%u" elm
    );
    printf "|]"
  end in
  let test_fold2 uarr0 uarr1 = begin
    print_uint_array uarr0;
    printf " ";
    print_uint_array uarr1;
    let accum = fold2 uarr0 uarr1 ~init:0 ~f:(fun accum elm0 elm1 ->
      Uint.(accum + elm0 + elm1)
    ) in
    printf " -> fold2 %u" accum;
    let accum = foldi2 uarr0 uarr1 ~init:0 ~f:(fun i accum elm0 elm1 ->
      Uint.(accum + i + elm0 + elm1)
    ) in
    printf " -> foldi2 %u\n" accum
  end in
  test_fold2 [||] [||];
  test_fold2 [|1|] [|0|];
  test_fold2 [|3; 2|] [|1; 0|];
  test_fold2 [|5; 4; 3|] [|2; 1; 0|];

  [%expect{|
    [||] [||] -> fold2 0 -> foldi2 0
    [|1|] [|0|] -> fold2 1 -> foldi2 1
    [|3; 2|] [|1; 0|] -> fold2 6 -> foldi2 7
    [|5; 4; 3|] [|2; 1; 0|] -> fold2 15 -> foldi2 18 |}]

let%expect_test "iteri2" =
  let open Printf in
  let print_uint_arrays arr0 arr1 = begin
    printf "[|";
    iteri2 arr0 arr1 ~f:(fun i elm0 elm1 ->
      if Uint.(i > 0) then printf "; ";
      printf "(%u, %u)" elm0 elm1
    );
    printf "|]"
  end in
  let test_iter2 arr0 arr1 = begin
    print_uint_arrays arr0 arr1;
    printf "\n"
  end in
  test_iter2 [||] [||];
  test_iter2 [|0|] [|1|];
  test_iter2 [|0; 1|] [|2; 3|];
  test_iter2 [|0; 1; 2|] [|3; 4; 5|];

  [%expect{|
    [||]
    [|(0, 1)|]
    [|(0, 2); (1, 3)|]
    [|(0, 3); (1, 4); (2, 5)|] |}]

let%expect_test "map2,mapi2" =
  let open Printf in
  let print_uint_array arr = begin
    printf "[|";
    iteri arr ~f:(fun i elm ->
      if Uint.(i > 0) then printf "; ";
      printf "%u" elm
    );
    printf "|]"
  end in
  let print_string_array arr = begin
    printf "[|";
    iteri arr ~f:(fun i elm ->
      if Uint.(i > 0) then printf "; ";
      printf "\"%s\"" elm
    );
    printf "|]"
  end in
  let test_map2 uarr0 uarr1 = begin
    print_uint_array uarr0;
    printf " ";
    print_uint_array uarr1;
    printf " -> map2 ";
    let sarr = map2 uarr0 uarr1 ~f:(fun elm0 elm1 ->
      sprintf "(%u,%u)" elm0 elm1
    ) in
    print_string_array sarr;
    printf " -> mapi2 ";
    let sarr = mapi2 uarr0 uarr1 ~f:(fun i elm0 elm1 ->
      sprintf "[%u]=(%u,%u)" i elm0 elm1
    ) in
    print_string_array sarr;
    printf "\n"
  end in
  test_map2 [||] [||];
  test_map2 [|1|] [|0|];
  test_map2 [|3; 2|] [|1; 0|];
  test_map2 [|5; 4; 3|] [|2; 1; 0|];

  [%expect{|
    [||] [||] -> map2 [||] -> mapi2 [||]
    [|1|] [|0|] -> map2 [|"(1,0)"|] -> mapi2 [|"[0]=(1,0)"|]
    [|3; 2|] [|1; 0|] -> map2 [|"(3,1)"; "(2,0)"|] -> mapi2 [|"[0]=(3,1)"; "[1]=(2,0)"|]
    [|5; 4; 3|] [|2; 1; 0|] -> map2 [|"(5,2)"; "(4,1)"; "(3,0)"|] -> mapi2 [|"[0]=(5,2)"; "[1]=(4,1)"; "[2]=(3,0)"|] |}]

let%expect_test "fold2_map,foldi2_map" =
  let open Printf in
  let print_uint_array arr = begin
    printf "[|";
    iteri arr ~f:(fun i elm ->
      if Uint.(i > 0) then printf "; ";
      printf "%u" elm
    );
    printf "|]"
  end in
  let print_string_array arr = begin
    printf "[|";
    iteri arr ~f:(fun i elm ->
      if Uint.(i > 0) then printf "; ";
      printf "\"%s\"" elm
    );
    printf "|]"
  end in
  let test_fold2_map uarr0 uarr1 = begin
    print_uint_array uarr0;
    printf " ";
    print_uint_array uarr1;
    printf " -> fold2_map";
    let accum, sarr = fold2_map uarr0 uarr1 ~init:0 ~f:(fun accum elm0 elm1 ->
      Uint.(accum + elm0 + elm1), (sprintf "(%u,%u)" elm0 elm1)
    ) in
    printf " %u " accum;
    print_string_array sarr;
    printf " -> foldi2_map";
    let accum, sarr = foldi2_map uarr0 uarr1 ~init:0
        ~f:(fun i accum elm0 elm1 ->
          Uint.(accum + i + elm0 + elm1), (sprintf "[%u]=(%u,%u)" i elm0 elm1)
        ) in
    printf " %u " accum;
    print_string_array sarr;
    printf "\n"
  end in
  test_fold2_map [||] [||];
  test_fold2_map [|1|] [|0|];
  test_fold2_map [|3; 2|] [|1; 0|];
  test_fold2_map [|5; 4; 3|] [|2; 1; 0|];

  [%expect{|
    [||] [||] -> fold2_map 0 [||] -> foldi2_map 0 [||]
    [|1|] [|0|] -> fold2_map 1 [|"(1,0)"|] -> foldi2_map 1 [|"[0]=(1,0)"|]
    [|3; 2|] [|1; 0|] -> fold2_map 6 [|"(3,1)"; "(2,0)"|] -> foldi2_map 7 [|"[0]=(3,1)"; "[1]=(2,0)"|]
    [|5; 4; 3|] [|2; 1; 0|] -> fold2_map 15 [|"(5,2)"; "(4,1)"; "(3,0)"|] -> foldi2_map 18 [|"[0]=(5,2)"; "[1]=(4,1)"; "[2]=(3,0)"|] |}]

let%expect_test "zip,unzip" =
  let open Printf in
  let print_uint_array arr = begin
    printf "[|";
    iteri arr ~f:(fun i elm ->
      if Uint.(i > 0) then printf "; ";
      printf "%u" elm
    );
    printf "|]"
  end in
  let test_zip arr0 arr1 = begin
    let arr0', arr1' = unzip (zip arr0 arr1) in
    print_uint_array arr0';
    printf " ";
    print_uint_array arr1';
    printf "\n"
  end in
  test_zip [||] [||];
  test_zip [|0|] [|1|];
  test_zip [|0; 1|] [|2; 3|];
  test_zip [|0; 1; 2|] [|3; 4; 5|];

  [%expect{|
    [||] [||]
    [|0|] [|1|]
    [|0; 1|] [|2; 3|]
    [|0; 1; 2|] [|3; 4; 5|] |}]
