open Rudiments0
open RudimentsInt

module List = List0

module T = struct
  type 'a t = 'a array
  type 'a elm = 'a

  let get i t =
    Stdlib.Array.get t i

  let length t =
    Stdlib.Array.length t

  module Cursor = struct
    module T = struct
      type 'a container = 'a t
      type 'a elm = 'a
      type 'a t = {
        array: 'a container;
        index: uns;
      }

      let cmp t0 t1 =
        (* == is excessively vague in OCaml. *)
        assert ((t0.array == t1.array) || (Stdlib.( = ) t0.array t1.array));
        Uns.cmp t0.index t1.index

      let hd array =
        {array; index=0}

      let tl array =
        {array; index=(length array)}

      let seek i t =
        match Sint.(i < (kv 0)) with
        | true -> begin
            match (Uns.of_sint Sint.(neg i)) > t.index with
            | true -> halt "Cannot seek before beginning of array"
            | false -> {t with index=(t.index - Uns.of_sint (Sint.neg i))}
          end
        | false -> begin
            match (t.index + (Uns.of_sint i)) > (length t.array) with
            | true -> halt "Cannot seek past end of array"
            | false -> {t with index=(t.index + (Uns.of_sint i))}
          end

      let pred t =
        seek (Sint.kv (-1)) t

      let succ t =
        seek (Sint.kv 1) t

      let lget t =
        get (Uns.pred t.index) t.array

      let rget t =
        get t.index t.array

      let prev t =
        lget t, pred t

      let next t =
        rget t, succ t

      let container t =
        t.array

      let index t =
        t.index
    end
    include T
    include Cmpable.MakePoly(T)
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
include ContainerCommon.MakePolyFold(T)

let hash_fold hash_fold_a t state =
  foldi t ~init:state ~f:(fun i state elm ->
    state
    |> Uns.hash_fold i
    |> hash_fold_a elm
  )
  |> Uns.hash_fold (length t)

module Seq = struct
  type 'a outer = 'a t
  module type SMono = sig
    type t
    type elm
    val to_array: t -> elm outer
  end
  module type SPoly = sig
    type 'a t
    type 'a elm
    val to_array: 'a t -> 'a elm outer
  end
  module type SPoly2 = sig
    type ('a, 'cmp) t
    type 'a elm
    val to_array: ('a, 'cmp) t -> 'a elm outer
  end
  module type SPoly3 = sig
    type ('k, 'v, 'cmp) t
    type 'k key
    type 'v value
    val to_array: ('k, 'v, 'cmp) t -> ('k key * 'v value) outer
  end

  module MakeMono (T : SeqIntf.IMonoDef) : SMono
    with type t := T.t
    with type elm := T.elm =
  struct
    let to_array t =
      match T.length t with
      | 0 -> [||]
      | l -> begin
          let rec fn t a i = begin
            match i = l with
            | true -> a
            | false -> begin
                let elm, t' = T.next t in
                let () = Stdlib.Array.set a i elm in
                let i' = Uns.succ i in
                fn t' a i'
              end
          end in
          let elm0, t' = T.next t in
          let a = Stdlib.Array.make l elm0 in
          fn t' a 1
        end
  end

  module MakeMonoRev (T : SeqIntf.IMonoDef) : SMono
    with type t := T.t
    with type elm := T.elm = struct
    let to_array t =
      match T.length t with
      | 0 -> [||]
      | l -> begin
          let rec fn t a i = begin
            let elm, t' = T.next t in
            let () = Stdlib.Array.set a i elm in
            match i with
            | 0 -> a
            | _ -> begin
                let i' = Uns.pred i in
                fn t' a i'
              end
          end in
          let elm, t' = T.next t in
          let a = Stdlib.Array.make l elm in
          fn t' a (Uns.pred l)
        end
  end

  module MakePoly (T : SeqIntf.IPolyDef) : SPoly
    with type 'a t := 'a T.t
    with type 'a elm := 'a T.elm = struct
    let to_array t =
      match T.length t with
      | 0 -> [||]
      | l -> begin
          let rec fn t a i = begin
            match i = l with
            | true -> a
            | false -> begin
                let elm, t' = T.next t in
                let () = Stdlib.Array.set a i elm in
                let i' = Uns.succ i in
                fn t' a i'
              end
          end in
          let elm0, t' = T.next t in
          let a = Stdlib.Array.make l elm0 in
          fn t' a 1
        end
  end

  module MakePolyRev (T : SeqIntf.IPolyDef) : SPoly
    with type 'a t := 'a T.t
    with type 'a elm := 'a T.elm = struct
    let to_array t =
      match T.length t with
      | 0 -> [||]
      | l -> begin
          let rec fn t a i = begin
            let elm, t' = T.next t in
            let () = Stdlib.Array.set a i elm in
            match i with
            | 0 -> a
            | _ -> begin
                let i' = Uns.pred i in
                fn t' a i'
              end
          end in
          let elm, t' = T.next t in
          let a = Stdlib.Array.make l elm in
          match l with
          | 1 -> a
          | _ -> fn t' a Uns.(l - 2)
        end
  end

  module MakePoly2 (T : SeqIntf.IPoly2Def) : SPoly2
    with type ('a, 'cmp) t := ('a, 'cmp) T.t
    with type 'a elm := 'a T.elm = struct
    let to_array t =
      match T.length t with
      | 0 -> [||]
      | l -> begin
          let rec fn t a i = begin
            match i = l with
            | true -> a
            | false -> begin
                let elm, t' = T.next t in
                let () = Stdlib.Array.set a i elm in
                let i' = Uns.succ i in
                fn t' a i'
              end
          end in
          let elm0, t' = T.next t in
          let a = Stdlib.Array.make l elm0 in
          fn t' a 1
        end
  end

  module MakePoly3 (T : SeqIntf.IPoly3Def) : SPoly3
    with type ('k, 'v, 'cmp) t := ('k, 'v, 'cmp) T.t
    with type 'k key := 'k T.key
    with type 'k value := 'k T.value = struct
    let to_array t =
      match T.length t with
      | 0 -> [||]
      | l -> begin
          let rec fn t a i = begin
            match i = l with
            | true -> a
            | false -> begin
                let kv, t' = T.next t in
                let () = Stdlib.Array.set a i kv in
                let i' = Uns.succ i in
                fn t' a i'
              end
          end in
          let kv0, t' = T.next t in
          let a = Stdlib.Array.make l kv0 in
          fn t' a 1
        end
  end

  (* Special-purpose, for fold[i]_map . *)
  module MakePoly3FoldMap (T : sig
      type ('a, 'accum, 'b) t
      val length: ('a,'accum,'b) t -> uns
      val next: ('a,'accum,'b) t -> 'accum -> 'b * ('a,'accum,'b) t * 'accum
    end) : sig
    type ('a, 'accum, 'b) t
    val to_accum_array: ('a,'accum,'b) t -> init:'accum -> 'accum * 'b outer
  end with type ('a,'accum,'b) t := ('a,'accum,'b) T.t = struct
    let to_accum_array t ~init =
      match T.length t with
      | 0 -> init, [||]
      | l -> begin
          let rec fn t accum a i = begin
            match i = l with
            | true -> accum, a
            | false -> begin
                let elm, t', accum' = T.next t accum in
                let () = Stdlib.Array.set a i elm in
                let i' = Uns.succ i in
                fn t' accum' a i'
              end
          end in
          let elm0, t', accum = T.next t init in
          let a = Stdlib.Array.make l elm0 in
          fn t' accum a 1
        end
  end

  (* Special-purpose, for fold[i]2_map . *)
  module MakePoly4Fold2Map (T : sig
      type ('a, 'b, 'accum, 'c) t
      val length: ('a,'b,'accum,'c) t -> uns
      val next: ('a,'b,'accum,'c) t -> 'accum -> 'c * ('a,'b,'accum,'c) t * 'accum
    end) : sig
    type ('a, 'b, 'accum, 'c) t
    val to_accum_array: ('a,'b,'accum,'c) t -> init:'accum -> 'accum * 'c outer
  end with type ('a,'b,'accum,'c) t := ('a,'b,'accum,'c) T.t = struct
    let to_accum_array t ~init =
      match T.length t with
      | 0 -> init, [||]
      | l -> begin
          let rec fn t accum a i = begin
            match i = l with
            | true -> accum, a
            | false -> begin
                let elm, t', accum' = T.next t accum in
                let () = Stdlib.Array.set a i elm in
                let i' = Uns.succ i in
                fn t' accum' a i'
              end
          end in
          let elm0, t', accum = T.next t init in
          let a = Stdlib.Array.make l elm0 in
          fn t' accum a 1
        end
  end
end

module Slice = struct
  include Slice.MakePoly(Cursor)
end

module ArrayInit = struct
  module T = struct
    type 'a t = {
      f: uns -> 'a;
      index: uns;
      length: uns;
    }
    type 'a elm = 'a

    let init length ~f =
      {f; index=0; length}

    let length t =
      t.length

    let next t =
      let elm = t.f t.index in
      let t' = {t with index=(succ t.index)} in
      elm, t'
  end
  include T
  include Seq.MakePoly(T)
end

let init n ~f =
  ArrayInit.(to_array (init n ~f))

module ArrayOfListCommon = struct
  type 'a t = {
    list: 'a list;
    length: uns;
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

module ArrayOfList = struct
  include ArrayOfListCommon
  include Seq.MakePoly(ArrayOfListCommon)
end

module ArrayOfListRev = struct
  include ArrayOfListCommon
  include Seq.MakePolyRev(ArrayOfListCommon)
end

let of_list ?length list =
  let length = match length with
    | None -> List.length list
    | Some length -> length
  in
  ArrayOfList.to_array (ArrayOfList.init length list)

let of_list_rev ?length list =
  let length = match length with
    | None -> List.length list
    | Some length -> length
  in
  ArrayOfListRev.to_array (ArrayOfListRev.init length list)

module ArrayOfStreamCommon = struct
  type 'a t = {
    stream: 'a Stream.t;
    length: uns;
  }
  type 'a elm = 'a

  let init length stream =
    {stream; length}

  let length t =
    t.length

  let next t =
    match t.stream with
    | lazy Stream.Nil -> not_reached ()
    | lazy (Stream.Cons(elm, stream')) -> begin
        let t' = {t with stream=stream'} in
        elm, t'
      end
end

module ArrayOfStream = struct
  include ArrayOfStreamCommon
  include Seq.MakePoly(ArrayOfStreamCommon)
end

module ArrayOfStreamRev = struct
  include ArrayOfStreamCommon
  include Seq.MakePolyRev(ArrayOfStreamCommon)
end

let of_stream ?length stream =
  let length = match length with
    | None -> Stream.length stream
    | Some length -> length
  in
  ArrayOfStream.to_array (ArrayOfStream.init length stream)

let of_stream_rev ?length stream =
  let length = match length with
    | None -> Stream.length stream
    | Some length -> length
  in
  ArrayOfStreamRev.to_array (ArrayOfStreamRev.init length stream)

let is_empty t =
  (length t) = 0

let set_inplace i elm t =
  Stdlib.Array.set t i elm

let copy t =
  init (length t) ~f:(fun i -> get i t)

let pare ~base ~past t =
  assert (base <= past);
  init (past - base) ~f:(fun i -> get (base + i) t)

let set i elm t =
  let t' = copy t in
  set_inplace i elm t';
  t'

module ArrayJoin = struct
  module T = struct
    type 'a outer = 'a t
    type 'a t = {
      sep: 'a outer;
      arrays: 'a outer list;
      length: uns;
      in_sep: bool;
      index: uns;
    }
    type 'a elm = 'a

    let init length sep arrays =
      {sep; arrays; length; in_sep=false; index=0}

    let next t =
      let rec fn t = begin
        match t.in_sep with
        | false -> begin
            let array = List.hd t.arrays in
            match t.index < length array with
            | true -> begin
                let elm = get t.index array in
                let t' = {t with index=succ t.index} in
                elm, t'
              end
            | false -> fn {t with arrays=List.tl t.arrays; in_sep=true; index=0}
          end
        | true -> begin
            match t.index < length t.sep with
            | true -> begin
                let elm = get t.index t.sep in
                let t' = {t with index=succ t.index} in
                elm, t'
              end
            | false -> fn {t with in_sep=false; index=0}
          end
      end in
      fn t

    let length t =
      t.length
  end
  include T
  include Seq.MakePoly(T)
end

let join ?sep tlist =
  let sep, sep_len = match sep with
    | None -> [||], 0
    | Some sep -> sep, length sep
  in
  let _, tlist_length = List.fold tlist ~init:(0, 0) ~f:(fun (i, accum) list ->
      let i' = Uns.succ i in
      let sep_len' = match i with
        | 0 -> 0
        | _ -> sep_len
      in
      let accum' = accum + sep_len' + (length list) in
      i', accum'
    ) in
  ArrayJoin.(to_array (init tlist_length sep tlist))

let concat t0 t1 =
  let length_t0 = length t0 in
  let length_t1 = length t1 in
  let length = length_t0 + length_t1 in
  init length ~f:(fun i ->
    if i < length_t0 then get i t0
    else get (i - length_t0) t1
  )

let append elm t =
  let tlen = length t in
  init (succ tlen) ~f:(fun i ->
    if i < tlen then get i t
    else elm
  )

let prepend elm t =
  init (succ (length t)) ~f:(fun i ->
    if i = 0 then elm
    else get (pred i) t
  )

let insert i elm t =
  if i = 0 then
    prepend elm t
  else begin
    let len = length t in
    if i < len then
      init (succ len) ~f:(fun index ->
        match Uns.cmp index i with
        | Lt -> get index t
        | Eq -> elm
        | Gt -> get (pred index) t
      )
    else
      append elm t
  end

let remove i t =
  let len = length t in
  assert (len > 0);
  match len with
  | 1 ->
    assert (i = 0);
    [||]
  | _ -> begin
      if i = 0 then
        pare ~base:1 ~past:len t
      else if i < len then
        init (pred len) ~f:(fun index ->
          if index < i then get index t
          else get (succ index) t
        )
      else
        pare ~base:0 ~past:(Uns.pred len) t
    end

let reduce ~f t =
  let rec fn i accum = begin
    match (i = (length t)) with
    | true -> accum
    | false -> fn (Uns.succ i) (f accum (get i t))
  end in
  match t with
  | [||] -> None
  | _ -> Some (fn 1 (get 0 t))

let reduce_hlt ~f t =
  match reduce ~f t with
  | None -> halt "Empty array"
  | Some x -> x

let swap_inplace i0 i1 t =
  let elm0 = get i0 t in
  let elm1 = get i1 t in
  set_inplace i0 elm1 t;
  set_inplace i1 elm0 t

let swap i0 i1 t =
  let t' = copy t in
  swap_inplace i0 i1 t';
  t'

let rev_inplace t =
  let rec fn i0 i1 = begin
    match (i0 >= i1) with
    | true -> ()
    | false -> begin
        swap_inplace i0 i1 t;
        fn (Uns.succ i0) (Uns.pred i1)
      end
  end in
  let len = length t in
  match len with
  | 0 -> ()
  | _ -> fn 0 (pred len)

let rev t =
  let l = length t in
  init l ~f:(fun i -> get (l - i - 1) t)

(* Used directly for non-overlapping blits. *)
let blit_ascending len i0 t0 i1 t1 =
  for i = 0 to pred len do
    set_inplace (i1 + i) (get (i0 + i) t0) t1
  done
let blit_descending len i0 t0 i1 t1 =
  for i = pred len downto 0 do
    set_inplace (i1 + i) (get (i0 + i) t0) t1
  done

let blit len i0 t0 i1 t1 =
  (* Choose direction such that overlapping ranges don't corrupt the source. *)
  match i0 < i1 with
  | true -> blit_descending len i0 t0 i1 t1
  | false -> blit_ascending len i0 t0 i1 t1

let is_sorted ?(strict=false) ~cmp t =
  let len = length t in
  let rec fn elm i = begin
    match i = len with
    | true -> true
    | false -> begin
        let elm' = get i t in
        match (cmp elm elm'), strict with
        | Cmp.Lt, _
        | Cmp.Eq, false -> fn elm' (Uns.succ i)
        | Cmp.Eq, true
        | Cmp.Gt, _ -> false
      end
  end in
  match len with
  | 0 -> true
  | _ -> fn (get 0 t) 1

type order =
  | Increasing
  | Decreasing
  | Either
type run = {
  base: uns;
  past: uns;
}
let sort_impl ?(stable=false) ~cmp ~inplace t =
  (* Merge a pair of adjacent runs. Input runs may be in increasing or decreasing order; the output
   * is always in increasing order. *)
  let merge_pair ~cmp src run0 order0 run1 order1 dst = begin
    assert (run0.past = run1.base);
    let rblit len i0 t0 i1 t1 = begin
      for i = 0 to pred len do
        set_inplace (i1 + i) (get (i0 + len - (i + 1)) t0) t1
      done
    end in
    let merge_pair_oo ~cmp src run0 order0 run1 order1 dst = begin
      let rec fn ~cmp src run0 order0 run1 order1 dst run = begin
        match (run0.base = run0.past), (run1.base = run1.past) with
        | false, false -> begin
            let elm0 = match order0 with
              | Increasing -> get run0.base src
              | Decreasing -> get (Uns.pred run0.past) src
              | Either -> not_reached ()
            in
            let elm1 = match order1 with
              | Increasing -> get run1.base src
              | Decreasing -> get (Uns.pred run1.past) src
              | Either -> not_reached ()
            in
            match cmp elm0 elm1 with
            | Cmp.Lt
            | Cmp.Eq -> begin
                match order0 with
                | Increasing -> begin
                    set_inplace run.past elm0 dst;
                    let run0' = {run0 with base=Uns.succ run0.base} in
                    let run' = {run with past=Uns.succ run.past} in
                    fn ~cmp src run0' order0 run1 order1 dst run'
                  end
                | Decreasing -> begin
                    set_inplace run.past elm0 dst;
                    let run0' = {run0 with past=Uns.pred run0.past} in
                    let run' = {run with past=Uns.succ run.past} in
                    fn ~cmp src run0' order0 run1 order1 dst run'
                  end
                | Either -> not_reached ()
              end
            | Cmp.Gt -> begin
                match order1 with
                | Increasing -> begin
                    set_inplace run.past elm1 dst;
                    let run1' = {run1 with base=Uns.succ run1.base} in
                    let run' = {run with past=Uns.succ run.past} in
                    fn ~cmp src run0 order0 run1' order1 dst run'
                  end
                | Decreasing -> begin
                    set_inplace run.past elm1 dst;
                    let run1' = {run1 with past=Uns.pred run1.past} in
                    let run' = {run with past=Uns.succ run.past} in
                    fn ~cmp src run0 order0 run1' order1 dst run'
                  end
                | Either -> not_reached ()
              end
          end
        | true, false -> begin
            let len = run1.past - run1.base in
            let () = match order1 with
              | Increasing -> blit_ascending len run1.base src run.past dst;
              | Decreasing -> rblit len run1.base src run.past dst;
              | Either -> not_reached ()
            in
            {run with past=run.past + len}
          end
        | false, true -> begin
            let len = run0.past - run0.base in
            let () = match order0 with
              | Increasing -> blit_ascending len run0.base src run.past dst;
              | Decreasing -> rblit len run0.base src run.past dst;
              | Either -> not_reached ()
            in
            {run with past=run.past + len}
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
      match i = (length src) with
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
          let elm' = get i src in
          let i' = Uns.succ i in
          match cmp elm elm', order, stable with
          | Cmp.Lt, Either, _ ->
            select ~stable ~cmp elm' base i' Increasing run0_opt order0 src dst runs
          | Cmp.Gt, Either, _ ->
            select ~stable ~cmp elm' base i' Decreasing run0_opt order0 src dst runs
          | Cmp.Lt, Increasing, _
          | Cmp.Eq, Increasing, true
          | Cmp.Eq, Either, true ->
            select ~stable ~cmp elm' base i' Increasing run0_opt order0 src dst runs
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
                    let run = merge_pair ~cmp src run0 order0 run1 order dst in
                    None, Either, (run :: runs)
                  end
              in
              select ~stable ~cmp elm' i i' Either run0_opt' order0' src dst
                runs'
            end
        end
    end in
  let aux = copy t in
  let runs = match t with
    | [||] -> [{base=0; past=0}]
    | _ -> select ~stable ~cmp (get 0 t) 0 1
      Either None Either t aux []
  in

  (* Repeatedly sweep through runs and merge pairs until only one run remains. Take care to
   * read/write linearly up/down based on the sweep direction, in order to improve cache locality.
  *)
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
        blit_descending (run.past - run.base) run.base src run.base dst;
        merge ~cmp ~inplace dst (run :: merged) (not up) src []
      end
    | run :: [], true, _ :: _ -> begin
        (* Copy odd run to dst, then start another sweep. *)
        blit_ascending (run.past - run.base) run.base src run.base dst;
        merge ~cmp ~inplace dst (run :: merged) (not up) src []
      end
    | [], true, _ :: []
    | _ :: [], true, [] -> begin
        match inplace with
        | true -> begin
            (* Odd number of sweeps performed; copy result back into t. *)
            blit_ascending (length dst) 0 dst 0 src;
            src
          end
        | false -> dst
      end
    | [], false, _ :: [] -> dst
    | [], _, [] -> not_reached ()
  end in
  merge ~cmp ~inplace aux runs false t []

let sort_inplace ?stable ~cmp t =
  let _ = sort_impl ?stable ~cmp ~inplace:true t in
  ()

let sort ?stable ~cmp t =
  sort_impl ?stable ~cmp ~inplace:false (copy t)

let search_impl ?base ?past key ~cmp mode t =
  let base = match base with
    | None -> 0
    | Some base -> base
  in
  let past = match past with
    | None -> length t
    | Some past -> past
  in
  assert (base <= past);
  assert (past <= length t);
  let rec fn ~base ~past = begin
    assert (base <= past);
    match (base = past) with
    | true -> begin
        (* Empty range. *)
        match mode with
        | Cmp.Lt -> begin
            match (base = 0), (base = (length t)) with
            | true, true -> None (* Empty; key < elms. *)
            | _, false -> begin (* At beginning, or interior. *)
                match cmp key (get base t) with
                | Cmp.Lt -> begin (* At successor. *)
                    match base = 0 with
                    | true -> Some (Cmp.Lt, 0) (* At beginning; key < elms. *)
                    | false -> Some (Cmp.Gt, (Uns.pred base)) (* In interior; key > predecessor. *)
                  end
                | Cmp.Eq -> Some (Cmp.Eq, base) (* base at leftmost match. *)
                | Cmp.Gt -> not_reached ()
              end
            | false, true -> Some (Cmp.Gt, (Uns.pred base)) (* Past end; key > elms. *)
          end
        | Cmp.Eq -> None (* No match. *)
        | Cmp.Gt -> begin
            match (base = 0), (base = (length t)) with
            | true, true -> None (* Empty; key > elms. *)
            | true, false ->
              Some (Cmp.Lt, 0) (* At beginning; key < elms. *)
            | false, _ -> begin (* In interior, or past end. *)
                let probe = Uns.pred base in
                match cmp key (get probe t) with
                | Cmp.Lt -> not_reached ()
                | Cmp.Eq -> Some (Cmp.Eq, probe) (* probe at rightmost match. *)
                | Cmp.Gt -> begin
                    match (base = (length t)) with
                    | false -> Some (Cmp.Lt, base) (* In interior; key < successor. *)
                    | true -> Some (Cmp.Gt, probe) (* Past end; key > elms. *)
                  end
              end
          end
      end
    | false -> begin
        let mid = (base + past) / 2 in
        match (cmp key (get mid t)), mode with
        | Cmp.Lt, _
        | Cmp.Eq, Cmp.Lt -> fn ~base ~past:mid
        | Cmp.Eq, Cmp.Eq -> Some (Cmp.Eq, mid)
        | Cmp.Eq, Cmp.Gt
        | Cmp.Gt, _ -> fn ~base:(Uns.succ mid) ~past
      end
  end in
  fn ~base ~past

let psearch ?base ?past key ~cmp t =
  search_impl ?base ?past key ~cmp Cmp.Lt t

let search ?base ?past key ~cmp t =
  match search_impl ?base ?past key ~cmp Cmp.Eq t with
  | Some (_, i) -> Some i
  | _ -> None

let nsearch ?base ?past key ~cmp t =
  search_impl ?base ?past key ~cmp Cmp.Gt t

let map ~f t =
  init (length t) ~f:(fun i -> f (get i t))

let mapi ~f t =
  init (length t) ~f:(fun i -> f i (get i t))

module ArrayFoldiMap = struct
  module T = struct
    type 'a outer = 'a t
    type ('a, 'accum, 'b) t = {
      arr: 'a outer;
      f: uns -> 'accum -> 'a -> 'accum * 'b;
      index: uns;
      length: uns;
    }

    let init arr ~f length =
      {arr; f; length; index=0}

    let length t =
      t.length

    let next t accum =
      let accum', elm' = t.f t.index accum (get t.index t.arr) in
      let t' = {t with index=Uns.succ t.index} in
      elm', t', accum'
  end
  include T
  include Seq.MakePoly3FoldMap(T)
end

let fold_map ~init ~f t =
  ArrayFoldiMap.to_accum_array (ArrayFoldiMap.init t ~f:(fun _ accum elm -> f accum elm) (length t))
    ~init

let foldi_map ~init ~f t =
  ArrayFoldiMap.to_accum_array (ArrayFoldiMap.init t ~f (length t)) ~init

let filter ~f t =
  let _, t' = ArrayFoldiMap.to_accum_array
      (ArrayFoldiMap.init t ~f:(fun _ i _ ->
          let rec fn i elm = begin
            let i' = Uns.succ i in
            match f elm with
            | true -> i', elm
            | false -> fn i' (get i' t)
          end in
          fn i (get i t)
        ) (count ~f t)) ~init:0 in
  t'

let filteri ~f t =
  let n = foldi t ~init:0 ~f:(fun i accum elm ->
    match f i elm with
    | false -> accum
    | true -> Uns.succ accum
  ) in
  let _, t' = ArrayFoldiMap.to_accum_array
      (ArrayFoldiMap.init t ~f:(fun _ i _ ->
          let rec fn i elm = begin
            let i' = Uns.succ i in
            match f i elm with
            | true -> i', elm
            | false -> fn i' (get i' t)
          end in
          fn i (get i t)
        ) n)
      ~init:0 in
  t'

let foldi2_until ~init ~f t0 t1 =
  assert ((length t0) = (length t1));
  let len = length t0 in
  let rec fn i accum = begin
    match i = len with
    | true -> accum
    | false -> begin
        let accum', until = f i accum (get i t0) (get i t1) in
        match until with
        | true -> accum'
        | false -> fn (Uns.succ i) accum'
      end
  end in
  fn 0 init

let fold2_until ~init ~f t0 t1 =
  foldi2_until ~init ~f:(fun _ accum a b -> f accum a b) t0 t1

let fold2 ~init ~f t0 t1 =
  fold2_until ~init ~f:(fun accum a b -> (f accum a b), false) t0 t1

let foldi2 ~init ~f t0 t1 =
  foldi2_until ~init ~f:(fun i accum a b -> (f i accum a b), false) t0 t1

let iter2 ~f t0 t1 =
  assert ((length t0) = (length t1));
  for i = 0 to pred (length t0) do
    f (get i t0) (get i t1)
  done

let iteri2 ~f t0 t1 =
  assert ((length t0) = (length t1));
  for i = 0 to pred (length t0) do
    f i (get i t0) (get i t1)
  done

let map2 ~f t0 t1 =
  assert ((length t0) = (length t1));
  init (length t0) ~f:(fun i -> f (get i t0) (get i t1))

let mapi2 ~f t0 t1 =
  assert ((length t0) = (length t1));
  init (length t0) ~f:(fun i -> f i (get i t0) (get i t1))

module ArrayFoldi2Map = struct
  module T = struct
    type 'a outer = 'a t
    type ('a, 'b, 'accum, 'c) t = {
      arr0: 'a outer;
      arr1: 'b outer;
      f: uns -> 'accum -> 'a -> 'b -> 'accum * 'c;
      index: uns;
    }

    let init arr0 arr1 ~f =
      {arr0; arr1; f; index=0}

    let length t =
      length t.arr0

    let next t accum =
      let accum', elm' = t.f t.index accum (get t.index t.arr0) (get t.index t.arr1) in
      let t' = {t with index=Uns.succ t.index} in
      elm', t', accum'
  end
  include T
  include Seq.MakePoly4Fold2Map(T)
end

let fold2_map ~init ~f t0 t1 =
  ArrayFoldi2Map.to_accum_array (ArrayFoldi2Map.init t0 t1 ~f:(fun _ accum elm -> f accum elm))
    ~init

let foldi2_map ~init ~f t0 t1 =
  ArrayFoldi2Map.to_accum_array (ArrayFoldi2Map.init t0 t1 ~f) ~init

let zip t0 t1 =
  map2 ~f:(fun a b -> a, b) t0 t1

let unzip t =
  let t0 = map ~f:(fun (a, _) -> a) t in
  let t1 = map ~f:(fun (_, b) -> b) t in
  t0, t1

let pp pp_elm ppf t =
  let open Format in
  fprintf ppf "@[<h>[|";
  iteri t ~f:(fun i elm ->
    if i > 0 then fprintf ppf ";@ ";
    fprintf ppf "%a" pp_elm elm
  );
  fprintf ppf "|]@]"

(******************************************************************************)
(* Begin tests. *)

let%expect_test "cursor" =
  let open Format in
  let rec fn arr hd cursor tl = begin
    let index = Cursor.index cursor in
    printf "index=%a" Uns.pp index;
    printf ", container %a arr"
      Cmp.pp (cmp Uns.cmp (Cursor.container cursor) arr);
    let hd_cursor = Cursor.cmp hd cursor in
    printf ", hd %a cursor" Cmp.pp hd_cursor;
    let cursor_tl = Cursor.cmp cursor tl in
    printf ", cursor %a tl" Cmp.pp cursor_tl;
    let () = match hd_cursor with
      | Lt -> printf ", lget=%a" Uns.pp (Cursor.lget cursor)
      | Eq -> printf ", lget=_"
      | Gt -> not_reached ()
    in
    let () = match cursor_tl with
      | Lt -> printf ", rget=%a" Uns.pp (Cursor.rget cursor)
      | Eq -> printf ", rget=_"
      | Gt -> not_reached ()
    in
    printf "\n";

    let length = length arr in
    assert (Cursor.(=)
        (Cursor.seek (Uns.to_sint index) hd)
        cursor);
    assert (Cursor.(=)
        hd
        (Cursor.seek (Sint.neg (Uns.to_sint index)) cursor)
    );
    assert (Cursor.(=)
        (Cursor.seek (Uns.to_sint (length - index)) cursor)
        tl
    );
    assert (Cursor.(=)
        cursor
        (Cursor.seek (Sint.neg (Uns.to_sint (length - index))) tl)
    );

    match cursor_tl with
    | Lt -> begin
        let cursor' = Cursor.succ cursor in
        assert Cursor.(cursor = (pred cursor'));
        fn arr hd cursor' tl
      end
    | Eq | Gt -> ()
  end in
  let arrs = [
    [||];
    [|0|];
    [|0; 1|];
    [|0; 1; 2|];
  ] in
  printf "@[<h>";
  List.iter arrs ~f:(fun arr ->
    printf "--- %a ---\n" (pp Uns.pp) arr;
    let hd = Cursor.hd arr in
    fn arr hd hd (Cursor.tl arr)
  );
  printf "@]";

  [%expect{|
    --- [||] ---
    index=0, container Eq arr, hd Eq cursor, cursor Eq tl, lget=_, rget=_
    --- [|0|] ---
    index=0, container Eq arr, hd Eq cursor, cursor Lt tl, lget=_, rget=0
    index=1, container Eq arr, hd Lt cursor, cursor Eq tl, lget=0, rget=_
    --- [|0; 1|] ---
    index=0, container Eq arr, hd Eq cursor, cursor Lt tl, lget=_, rget=0
    index=1, container Eq arr, hd Lt cursor, cursor Lt tl, lget=0, rget=1
    index=2, container Eq arr, hd Lt cursor, cursor Eq tl, lget=1, rget=_
    --- [|0; 1; 2|] ---
    index=0, container Eq arr, hd Eq cursor, cursor Lt tl, lget=_, rget=0
    index=1, container Eq arr, hd Lt cursor, cursor Lt tl, lget=0, rget=1
    index=2, container Eq arr, hd Lt cursor, cursor Lt tl, lget=1, rget=2
    index=3, container Eq arr, hd Lt cursor, cursor Eq tl, lget=2, rget=_
    |}]

let%expect_test "hash_fold" =
  let open Format in
  printf "@[<h>";
  let rec fn arrs = begin
    match arrs with
    | [] -> ()
    | arr :: arrs' -> begin
        printf "hash_fold %a -> %a\n"
          (pp Uns.pp) arr
          Hash.pp (Hash.t_of_state
            (hash_fold Uns.hash_fold arr Hash.State.empty));
        fn arrs'
      end
  end in
  let arrs = [
    [||];
    [|0|];
    [|0; 0|];
    [|0; 1|]
  ] in
  fn arrs;
  printf "@]";

  [%expect{|
    hash_fold [||] -> 0xf255_7dfc_c4e8_fe52_28df_63b7_cc57_c3cbu128
    hash_fold [|0|] -> 0x5813_89d3_d926_9f3c_ffb4_92a6_b118_4dfcu128
    hash_fold [|0; 0|] -> 0x322f_9602_7919_2b02_9116_8e4e_6e9c_67b0u128
    hash_fold [|0; 1|] -> 0xa627_52f5_ea2f_d3c0_39a9_536b_6256_1d94u128
    |}]

let%expect_test "hash_fold empty" =
  let hash_empty state = begin
    state
    |> hash_fold Unit.hash_fold [||]
  end in
  let e1 =
    Hash.State.empty
    |> hash_empty
  in
  let e2 =
    Hash.State.empty
    |> hash_empty
    |> hash_empty
  in
  assert Sint.((u128_compare (Hash.t_of_state e1) (Hash.t_of_state e2)) <>
      kv 0);

  [%expect{|
    |}]

let%expect_test "cmp" =
  let open Format in
  let arrs = [
    [||];
    [|0|];
    [|0; 0|];
    [|0; 1|];
    [|0; 0|];
    [|0|];
    [||];
  ] in
  let rec fn arr arrs = begin
    match arrs with
    | [] -> ()
    | hd :: tl -> begin
        let () = List.iter arrs ~f:(fun arr2 ->
          printf "cmp %a %a -> %a\n"
            (pp Uns.pp) arr
            (pp Uns.pp) arr2
            Cmp.pp (cmp Uns.cmp arr arr2)
        ) in
        fn hd tl
      end
  end in
  let hd, tl = match arrs with
    | hd :: tl -> hd, tl
    | [] -> not_reached ()
  in
  printf "@[<h>";
  fn hd tl;
  printf "@]";

  [%expect{|
    cmp [||] [|0|] -> Lt
    cmp [||] [|0; 0|] -> Lt
    cmp [||] [|0; 1|] -> Lt
    cmp [||] [|0; 0|] -> Lt
    cmp [||] [|0|] -> Lt
    cmp [||] [||] -> Eq
    cmp [|0|] [|0; 0|] -> Lt
    cmp [|0|] [|0; 1|] -> Lt
    cmp [|0|] [|0; 0|] -> Lt
    cmp [|0|] [|0|] -> Eq
    cmp [|0|] [||] -> Gt
    cmp [|0; 0|] [|0; 1|] -> Lt
    cmp [|0; 0|] [|0; 0|] -> Eq
    cmp [|0; 0|] [|0|] -> Gt
    cmp [|0; 0|] [||] -> Gt
    cmp [|0; 1|] [|0; 0|] -> Gt
    cmp [|0; 1|] [|0|] -> Gt
    cmp [|0; 1|] [||] -> Gt
    cmp [|0; 0|] [|0|] -> Gt
    cmp [|0; 0|] [||] -> Gt
    cmp [|0|] [||] -> Gt
    |}]

let%expect_test "of_list,of_list_rev" =
  let open Format in
  printf "@[<h>";
  let test list = begin
    printf "of_list[_rev] %a -> %a / %a\n"
      (List.pp Uns.pp) list
      (pp Uns.pp) (of_list list)
      (pp Uns.pp) (of_list_rev list);
  end in
  let lists = [
    [];
    [0];
    [0; 1];
    [0; 1; 2];
    [0; 1; 2; 3];
  ] in
  List.iter lists ~f:test;
  printf "@]";

  [%expect{|
    of_list[_rev] [] -> [||] / [||]
    of_list[_rev] [0] -> [|0|] / [|0|]
    of_list[_rev] [0; 1] -> [|0; 1|] / [|1; 0|]
    of_list[_rev] [0; 1; 2] -> [|0; 1; 2|] / [|2; 1; 0|]
    of_list[_rev] [0; 1; 2; 3] -> [|0; 1; 2; 3|] / [|3; 2; 1; 0|]
    |}]

let%expect_test "of_stream,of_stream_rev" =
  let open Format in
  printf "@[<h>";
  let test stream = begin
    printf "of_stream[_rev] %a -> %a / %a\n"
      (Stream.pp Uns.pp) stream
      (pp Uns.pp) (of_stream stream)
      (pp Uns.pp) (of_stream_rev stream);
  end in
  let f list = begin
    match list with
    | [] -> None
    | hd :: tl -> Some (hd, tl)
  end in
  let streams = List.map ~f:(Stream.init_indef ~f) [
    [];
    [0];
    [0; 1];
    [0; 1; 2];
    [0; 1; 2; 3];
  ] in
  List.iter streams ~f:test;
  printf "@]";

  [%expect{|
    of_stream[_rev] Nil -> [||] / [||]
    of_stream[_rev] (0 Nil) -> [|0|] / [|0|]
    of_stream[_rev] (0 (1 Nil)) -> [|0; 1|] / [|1; 0|]
    of_stream[_rev] (0 (1 (2 Nil))) -> [|0; 1; 2|] / [|2; 1; 0|]
    of_stream[_rev] (0 (1 (2 (3 Nil)))) -> [|0; 1; 2; 3|] / [|3; 2; 1; 0|]
    |}]

let%expect_test "get,length,is_empty" =
  let open Format in
  let test_length arr = begin
    printf "%a: length=%a, is_empty=%B\n"
      (pp Uns.pp) arr
      Uns.pp (length arr)
      (is_empty arr)
  end in
  printf "@[<h>";
  test_length [||];
  test_length [|0|];
  test_length [|0; 1|];
  test_length [|0; 1; 2|];
  printf "@]";

  [%expect{|
    [||]: length=0, is_empty=true
    [|0|]: length=1, is_empty=false
    [|0; 1|]: length=2, is_empty=false
    [|0; 1; 2|]: length=3, is_empty=false
    |}]

let%expect_test "set,set_inplace" =
  let open Format in
  let test_set len = begin
    for i = 0 to pred len do
      let arr = init len ~f:(fun _ -> 0) in
      let arr' = set i 1 arr in
      printf "set %a: %a -> %a"
        Uns.pp i
        (pp Uns.pp) arr
        (pp Uns.pp) arr'
      ;
      set_inplace i 1 arr;
      printf " -> set_inplace: %a" (pp Uns.pp) arr;
      let arr'' = copy arr in
      printf " -> copy,set_inplace: %a" (pp Uns.pp) arr'';
      set_inplace i 2 arr'';
      printf " -> %a\n"
        (pp Uns.pp) arr''
    done
  end in
  printf "@[<h>";
  test_set 1;
  test_set 2;
  test_set 3;
  printf "@]";

  [%expect{|
    set 0: [|0|] -> [|1|] -> set_inplace: [|1|] -> copy,set_inplace: [|1|] -> [|2|]
    set 0: [|0; 0|] -> [|1; 0|] -> set_inplace: [|1; 0|] -> copy,set_inplace: [|1; 0|] -> [|2; 0|]
    set 1: [|0; 0|] -> [|0; 1|] -> set_inplace: [|0; 1|] -> copy,set_inplace: [|0; 1|] -> [|0; 2|]
    set 0: [|0; 0; 0|] -> [|1; 0; 0|] -> set_inplace: [|1; 0; 0|] -> copy,set_inplace: [|1; 0; 0|] -> [|2; 0; 0|]
    set 1: [|0; 0; 0|] -> [|0; 1; 0|] -> set_inplace: [|0; 1; 0|] -> copy,set_inplace: [|0; 1; 0|] -> [|0; 2; 0|]
    set 2: [|0; 0; 0|] -> [|0; 0; 1|] -> set_inplace: [|0; 0; 1|] -> copy,set_inplace: [|0; 0; 1|] -> [|0; 0; 2|]
    |}]

let%expect_test "pare" =
  let open Format in
  let test arr = begin
    printf "pare %a ->" (pp Uns.pp) arr;
    for i = 0 to length arr do
      for j = i to length arr do
        let arr' = pare arr ~base:i ~past:j in
        printf " [%a,%a)=%a"
          Uns.pp i
          Uns.pp j
          (pp Uns.pp) arr'
      done
    done;
    printf "\n"
  end in
  printf "@[<h>";
  test [||];
  test [|0|];
  test [|0; 1|];
  test [|0; 1; 2|];
  printf "@]";

  [%expect{|
    pare [||] -> [0,0)=[||]
    pare [|0|] -> [0,0)=[||] [0,1)=[|0|] [1,1)=[||]
    pare [|0; 1|] -> [0,0)=[||] [0,1)=[|0|] [0,2)=[|0; 1|] [1,1)=[||] [1,2)=[|1|] [2,2)=[||]
    pare [|0; 1; 2|] -> [0,0)=[||] [0,1)=[|0|] [0,2)=[|0; 1|] [0,3)=[|0; 1; 2|] [1,1)=[||] [1,2)=[|1|] [1,3)=[|1; 2|] [2,2)=[||] [2,3)=[|2|] [3,3)=[||]
    |}]

let%expect_test "join" =
  let open Format in
  let test ?sep arrs = begin
    printf "join";
    let () = match sep with
      | None -> ()
      | Some sep -> printf " ~sep:%a" (pp Uns.pp) sep;
    in
    printf " %a -> %a\n"
      (List.pp (pp Uns.pp)) arrs
      (pp Uns.pp) (join ?sep arrs)
  end in
  printf "@[<h>";
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

  test ~sep:[|3|] [];
  test ~sep:[|3|] [[||]];
  test ~sep:[|3|] [[||]; [||]];
  test ~sep:[|3|] [[||]; [||]; [||]];

  test ~sep:[|3|] [[|0|]];

  test ~sep:[|3|] [[|0|]; [||]];
  test ~sep:[|3|] [[||]; [|0|]];
  test ~sep:[|3|] [[|0|]; [|1|]];

  test ~sep:[|3|] [[|0|]; [||]; [||]];
  test ~sep:[|3|] [[||]; [|0|]; [||]];
  test ~sep:[|3|] [[||]; [||]; [|0|]];
  test ~sep:[|3|] [[|0|]; [|1|]; [||]];
  test ~sep:[|3|] [[|0|]; [||]; [|1|]];
  test ~sep:[|3|] [[|0|]; [|1|]; [|2|]];
  printf "@]";

  [%expect{|
    join [] -> [||]
    join [[||]] -> [||]
    join [[||]; [||]] -> [||]
    join [[||]; [||]; [||]] -> [||]
    join [[|0|]] -> [|0|]
    join [[|0|]; [||]] -> [|0|]
    join [[||]; [|0|]] -> [|0|]
    join [[|0|]; [|1|]] -> [|0; 1|]
    join [[|0|]; [||]; [||]] -> [|0|]
    join [[||]; [|0|]; [||]] -> [|0|]
    join [[||]; [||]; [|0|]] -> [|0|]
    join [[|0|]; [|1|]; [||]] -> [|0; 1|]
    join [[|0|]; [||]; [|1|]] -> [|0; 1|]
    join [[|0|]; [|1|]; [|2|]] -> [|0; 1; 2|]
    join ~sep:[|3|] [] -> [||]
    join ~sep:[|3|] [[||]] -> [||]
    join ~sep:[|3|] [[||]; [||]] -> [|3|]
    join ~sep:[|3|] [[||]; [||]; [||]] -> [|3; 3|]
    join ~sep:[|3|] [[|0|]] -> [|0|]
    join ~sep:[|3|] [[|0|]; [||]] -> [|0; 3|]
    join ~sep:[|3|] [[||]; [|0|]] -> [|3; 0|]
    join ~sep:[|3|] [[|0|]; [|1|]] -> [|0; 3; 1|]
    join ~sep:[|3|] [[|0|]; [||]; [||]] -> [|0; 3; 3|]
    join ~sep:[|3|] [[||]; [|0|]; [||]] -> [|3; 0; 3|]
    join ~sep:[|3|] [[||]; [||]; [|0|]] -> [|3; 3; 0|]
    join ~sep:[|3|] [[|0|]; [|1|]; [||]] -> [|0; 3; 1; 3|]
    join ~sep:[|3|] [[|0|]; [||]; [|1|]] -> [|0; 3; 3; 1|]
    join ~sep:[|3|] [[|0|]; [|1|]; [|2|]] -> [|0; 3; 1; 3; 2|]
    |}]

let%expect_test "concat" =
  let open Format in
  let test arr0 arr1 = begin
    printf "concat %a %a -> %a\n"
      (pp Uns.pp) arr0
      (pp Uns.pp) arr1
      (pp Uns.pp) (concat arr0 arr1)
    ;
  end in
  printf "@[<h>";
  test [||] [||];
  test [|0|] [||];
  test [||] [|0|];
  test [|0|] [|1|];
  test [|0; 1|] [|2|];
  test [|0|] [|1; 2|];
  test [|0; 1|] [|2; 3|];
  printf "@]";

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
  let open Format in
  let test arr x = begin
    let arr_x = append x arr in
    let x_arr = prepend x arr in
    printf "%a %a: append -> %a, prepend -> %a\n"
      (pp Uns.pp) arr
      Uns.pp x
      (pp Uns.pp) arr_x
      (pp Uns.pp) x_arr
  end in
  printf "@[<h>";
  test [||] 0;
  test [|0|] 1;
  test [|0; 1|] 2;
  test [|0; 1; 2|] 3;
  printf "@]";

  [%expect{|
    [||] 0: append -> [|0|], prepend -> [|0|]
    [|0|] 1: append -> [|0; 1|], prepend -> [|1; 0|]
    [|0; 1|] 2: append -> [|0; 1; 2|], prepend -> [|2; 0; 1|]
    [|0; 1; 2|] 3: append -> [|0; 1; 2; 3|], prepend -> [|3; 0; 1; 2|]
    |}]

let%expect_test "insert" =
  let open Format in
  let test arr x = begin
    printf "insert %a %a ->"
      (pp Uns.pp) arr
      Uns.pp x
    ;
    for i = 0 to length arr do
      let arr' = insert i x arr in
      printf " %a" (pp Uns.pp) arr'
    done;
    printf "\n"
  end in
  printf "@[<h>";
  test [||] 0;
  test [|0|] 1;
  test [|0; 1|] 2;
  test [|0; 1; 2|] 3;
  printf "@]";

  [%expect{|
    insert [||] 0 -> [|0|]
    insert [|0|] 1 -> [|1; 0|] [|0; 1|]
    insert [|0; 1|] 2 -> [|2; 0; 1|] [|0; 2; 1|] [|0; 1; 2|]
    insert [|0; 1; 2|] 3 -> [|3; 0; 1; 2|] [|0; 3; 1; 2|] [|0; 1; 3; 2|] [|0; 1; 2; 3|]
    |}]

let%expect_test "remove" =
  let open Format in
  let test arr = begin
    printf "remove %a ->" (pp Uns.pp) arr;
    for i = 0 to pred (length arr) do
      let arr' = remove i arr in
      printf " %a" (pp Uns.pp) arr';
    done;
    printf "\n"
  end in
  printf "@[<h>";
  test [|0|];
  test [|0; 1|];
  test [|0; 1; 2|];
  printf "@]";

  [%expect{|
    remove [|0|] -> [||]
    remove [|0; 1|] -> [|1|] [|0|]
    remove [|0; 1; 2|] -> [|1; 2|] [|0; 2|] [|0; 1|]
    |}]

let%expect_test "reduce" =
  let open Format in
  let test_reduce arr ~f = begin
    printf "reduce %a" (pp Uns.pp) arr;
    match reduce arr ~f with
    | None -> printf " -> None\n"
    | Some x -> printf " -> %a\n" Uns.pp x
  end in
  let f a b = (a + b) in
  printf "@[<h>";
  test_reduce [||] ~f;
  test_reduce [|0; 1; 2; 3; 4|] ~f;
  printf "@]";

  [%expect{|
    reduce [||] -> None
    reduce [|0; 1; 2; 3; 4|] -> 10
    |}]

let%expect_test "swap,swap_inplace" =
  let open Format in
  let test_swap arr = begin
    for i = 0 to pred (length arr) do
      for j = i to pred (length arr) do
        let arr' = copy arr in
        printf "%a %a: swap %a -> %a -> swap_inplace %a -> "
          Uns.pp i
          Uns.pp j
          (pp Uns.pp) arr'
          (pp Uns.pp) (swap i j arr')
          (pp Uns.pp) arr'
        ;
        swap_inplace i j arr';
        printf "%a\n" (pp Uns.pp) arr'
      done
    done
  end in
  printf "@[<h>";
  test_swap [|0|];
  test_swap [|0; 1|];
  test_swap [|0; 1; 2|];
  test_swap [|0; 1; 2; 3|];
  printf "@]";

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
  let open Format in
  let test_rev arr = begin
    printf "rev %a -> %a -> rev_inplace %a -> "
      (pp Uns.pp) arr
      (pp Uns.pp) (rev arr)
      (pp Uns.pp) arr
    ;
    rev_inplace arr;
    printf "%a\n" (pp Uns.pp) arr
  end in
  printf "@[<h>";
  test_rev [|0|];
  test_rev [|0; 1|];
  test_rev [|0; 1; 2|];
  test_rev [|0; 1; 2; 3|];
  printf "@]";

  [%expect{|
    rev [|0|] -> [|0|] -> rev_inplace [|0|] -> [|0|]
    rev [|0; 1|] -> [|1; 0|] -> rev_inplace [|0; 1|] -> [|1; 0|]
    rev [|0; 1; 2|] -> [|2; 1; 0|] -> rev_inplace [|0; 1; 2|] -> [|2; 1; 0|]
    rev [|0; 1; 2; 3|] -> [|3; 2; 1; 0|] -> rev_inplace [|0; 1; 2; 3|] -> [|3; 2; 1; 0|]
    |}]

let%expect_test "blit" =
  let open Format in
  let test_blit len i0 arr0 i1 arr1 = begin
    printf "blit %a %a %a %a %a -> "
      Uns.pp len
      Uns.pp i0
      (pp Uns.pp) arr0
      Uns.pp i1
      (pp Uns.pp) arr1
    ;
    blit len i0 arr0 i1 arr1;
    printf "%a\n" (pp Uns.pp) arr1
  end in
  printf "@[<h>";
  test_blit 0 0 [||] 0 [||];
  test_blit 1 0 [|0|] 0 [|1|];
  test_blit 1 1 [|0; 1|] 0 [|2|];
  test_blit 1 0 [|0|] 1 [|1; 2|];
  test_blit 2 0 [|0; 1|] 0 [|2; 3|];
  test_blit 2 1 [|0; 1; 2|] 0 [|3; 4; 5|];
  test_blit 3 0 [|0; 1; 2|] 0 [|3; 4; 5|];
  printf "@]";

  [%expect{|
    blit 0 0 [||] 0 [||] -> [||]
    blit 1 0 [|0|] 0 [|1|] -> [|0|]
    blit 1 1 [|0; 1|] 0 [|2|] -> [|1|]
    blit 1 0 [|0|] 1 [|1; 2|] -> [|1; 0|]
    blit 2 0 [|0; 1|] 0 [|2; 3|] -> [|0; 1|]
    blit 2 1 [|0; 1; 2|] 0 [|3; 4; 5|] -> [|1; 2; 5|]
    blit 3 0 [|0; 1; 2|] 0 [|3; 4; 5|] -> [|0; 1; 2|]
    |}]

let%expect_test "is_sorted" =
  let open Format in
  let test_is_sorted arr = begin
    printf "is_sorted %a: not strict -> %B, strict -> %B\n"
      (pp Uns.pp) arr
      (is_sorted arr ~cmp:Uns.cmp)
      (is_sorted ~strict:true arr ~cmp:Uns.cmp)
  end in
  printf "@[<h>";
  test_is_sorted [||];
  test_is_sorted [|0|];
  test_is_sorted [|0; 0|];
  test_is_sorted [|0; 1|];
  test_is_sorted [|1; 0|];
  test_is_sorted [|0; 1; 1|];
  test_is_sorted [|0; 1; 2|];
  test_is_sorted [|0; 2; 1|];
  printf "@]";

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
  key: uns; (* Random, possibly non-unique. *)
  sn: uns; (* Sequential in initial array. *)
}
let%expect_test "sort" =
  let gen_array len = begin
    let key_limit =
      if len > 0 then len
      else 1
    in
    init len ~f:(fun i -> {key=Stdlib.Random.int key_limit; sn=i})
  end in
  let cmp elm0 elm1 =
    Uns.cmp elm0.key elm1.key
  in
  let test_sort arr = begin
    let arr' = sort arr ~cmp in
    assert (is_sorted arr' ~cmp);

    let arr' = sort ~stable:true arr ~cmp in
    assert (is_sorted ~strict:true arr' ~cmp:(fun elm0 elm1 ->
      match cmp elm0 elm1 with
      | Cmp.Lt -> Cmp.Lt
      | Cmp.Eq -> Uns.cmp elm0.sn elm1.sn
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
  let open Format in
  let test_search arr key_max = begin
    printf "%a\n" (pp Uns.pp) arr;
    for probe = 0 to key_max do
      printf "  %a -> %s, %s, %s\n" Uns.pp probe
        (match psearch probe ~cmp:Uns.cmp arr with
          | None -> "<"
          | Some (Cmp.Lt, i) -> asprintf "<[%a]=%a"
              Uns.pp i Uns.pp (get i arr)
          | Some (Cmp.Eq, i) -> asprintf "=[%a]=%a"
              Uns.pp i Uns.pp (get i arr)
          | Some (Cmp.Gt, i) -> asprintf ">[%a]=%a"
              Uns.pp i Uns.pp (get i arr)
        )
        (match search probe ~cmp:Uns.cmp arr with
          | None -> "<>"
          | Some i -> asprintf "=%a" Uns.pp (get i arr)
        )
        (match nsearch probe ~cmp:Uns.cmp arr with
          | Some (Cmp.Lt, i) -> asprintf "<[%a]=%a"
              Uns.pp i Uns.pp (get i arr)
          | Some (Cmp.Eq, i) -> asprintf "=[%a]=%a"
              Uns.pp i Uns.pp (get i arr)
          | Some (Cmp.Gt, i) -> asprintf ">[%a]=%a"
              Uns.pp i Uns.pp (get i arr)
          | None -> ">"
        );
    done
  end in
  printf "@[<h>";
  for len = 0 to 3 do
    let arr = init len ~f:(fun i -> i * 2 + 1) in
    let key_max = len * 2 in
    test_search arr key_max
  done;
  for hlen = 1 to 3 do
    let len = hlen * 2 in
    let arr = init len ~f:(fun i -> i + ((i + 1) % 2)) in
    let key_max = len in
    test_search arr key_max
  done;
  printf "@]";

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
  let open Format in
  let pp_str ppf t = Format.fprintf ppf "%S" t in
  let test_map uarr = begin
    printf "%a -> map " (pp Uns.pp) uarr;
    let sarr = map uarr ~f:(fun elm -> asprintf "%a" Uns.pp elm) in
    printf "%a" (pp pp_str) sarr;
    printf " -> mapi ";
    let sarr = mapi uarr ~f:(fun i elm ->
      asprintf "[%a]=%a" Uns.pp i Uns.pp elm
    ) in
    printf "%a\n" (pp pp_str) sarr
  end in
  printf "@[<h>";
  test_map [||];
  test_map [|0|];
  test_map [|1; 0|];
  test_map [|2; 1; 0|];
  printf "@]";

  [%expect{|
    [||] -> map [||] -> mapi [||]
    [|0|] -> map [|"0"|] -> mapi [|"[0]=0"|]
    [|1; 0|] -> map [|"1"; "0"|] -> mapi [|"[0]=1"; "[1]=0"|]
    [|2; 1; 0|] -> map [|"2"; "1"; "0"|] -> mapi [|"[0]=2"; "[1]=1"; "[2]=0"|]
    |}]

let%expect_test "fold_map,foldi_map" =
  let open Format in
  let pp_str ppf t = Format.fprintf ppf "%S" t in
  let test_fold_map uarr = begin
    let accum, sarr = fold_map uarr ~init:0 ~f:(fun accum elm ->
      (accum + elm), (asprintf "%a" Uns.pp elm)
    ) in
    let accum2, sarr2 = foldi_map uarr ~init:0 ~f:(fun i accum elm ->
      (accum + i + elm),
      (asprintf "[%a]=%a" Uns.pp i Uns.pp elm)
    ) in
    printf "%a -> fold_map %a %a -> foldi_map %a %a\n"
      (pp Uns.pp) uarr
      Uns.pp accum
      (pp pp_str) sarr
      Uns.pp accum2
      (pp pp_str) sarr2
  end in
  printf "@[<h>";
  test_fold_map [||];
  test_fold_map [|0|];
  test_fold_map [|1; 0|];
  test_fold_map [|2; 1; 0|];
  printf "@]";

  [%expect{|
    [||] -> fold_map 0 [||] -> foldi_map 0 [||]
    [|0|] -> fold_map 0 [|"0"|] -> foldi_map 0 [|"[0]=0"|]
    [|1; 0|] -> fold_map 1 [|"1"; "0"|] -> foldi_map 2 [|"[0]=1"; "[1]=0"|]
    [|2; 1; 0|] -> fold_map 3 [|"2"; "1"; "0"|] -> foldi_map 6 [|"[0]=2"; "[1]=1"; "[2]=0"|] |}]

let%expect_test "filter,filteri" =
  let open Format in
  let test_filter arr = begin
    let farr = filter arr ~f:(fun elm -> elm % 2 = 0) in
    let farr2 = filteri arr ~f:(fun _ elm -> elm % 2 = 0) in
    let farr3 = filteri arr ~f:(fun i _ -> i % 2 = 0) in
    printf "%a -> filter %a -> filteri %a %a\n"
      (pp Uns.pp) arr
      (pp Uns.pp) farr
      (pp Uns.pp) farr2
      (pp Uns.pp) farr3
  end in
  printf "@[<h>";
  test_filter [||];
  test_filter [|0|];
  test_filter [|1; 0|];
  test_filter [|2; 1; 0|];
  test_filter [|3; 2; 1; 0|];
  printf "@]";

  [%expect{|
    [||] -> filter [||] -> filteri [||] [||]
    [|0|] -> filter [|0|] -> filteri [|0|] [|0|]
    [|1; 0|] -> filter [|0|] -> filteri [|0|] [|1|]
    [|2; 1; 0|] -> filter [|2; 0|] -> filteri [|2; 0|] [|2; 0|]
    [|3; 2; 1; 0|] -> filter [|2; 0|] -> filteri [|2; 0|] [|3; 1|] |}]

let%expect_test "fold2_until,foldi2_until" =
  let open Format in
  let test_fold2_until uarr0 uarr1 = begin
    printf "%a %a"
      (pp Uns.pp) uarr0
      (pp Uns.pp) uarr1
    ;
    let accum = fold2_until uarr0 uarr1 ~init:0
        ~f:(fun accum elm0 elm1 ->
          (accum + elm0 + elm1), (accum > 10)
        ) in
    printf " -> fold2_until %a" Uns.pp accum;
    let accum = foldi2_until uarr0 uarr1 ~init:0
        ~f:(fun i accum elm0 elm1 ->
          (accum + i + elm0 + elm1), ((i + 2) >= (length uarr0))
        ) in
    printf " -> foldi2_until %a\n" Uns.pp accum
  end in
  printf "@[<h>";
  test_fold2_until [||] [||];
  test_fold2_until [|1|] [|0|];
  test_fold2_until [|3; 2|] [|1; 0|];
  test_fold2_until [|5; 4; 3|] [|2; 1; 0|];
  printf "@]";

  [%expect{|
    [||] [||] -> fold2_until 0 -> foldi2_until 0
    [|1|] [|0|] -> fold2_until 1 -> foldi2_until 1
    [|3; 2|] [|1; 0|] -> fold2_until 6 -> foldi2_until 4
    [|5; 4; 3|] [|2; 1; 0|] -> fold2_until 15 -> foldi2_until 13 |}]

let%expect_test "fold2,foldi2" =
  let open Format in
  let test_fold2 uarr0 uarr1 = begin
    printf "%a %a"
      (pp Uns.pp) uarr0
      (pp Uns.pp) uarr1
    ;
    let accum = fold2 uarr0 uarr1 ~init:0 ~f:(fun accum elm0 elm1 ->
      accum + elm0 + elm1
    ) in
    printf " -> fold2 %a" Uns.pp accum;
    let accum = foldi2 uarr0 uarr1 ~init:0
        ~f:(fun i accum elm0 elm1 -> accum + i + elm0 + elm1 ) in
    printf " -> foldi2 %a\n" Uns.pp accum
  end in
  printf "@[<h>";
  test_fold2 [||] [||];
  test_fold2 [|1|] [|0|];
  test_fold2 [|3; 2|] [|1; 0|];
  test_fold2 [|5; 4; 3|] [|2; 1; 0|];
  printf "@]";

  [%expect{|
    [||] [||] -> fold2 0 -> foldi2 0
    [|1|] [|0|] -> fold2 1 -> foldi2 1
    [|3; 2|] [|1; 0|] -> fold2 6 -> foldi2 7
    [|5; 4; 3|] [|2; 1; 0|] -> fold2 15 -> foldi2 18 |}]

let%expect_test "iteri2" =
  let open Format in
  let print_uns_arrays arr0 arr1 = begin
    printf "[|";
    iteri2 arr0 arr1 ~f:(fun i elm0 elm1 ->
      if i > 0 then printf "; ";
      printf "(%a, %a)" Uns.pp elm0 Uns.pp elm1
    );
    printf "|]"
  end in
  let test_iter2 arr0 arr1 = begin
    print_uns_arrays arr0 arr1;
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
  let open Format in
  let pp_str ppf t = Format.fprintf ppf "%S" t in
  let test_map2 uarr0 uarr1 = begin
    let sarr = map2 uarr0 uarr1 ~f:(fun elm0 elm1 ->
      asprintf "(%a,%a)" Uns.pp elm0 Uns.pp elm1
    ) in
    let sarr2 = mapi2 uarr0 uarr1 ~f:(fun i elm0 elm1 ->
      asprintf "[%a]=(%a,%a)" Uns.pp i Uns.pp elm0 Uns.pp elm1
    ) in
    printf "%a %a -> map2 %a -> mapi2 %a\n"
      (pp Uns.pp) uarr0
      (pp Uns.pp) uarr1
      (pp pp_str) sarr
      (pp pp_str) sarr2
  end in
  printf "@[<h>";
  test_map2 [||] [||];
  test_map2 [|1|] [|0|];
  test_map2 [|3; 2|] [|1; 0|];
  test_map2 [|5; 4; 3|] [|2; 1; 0|];
  printf "@]";

  [%expect{|
    [||] [||] -> map2 [||] -> mapi2 [||]
    [|1|] [|0|] -> map2 [|"(1,0)"|] -> mapi2 [|"[0]=(1,0)"|]
    [|3; 2|] [|1; 0|] -> map2 [|"(3,1)"; "(2,0)"|] -> mapi2 [|"[0]=(3,1)"; "[1]=(2,0)"|]
    [|5; 4; 3|] [|2; 1; 0|] -> map2 [|"(5,2)"; "(4,1)"; "(3,0)"|] -> mapi2 [|"[0]=(5,2)"; "[1]=(4,1)"; "[2]=(3,0)"|] |}]

let%expect_test "fold2_map,foldi2_map" =
  let open Format in
  let pp_str ppf t = Format.fprintf ppf "%S" t in
  let test_fold2_map uarr0 uarr1 = begin
    let accum, sarr = fold2_map uarr0 uarr1 ~init:0
        ~f:(fun accum elm0 elm1 ->
          (accum + elm0 + elm1),
          (asprintf "(%a,%a)" Uns.pp elm0 Uns.pp elm1)
        ) in
    let accum2, sarr2 = foldi2_map uarr0 uarr1 ~init:0
        ~f:(fun i accum elm0 elm1 ->
          (accum + i + elm0 + elm1),
          (asprintf "[%a]=(%a,%a)" Uns.pp i Uns.pp elm0 Uns.pp elm1)
        ) in
    printf "%a %a -> fold2_map %a %a -> foldi2_map %a %a\n"
      (pp Uns.pp) uarr0
      (pp Uns.pp) uarr1
      Uns.pp accum
      (pp pp_str) sarr
      Uns.pp accum2
      (pp pp_str) sarr2
  end in
  printf "@[<h>";
  test_fold2_map [||] [||];
  test_fold2_map [|1|] [|0|];
  test_fold2_map [|3; 2|] [|1; 0|];
  test_fold2_map [|5; 4; 3|] [|2; 1; 0|];
  printf "@]";

  [%expect{|
    [||] [||] -> fold2_map 0 [||] -> foldi2_map 0 [||]
    [|1|] [|0|] -> fold2_map 1 [|"(1,0)"|] -> foldi2_map 1 [|"[0]=(1,0)"|]
    [|3; 2|] [|1; 0|] -> fold2_map 6 [|"(3,1)"; "(2,0)"|] -> foldi2_map 7 [|"[0]=(3,1)"; "[1]=(2,0)"|]
    [|5; 4; 3|] [|2; 1; 0|] -> fold2_map 15 [|"(5,2)"; "(4,1)"; "(3,0)"|] -> foldi2_map 18 [|"[0]=(5,2)"; "[1]=(4,1)"; "[2]=(3,0)"|] |}]

let%expect_test "zip,unzip" =
  let open Format in
  let test_zip arr0 arr1 = begin
    let arr0', arr1' = unzip (zip arr0 arr1) in
    printf "%a %a\n"
      (pp Uns.pp) arr0'
      (pp Uns.pp) arr1'
  end in
  printf "@[<h>";
  test_zip [||] [||];
  test_zip [|0|] [|1|];
  test_zip [|0; 1|] [|2; 3|];
  test_zip [|0; 1; 2|] [|3; 4; 5|];
  printf "@]";

  [%expect{|
    [||] [||]
    [|0|] [|1|]
    [|0; 1|] [|2; 3|]
    [|0; 1; 2|] [|3; 4; 5|] |}]
