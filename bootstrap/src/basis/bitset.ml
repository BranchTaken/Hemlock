open Rudiments0

module T = struct
  type t = Nat.t
  type elm = uns

  let length = Nat.bit_pop

  let is_empty t =
    Nat.(t = zero)

  let hash_fold = Nat.hash_fold

  let empty = Nat.zero

  let singleton elm =
    Nat.(bit_sl ~shift:elm one)

  let mem elm t =
    Nat.(bit_and (singleton elm) t <> zero)

  let remove elm t =
    match mem elm t with
    | false -> t
    | true -> Nat.(bit_xor (singleton elm) t)

  let first_opt t =
    match is_empty t with
    | false -> Some (Nat.bit_ctz t)
    | true -> None

  let first t =
    match first_opt t with
    | Some elm -> elm
    | None -> halt "No first element"

  let last_opt t =
    match is_empty t with
    | false -> Some (Nat.floor_lg t)
    | true -> None

  let last t =
    match last_opt t with
    | Some elm -> elm
    | None -> halt "No last element"

  let next_ge_opt elm t =
    let mask = Nat.pred (singleton elm) in
    let bitset_lt = Nat.bit_and mask t in
    let bitset_ge = Nat.bit_xor bitset_lt t in
    match is_empty bitset_ge with
    | false -> Some (Nat.bit_ctz bitset_ge)
    | true -> None

  let next_ge elm t =
    match next_ge_opt elm t with
    | Some next_ge -> next_ge
    | None -> halt "No next element"

  let next_gt_opt elm t =
    next_ge_opt (succ elm) t

  let next_gt elm t =
    next_ge (succ elm) t

  let prev_le_opt elm t =
    let mask = Nat.pred (singleton (succ elm)) in
    let bitset_le = Nat.bit_and mask t in
    match is_empty bitset_le with
    | false -> Some (Nat.floor_lg bitset_le)
    | true -> None

  let prev_le elm t =
    match prev_le_opt elm t with
    | Some prev_le -> prev_le
    | None -> halt "No previous element"

  let prev_lt_opt elm t =
    prev_le_opt (pred elm) t

  let prev_lt elm t =
    prev_le (pred elm) t

  let fold_until ~init ~f t =
    let rec fn l_opt accum f t = begin
      let elm_opt = match l_opt with
        | None -> first_opt t
        | Some elm -> next_gt_opt elm t
      in
      match elm_opt with
      | None -> accum
      | Some elm -> begin
          let accum', until = f accum elm in
          match until with
          | true -> accum'
          | false -> fn elm_opt accum' f t
        end
    end in
    fn None init f t

  let fold_right_until ~init ~f t =
    let rec fn r_opt accum f t = begin
      let elm_opt = match r_opt with
        | None -> last_opt t
        | Some elm -> prev_lt_opt elm t
      in
      match elm_opt with
      | None -> accum
      | Some elm -> begin
          let accum', until = f accum elm in
          match until with
          | true -> accum'
          | false -> fn elm_opt accum' f t
        end
    end in
    fn None init f t

  let nth_nonempty i t =
    assert (not (is_empty t));
    (* Binary search. The combination of sparse member bits and integer rounding mean that the
     * search can quiesce at `lo_i + 1 = hi_i`, hence two termination comparisons rather than
     * terminating only when `lo_i = hi_i`. *)
    let rec fn lo lo_i hi hi_i pivot i t = begin
      match lo_i = i, hi_i = i with
      | true, _ -> lo
      | false, true -> hi
      | false, false -> begin
          let zero_lo_mask = Nat.pred (singleton (succ lo)) in
          let zero_pivot_mask = Nat.pred (singleton (succ pivot)) in
          let lo_pivot_mask = Nat.bit_xor zero_lo_mask zero_pivot_mask in
          let lo_pivot_pop = Nat.(bit_pop (bit_and lo_pivot_mask t)) in
          match lo_pivot_pop = 0L with
          | true -> begin
              (* No members in (lo..pivot]. *)
              let pivot = (pivot + hi) / 2L in
              fn lo lo_i hi hi_i pivot i t
            end
          | false -> begin
              (* Find nearest member less than pivot. *)
              let mid = Nat.(floor_lg (bit_and zero_pivot_mask t)) in
              let mid_i = lo_i + lo_pivot_pop in
              match mid_i >= i with
              | true -> begin
                  (* i ϵ (lo_i..mid_i] *)
                  let hi = mid in
                  let hi_i = mid_i in
                  let pivot = (lo + hi) / 2L in
                  fn lo lo_i hi hi_i pivot i t
                end
              | false -> begin
                  (* i ϵ (mid_i..hi_i] *)
                  let lo = mid in
                  let lo_i = mid_i in
                  let pivot = (lo + hi) / 2L in
                  fn lo lo_i hi hi_i pivot i t
                end
            end
        end
    end in
    let lo = first t in
    let lo_i = 0L in
    let hi = last t in
    let hi_i = pred (length t) in
    let pivot = (lo + hi) / 2L in
    fn lo lo_i hi hi_i pivot i t

  let nth i t =
    match length t > i with
    | true -> nth_nonempty i t
    | false -> halt "Out of range"

  let nth_opt i t =
    match length t > i with
    | true -> Some (nth_nonempty i t)
    | false -> None

  module Cursor = struct
    module T = struct
      type container = t
      type t = {
        bitset: container;
        i: uns;
        l_opt: uns option;
        r_opt: uns option;
      }

      let cmp {i=i0; _} {i=i1; _} =
        Uns.cmp i0 i1
    end
    include T
    include Cmpable.Make(T)

    let hd bitset =
      {bitset; i=0L; l_opt=None; r_opt=first_opt bitset}

    let tl bitset =
      {bitset; i=length bitset; l_opt=last_opt bitset; r_opt=None}

    let succ {bitset; i; l_opt=_; r_opt} =
      match Uns.( <= ) i (length bitset) with
      | false -> halt "Out of bounds"
      | true -> begin
          let r_opt' = match r_opt with
            | Some r -> next_gt_opt r bitset
            | None -> None
          in
          {bitset; i=succ i; l_opt=r_opt; r_opt=r_opt'}
        end

    let pred {bitset; i; l_opt; r_opt=_} =
      match Uns.(i > 0L) with
      | false -> halt "Out of bounds"
      | true -> begin
          let l_opt' = match l_opt with
            | Some l -> prev_lt_opt l bitset
            | None -> None
          in
          {bitset; i=pred i; l_opt=l_opt'; r_opt=l_opt}
        end

    let lget {bitset; i=_; l_opt; r_opt} =
      match l_opt, r_opt with
      | Some l, _ -> l
      | _, Some r -> prev_lt r bitset
      | None, None -> halt "No element left of cursor"

    let rget {bitset; i=_; l_opt; r_opt} =
      match l_opt, r_opt with
      | _, Some r -> r
      | Some l, _ -> next_gt l bitset
      | None, None -> halt "No element right of cursor"

    let prev t =
      (* Order operations so as to amortize element search. *)
      let t' = pred t in
      rget t', t'

    let next t =
      (* Order operations so as to amortize element search. *)
      let t' = succ t in
      lget t', t'
  end
end
include T
include Container.MakeMonoFold(T)
include Container.MakeMonoArray(T)

let choose t =
  match is_empty t with
  | false -> Some (Nat.bit_ctz t)
  | true -> None

let choose_hlt t =
  match is_empty t with
  | false -> Nat.bit_ctz t
  | true -> halt "Empty bitset"

(* NB: Result for `[pn]search` is elm's index within bitset, not elm's value. *)
let search elm t =
  match mem elm t with
  | true -> begin
      let mask = Nat.pred (singleton elm) in
      let bitset_lt = Nat.bit_and mask t in
      let i = Nat.bit_pop bitset_lt in
      Some i
    end
  | false -> None

let psearch elm t =
  match is_empty t with
  | false -> begin
      match search elm t with
      | Some i -> Some (Cmp.Eq, i)
      | None -> begin
          let mask = Nat.pred (singleton elm) in
          let bitset_lt = Nat.bit_and mask t in
          match is_empty bitset_lt with
          | true -> Some (Cmp.Lt, 0L)
          | false -> begin
              let pelm = Nat.floor_lg bitset_lt in
              let mask = Nat.pred (singleton pelm) in
              let bitset_lt = Nat.bit_and mask t in
              let i = Nat.bit_pop bitset_lt in
              Some (Cmp.Gt, i)
            end
        end
    end
  | true -> None

let nsearch elm t =
  match is_empty t with
  | false -> begin
      match search elm t with
      | Some i -> Some (Cmp.Eq, i)
      | None -> begin
          let mask = Nat.pred (singleton elm) in
          let bitset_lt = Nat.bit_and mask t in
          let bitset_gt = Nat.bit_xor bitset_lt t in
          match is_empty bitset_gt with
          | true -> Some (Cmp.Gt, pred (length t))
          | false -> begin
              let i = Nat.bit_pop bitset_lt in
              Some (Cmp.Lt, i)
            end
        end
    end
  | true -> None

(* Seq. *)
module SeqMonoFold2 = struct
  type container = t
  type nonrec elm = elm
  type t = {
    bitset: container;
    i: uns;
    elm_opt: uns option;
  }

  let init container =
    {bitset=container; i=0L; elm_opt=None}

  let length {bitset; i; elm_opt=_} =
    length bitset - i

  let next ({bitset; i; elm_opt} as t) =
    assert (length t > 0L);
    let elm = match elm_opt with
      | None -> first bitset
      | Some elm -> next_gt elm bitset
    in
    elm, {bitset; i=succ i; elm_opt=Some elm}

  let next_opt ({bitset; i; elm_opt} as t) =
    match length t = 0L with
    | false -> begin
        let elm = match elm_opt with
          | None -> first bitset
          | Some elm -> next_gt elm bitset
        in
        Some (elm, {bitset; i=succ i; elm_opt=Some elm})
      end
    | true -> None

  let cmp = Uns.cmp
end
include Seq.MakeMonoFold2(SeqMonoFold2)
module Seq = SeqMonoFold2

let cmp = Nat.cmp

let equal = Nat.( = )

let subset t0 t1 =
  Nat.(bit_or t0 t1 = t0)

let disjoint t0 t1 =
  Nat.(bit_xor t0 t1 <> zero)

let insert elm t =
  Nat.(bit_or (singleton elm) t)

let of_list elms =
  match elms with
  | [] -> Nat.zero
  | elm :: elms' -> begin
      let rec fn elms bitset = begin
        match elms with
        | [] -> bitset
        | elm :: elms' -> fn elms' (insert elm bitset)
      end in
      fn elms' (singleton elm)
    end

let split elm t =
  let elm_singleton = singleton elm in
  let lt_mask = Nat.(elm_singleton |> pred) in
  let lt = Nat.bit_and lt_mask t in
  let eq_opt, elm_mask = match mem elm t with
    | true -> Some elm, elm_singleton
    | false -> None, empty
  in
  let gt_mask = Nat.bit_or lt elm_mask in
  let gt = Nat.(bit_xor gt_mask t) in
  lt, eq_opt, gt

let union = Nat.bit_or

let of_array arr =
  match arr with
  | [||] -> empty
  | _ -> Array.reduce_hlt (Array.map arr ~f:(fun elm -> singleton elm))
    ~f:(fun bitset0 bitset1 -> union bitset0 bitset1)

let inter = Nat.bit_and

let diff t0 t1 =
  Nat.(bit_and t0 t1 |> bit_xor t0)

let filter ~f t =
  fold ~init:empty ~f:(fun accum elm ->
    match f elm with
    | false -> accum
    | true -> insert elm accum
  ) t

let filteri ~f t =
  foldi ~init:empty ~f:(fun i accum elm ->
    match f i elm with
    | false -> accum
    | true -> insert elm accum
  ) t

let partition_tf ~f t =
  fold ~init:(empty, empty) ~f:(fun (t_true, t_false) elm ->
    match f elm with
    | true -> (insert elm t_true), t_false
    | false -> t_true, (insert elm t_false)
  ) t

let partitioni_tf ~f t =
  foldi ~init:(empty, empty) ~f:(fun i (t_true, t_false) elm ->
    match f i elm with
    | true -> (insert elm t_true), t_false
    | false -> t_true, (insert elm t_false)
  ) t

let reduce ~f t =
  let rec fn i elm accum t = begin
    match (i < length t) with
    | false -> accum
    | true -> begin
        let elm' = next_gt elm t in
        fn (succ i) elm' (f accum elm') t
      end
  end in
  match is_empty t with
  | true -> None
  | false -> begin
      let elm = first t in
      Some (fn 1L elm elm t)
    end

let reduce_hlt ~f t =
  match reduce ~f t with
  | None -> halt "Empty bitset"
  | Some elm -> elm

let fmt ?(alt=Fmt.alt_default) ?(width=Fmt.width_default) t formatter =
  List.fmt ~alt ~width Uns.pp (to_list t) formatter

let pp t formatter =
  fmt t formatter
