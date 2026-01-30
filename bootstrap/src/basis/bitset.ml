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

  let fold_until ~init ~f t =
    let rec fn accum f rem = begin
      match is_empty rem with
      | true -> accum
      | false -> begin
          let elm = Nat.bit_ctz rem in
          let accum', until = f accum elm in
          match until with
          | true -> accum'
          | false -> begin
              let rem' = remove elm rem in
              fn accum' f rem'
            end
        end
    end in
    fn init f t

  let fold_right_until ~init ~f t =
    let rec fn accum f rem = begin
      match is_empty rem with
      | true -> accum
      | false -> begin
          let elm = Nat.floor_lg rem in
          let accum', until = f accum elm in
          match until with
          | true -> accum'
          | false -> begin
              let rem' = remove elm rem in
              fn accum' f rem'
            end
        end
    end in
    fn init f t

  let nth_nonempty i t =
    assert (not (is_empty t));
    let rec fn j elm i t = begin
      let tz = Nat.bit_ctz t in
      match j = i with
      | true -> elm + tz
      | false -> begin
          let shift = succ tz in
          fn (succ j) (elm + shift) i (Nat.bit_sr ~shift t)
        end
    end in
    fn 0L 0L i t

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
      }

      let cmp {i=i0; _} {i=i1; _} =
        Uns.cmp i0 i1
    end
    include T
    include Cmpable.Make(T)

    let hd bitset =
      {bitset; i=0L}

    let tl bitset =
      {bitset; i=length bitset}

    let succ {bitset; i} =
      match Uns.( <= ) i (length bitset) with
      | false -> halt "Out of bounds"
      | true -> {bitset; i=succ i}

    let pred {bitset; i} =
      match Uns.(i > 0L) with
      | false -> halt "Out of bounds"
      | true -> {bitset; i=pred i}

    let lget {bitset; i} =
      nth (Uns.pred i) bitset

    let rget {bitset; i} =
      nth i bitset

    let prev {bitset; i} =
      match Uns.(i > 0L) with
      | false -> halt "Out of bounds"
      | true -> begin
          let i' = Uns.pred i in
          nth i' bitset, {bitset; i=i'}
        end

    let next t =
      rget t, succ t
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
    i: uns;
    bitset: container
  }

  let init container =
    {i=0L; bitset=container}

  let length {i; bitset} =
    length bitset - i

  let next {i; bitset} =
    assert (not (is_empty bitset));
    let elm = nth_nonempty i bitset in
    elm, {i=succ i; bitset}

  let next_opt ({i; bitset} as t) =
    match length t = 0L with
    | false -> begin
        let elm = nth_nonempty i bitset in
        Some (elm, {i=succ i; bitset})
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
  let rec fn i accum = begin
    match (i = length t) with
    | true -> accum
    | false -> fn (succ i) (f accum (nth i t))
  end in
  match is_empty t with
  | true -> None
  | false -> begin
      Some (fn 1L (nth 0L t))
    end

let reduce_hlt ~f t =
  match reduce ~f t with
  | None -> halt "Empty bitset"
  | Some elm -> elm

let fmt ?(alt=Fmt.alt_default) ?(width=Fmt.width_default) t formatter =
  List.fmt ~alt ~width Uns.pp (to_list t) formatter

let pp t formatter =
  fmt t formatter
