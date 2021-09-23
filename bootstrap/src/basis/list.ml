open Rudiments0

include List0

let reduce ~f t =
  let rec fn accum t = begin
    match t with
    | [] -> Some accum
    | elm :: t' -> fn (f accum elm) t'
  end in
  match t with
  | [] -> None
  | elm :: t' -> fn elm t'

let reduce_hlt ~f t =
  match reduce t ~f with
  | None -> halt "Empty list cannot be reduced"
  | Some result -> result

let is_sorted ?strict ~cmp t =
  let strict = match strict with
    | None -> false
    | Some b -> b
  in
  let rec fn elm t = begin
    let open Cmp in
    match t with
    | [] -> true
    | elm' :: t' -> begin
        match (cmp elm elm'), strict with
        | Lt, _
        | Eq, false -> fn elm' t'
        | Eq, true
        | Gt, _ -> false
      end
  end in
  match t with
  | [] -> true
  | elm :: t' -> fn elm t'

let sort ?length ?stable ~cmp t =
  let arr = Array.of_list ?length t in
  Array.sort_inplace ?stable arr ~cmp;
  Array.to_list arr

let dedup ?length ~cmp t =
  let rec fn member t arr i = begin
    let open Cmp in
    let elm = Array.get i arr in
    let member', t' = match cmp elm member with
      | Lt -> elm, (member :: t)
      | Eq -> elm, t
      | Gt -> not_reached ()
    in
    if i = zero then member' :: t'
    else fn member' t' arr (Uns.pred i)
  end in
  match t with
  | []
  | _ :: [] -> t
  | _ :: _ -> begin
      let arr = Array.of_list ?length t in
      Array.sort_inplace ~stable:true arr ~cmp;
      let i = Uns.pred (Array.length arr) in
      fn (Array.get i arr) [] arr (Uns.pred i)
    end

let rev_dedup ?length ~cmp t =
  let rec fn member t arr i = begin
    let open Cmp in
    let elm = Array.get i arr in
    let member', t' = match cmp elm member with
      | Lt -> not_reached ()
      | Eq -> member, t
      | Gt -> elm, (member :: t)
    in
    let i' = Uns.succ i in
    if i' = (Array.length arr) then member' :: t'
    else fn member' t' arr i'
  end in
  match t with
  | []
  | _ :: [] -> t
  | _ :: _ -> begin
      let arr = Array.of_list ?length t in
      Array.sort_inplace ~stable:true arr ~cmp;
      let i = 0L in
      fn (Array.get i arr) [] arr (Uns.succ i)
    end

let dedup_sorted ~cmp t =
  let rec fn member t = begin
    let open Cmp in
    match t with
    | [] -> [member]
    | elm :: t' -> begin
        match cmp member elm with
        | Lt -> member :: (fn elm t')
        | Eq -> fn member t'
        | Gt -> not_reached ()
      end
  end in
  match t with
  | []
  | _ :: [] -> t
  | elm :: t' -> fn elm t'

let rev_dedup_sorted ~cmp t =
  let rec fn member t accum = begin
    let open Cmp in
    match t with
    | [] -> member :: accum
    | elm :: t' -> begin
        match cmp member elm with
        | Lt -> fn elm t' (member :: accum)
        | Eq -> fn member t' accum
        | Gt -> not_reached ()
      end
  end in
  match t with
  | []
  | _ :: [] -> t
  | elm :: t' -> fn elm t' []

let merge ~cmp t0 t1 =
  let rec fn elm0 t0 elm1 t1 = begin
    match cmp elm0 elm1 with
    | Cmp.Lt
    | Cmp.Eq -> begin
        match t0 with
        | [] -> elm0 :: elm1 :: t1
        | elm0' :: t0' -> elm0 :: (fn elm0' t0' elm1 t1)
      end
    | Cmp.Gt -> begin
        match t1 with
        | [] -> elm1 :: elm0 :: t0
        | elm1' :: t1' -> elm1 :: (fn elm0 t0 elm1' t1')
      end
  end in
  match t0, t1 with
  | [], _ -> t1
  | _, [] -> t0
  | (elm0 :: t0'), (elm1 :: t1') -> fn elm0 t0' elm1 t1'

let rev_merge ~cmp t0 t1 =
  let rec fn elm0 t0 elm1 t1 accum = begin
    let open Cmp in
    match cmp elm0 elm1 with
    | Lt
    | Eq -> begin
        let accum' = elm0 :: accum in
        match t0 with
        | [] -> rev_concat t1 (elm1 :: accum')
        | elm0' :: t0' -> fn elm0' t0' elm1 t1 accum'
      end
    | Gt -> begin
        let accum' = elm1 :: accum in
        match t1 with
        | [] -> rev_concat t0 (elm0 :: accum')
        | elm1' :: t1' -> fn elm0 t0 elm1' t1' accum'
      end
  end in
  match t0, t1 with
  | [], _ -> t1
  | _, [] -> t0
  | (elm0 :: t0'), (elm1 :: t1') -> fn elm0 t0' elm1 t1' []
