open SeqIntf

module MakeDef (T : IMonoDef) : SMonoDef
  with type t := T.t
  with type elm := T.elm = struct
  include T

  let next_opt t =
    match length t with
    | 0L -> None
    | _ -> Some (next t)
end

module MakeIndef (T : IMonoIndef) : SMonoIndef
  with type t := T.t
  with type elm := T.elm = struct
  include T
end

module MakePoly2Fold2 (T : IPoly2Fold2) : SPoly2Fold2
  with type ('a, 'cmp) t := ('a, 'cmp) T.container
  with type 'a elm := 'a T.elm = struct
  let fold2_until ~init ~f t0 t1 =
    let open Cmp in
    let next seq  = begin
      match T.next_opt seq with
      | None -> None, seq
      | Some (a, seq') -> (Some a), seq'
    end in
    let rec fn accum ~f cmp a0_opt seq0 a1_opt seq1 = begin
      let f_a0_a1 accum ~f cmp a0_opt seq0 a1_opt seq1 = begin
        let accum', until = f accum a0_opt a1_opt in
        match until with
        | true -> accum'
        | false -> begin
            let a0_opt', seq0' = next seq0 in
            let a1_opt', seq1' = next seq1 in
            fn accum' ~f cmp a0_opt' seq0' a1_opt' seq1'
          end
      end in
      let f_a0 accum ~f cmp a0_opt seq0 a1_opt seq1 = begin
        let accum', until = f accum a0_opt None in
        match until with
        | true -> accum'
        | false -> begin
            let a0_opt', seq0' = next seq0 in
            fn accum' ~f cmp a0_opt' seq0' a1_opt seq1
          end
      end in
      let f_a1 accum ~f cmp a0_opt seq0 a1_opt seq1 = begin
        let accum', until = f accum None a1_opt in
        match until with
        | true -> accum'
        | false -> begin
            let a1_opt', seq1' = next seq1 in
            fn accum' ~f cmp a0_opt seq0 a1_opt' seq1'
          end
      end in
      match a0_opt, a1_opt with
      | None, None -> accum
      | Some _, None
      | None, Some _ -> f_a0_a1 accum ~f cmp a0_opt seq0 a1_opt seq1
      | Some a0, Some a1 ->
        match cmp a0 a1 with
        | Lt -> f_a0 accum ~f cmp a0_opt seq0 a1_opt seq1
        | Eq -> f_a0_a1 accum ~f cmp a0_opt seq0 a1_opt seq1
        | Gt -> f_a1 accum ~f cmp a0_opt seq0 a1_opt seq1
    end in
    let a0_opt, seq0 = next (T.init t0) in
    let a1_opt, seq1 = next (T.init t1) in
    fn init ~f (T.cmp (T.cmper t0)) a0_opt seq0 a1_opt seq1

  let fold2 ~init ~f t0 t1 =
    fold2_until ~init ~f:(fun accum t0 t1 -> (f accum t0 t1), false) t0 t1

  let iter2 ~f t0 t1 =
    fold2 ~init:() ~f:(fun _ t0 t1 -> f t0 t1) t0 t1
end

module MakePoly3Fold2 (T : IPoly3Fold2) : SPoly3Fold2
  with type ('k, 'v, 'cmp) t := ('k, 'v, 'cmp) T.container
  with type 'k key := 'k T.key
  with type 'v value := 'v T.value = struct
  let fold2_until ~init ~f t0 t1 =
    let open Cmp in
    let next seq  = begin
      match T.next_opt seq with
      | None -> None, seq
      | Some ((k, v), seq') -> (Some (k, v)), seq'
    end in
    let rec fn accum ~f cmp kv0_opt seq0 kv1_opt seq1 = begin
      let f_kv0_kv1 accum ~f cmp kv0_opt seq0 kv1_opt seq1 = begin
        let accum', until = f accum kv0_opt kv1_opt in
        match until with
        | true -> accum'
        | false -> begin
            let kv0_opt', seq0' = next seq0 in
            let kv1_opt', seq1' = next seq1 in
            fn accum' ~f cmp kv0_opt' seq0' kv1_opt' seq1'
          end
      end in
      let f_kv0 accum ~f cmp kv0_opt seq0 kv1_opt seq1 = begin
        let accum', until = f accum kv0_opt None in
        match until with
        | true -> accum'
        | false -> begin
            let kv0_opt', seq0' = next seq0 in
            fn accum' ~f cmp kv0_opt' seq0' kv1_opt seq1
          end
      end in
      let f_kv1 accum ~f cmp kv0_opt seq0 kv1_opt seq1 = begin
        let accum', until = f accum None kv1_opt in
        match until with
        | true -> accum'
        | false -> begin
            let kv1_opt', seq1' = next seq1 in
            fn accum' ~f cmp kv0_opt seq0 kv1_opt' seq1'
          end
      end in
      match kv0_opt, kv1_opt with
      | None, None -> accum
      | Some _, None
      | None, Some _ -> f_kv0_kv1 accum ~f cmp kv0_opt seq0 kv1_opt seq1
      | Some (k0, _), Some (k1, _) ->
        match cmp k0 k1 with
        | Lt -> f_kv0 accum ~f cmp kv0_opt seq0 kv1_opt seq1
        | Eq -> f_kv0_kv1 accum ~f cmp kv0_opt seq0 kv1_opt seq1
        | Gt -> f_kv1 accum ~f cmp kv0_opt seq0 kv1_opt seq1
    end in
    let kv0_opt, seq0 = next (T.init t0) in
    let kv1_opt, seq1 = next (T.init t1) in
    fn init ~f (T.cmp (T.cmper t0)) kv0_opt seq0 kv1_opt seq1

  let fold2 ~init ~f t0 t1 =
    fold2_until ~init ~f:(fun accum t0 t1 -> (f accum t0 t1), false) t0 t1

  let iter2 ~f t0 t1 =
    fold2 ~init:() ~f:(fun _ t0 t1 -> f t0 t1) t0 t1
end
