(* Hash array mapped trie (HAMT) implementation of maps.  See the following for
 * background information:
 *
 * - [Ideal Hash
 *   Trees](http://infoscience.epfl.ch/record/64398/files/idealhashtrees.pdf)
 * - [Optimizing Hash-Array Mapped Tries for Fast and Lean Immutable JVM
 *   Collections](http://michael.steindorfer.name/publications/oopsla15.pdf)
 *
 * This implementation uses a canonical representation that results in stable
 * (but arbitrary) key ordering.  The canonical representation differs slightly
 * from that described in the referenced papers with regard to collisions.
 * Following is an informal description of canonical representation.
 *
 * - No subtree is empty, with the single exception of a root node representing
 *   an empty map.
 * - No subtree contains a single key, with the single exception of a root node
 *   representing a singleton.
 * - Collisions are always stored as children (specially interpreted nodes) of
 *   maximal-depth leaf nodes; their depth uniquely defines them as collisions,
 *   as does their present_{kv,child} bitmaps.  Collisions could in principle be
 *   stored closer to the root, but assuming a well defined hash function, doing
 *   so would increase common-case complexity for the sake of an edge condition
 *   that is already degenerate performance-wise.
 * - Collisions are stored in sorted order within each collision node.  This has
 *   the side benefit of making key lookup degrade from O(1) to O(lg n) in the
 *   presence of collisions (rather than O(n)), but the primary benefit is that
 *   it improves coupled iteration performance. *)

open Rudiments

(* Module for type used to encode node's present_{kv,child} bit sets. *)
module Bitset = U63

(* Number of bits per HAMT level.  Bitset.t, which is used for node's
 * present_{kv,child} fields must have at least elms_per_level bits. *)
let bits_per_level = 5
let elms_per_level = bit_sl ~shift:bits_per_level 1

(* Number of hash bits available per hash value. *)
let bits_per_hash = 128

(* Maximum HAMT height, ignoring collision nodes. *)
let max_height = bits_per_hash / bits_per_level

type ('k, 'v) node = {
  (* The present_{kv,child} bitmaps encode which node elements are non-empty.
   * Each node logically has elms_per_level elements, but only the non-empty
   * ones are stored in elms_{kv,child}.  The two bitmaps are non-intersecting
   * because each node element can only be a child *or* a key-value binding.
   *
   * The tree is limited to max_height levels, but leaves may need to store
   * collisions.  Collisions are stored as nodes at depth max_height, but they
   * must be interpreted specially.  We set all bits in both bitmaps for
   * collision nodes so that code which does not track recursion depth can still
   * recognize collision nodes. *)
  present_kv: Bitset.t;
  present_child: Bitset.t;
  (* Independent variable-length compressed element arrays, where the summed
   * length of the arrays is in [0..elms_per_level].  For example, if the least
   * significant bit of the present_kv bitmap is 1, then the corresponding
   * element of present_kv is at index 0, regardless of {present,elms}_child. *)
  elms_kv: ('k * 'v) array;
  elms_child: ('k, 'v) node array;
}

(* Used only ephemerally during remove and reduce. *)
type ('k, 'v) elm =
  | Child of ('k, 'v) node
  | Kv of ('k * 'v)

type ('k, 'v, 'cmp) t = {
  cmper: ('k, 'cmp) Cmper.t;
  length: usize;
  root: ('k, 'v) node;
}

type ('k, 'cmp) cmper =
  (module Cmper.S_mono with type t = 'k and type cmper_witness = 'cmp)

let cmper_m (type k cmp) t : (k, cmp) cmper =
  (module struct
    type t = k
    type cmper_witness = cmp
    let cmper = t.cmper
  end)

(* Extract cmper from first-class module compatible with Cmper.S_mono . *)
let m_cmper (type k cmp) ((module M) : (k, cmp) cmper) =
  M.cmper

(* Given a key and a non-empty array of key-value pairs, synthesize a pair of
 * the form (k, _) to be used with k_cmp. *)
let k__of_k_kvs k kvs =
  let _, v = Array.get 0 kvs in
  k, v

(* Compare keys in key-value array tuples, ignoring values. *)
let k_cmp kcmp (k0, _) (k1, _) =
  kcmp k0 k1

let kvcmp kcmp vcmp (k0, v0) (k1, v1) =
  let open Cmp in
  match kcmp k0 k1 with
  | Lt -> Lt
  | Eq -> vcmp v0 v1
  | Gt -> Gt

let cmper t =
  t.cmper

let shift_of_level level =
  bits_per_hash - (level + 1) * bits_per_level

let present_index_at_level hash level =
  let shift = shift_of_level level in
  let mask = U128.of_usize (elms_per_level - 1) in
  U128.(to_usize (bit_and mask (bit_usr ~shift hash)))

let present_bit_of_index index =
  Bitset.bit_sl ~shift:index Bitset.one

let elm_index_of_bit present present_bit =
  let trail_mask = Bitset.(present_bit - one) in
  Bitset.(bit_pop (bit_and trail_mask present))

let empty m =
  {cmper=m_cmper m; length=0;
    root={present_kv=Bitset.zero; present_child=Bitset.zero;
      elms_kv=[||]; elms_child=[||]}}

let singleton m ~k ~v =
  let cmper = m_cmper m in
  let k_hash = Hash.State.seed |> cmper.hash_fold k |> Hash.t_of_state in
  let present_index = present_index_at_level k_hash 0 in
  let present_kv = present_bit_of_index present_index in
  {cmper; length=1;
    root={present_kv; present_child=Bitset.zero;
      elms_kv=[|(k, v)|]; elms_child=[||]}}

let length t =
  t.length

let is_empty t =
  t.length = 0

let get a t =
  let open Cmper in
  let rec fn a a_hash cmper node level = begin
    match level < max_height with
    | true -> begin
        let present_index = present_index_at_level a_hash level in
        let present_bit = present_bit_of_index present_index in
        match Bitset.((bit_and present_bit node.present_kv) = zero) with
        | true -> begin
            match Bitset.((bit_and present_bit node.present_child) = zero) with
            | true -> None
            | false -> begin
                let elms_child_index =
                  elm_index_of_bit node.present_child present_bit in
                let child = Array.get elms_child_index node.elms_child in
                fn a a_hash cmper child (succ level)
              end
          end
        | false -> begin
            let elms_kv_index =
              elm_index_of_bit node.present_kv present_bit in
            let k, v = Array.get elms_kv_index node.elms_kv in
            match Cmp.is_eq (cmper.cmp a k) with
            | true -> Some v
            | false -> None
          end
      end
    | false -> begin
        (* Collision node. *)
        let k_ = k__of_k_kvs a node.elms_kv in
        match Array.search k_ ~cmp:(k_cmp cmper.cmp) node.elms_kv with
        | Some i -> begin
            let _, v = Array.get i node.elms_kv in
            Some v
          end
        | None -> None
      end
  end in
  let a_hash = Hash.State.seed |> t.cmper.hash_fold a |> Hash.t_of_state in
  fn a a_hash t.cmper t.root 0

let get_hlt a t =
  match get a t with
  | Some v -> v
  | None -> halt "Key not found"

let mem a t =
  Option.is_some (get a t)

let choose t =
  let rec fn node = begin
    match node.elms_kv, node.elms_child with
    | [||], [||] -> None
    | [||], _ -> fn (Array.get 0 node.elms_child)
    | _, _ -> Some (Array.get 0 node.elms_kv)
  end in
  fn t.root

let choose_hlt t =
  match choose t with
  | Some (k, v) -> k, v
  | None -> halt "Empty map"

let insert_impl k ~f t =
  let open Cmper in
  (* Create a collision node, with keys canonically ordered (sorted). *)
  let node_of_collision cmper (k0, v0) (k1, v1) = begin
    let kv0, kv1 = match cmper.cmp k0 k1 with
      | Lt -> (k0, v0), (k1, v1)
      | Eq -> not_reached ()
      | Gt -> (k1, v1), (k0, v0)
    in
    {present_kv=Bitset.max_value; present_child=Bitset.max_value;
      elms_kv=[|kv0; kv1|]; elms_child=[||]}
  end in
  (* Create a disambiguating subtree for a and b. *)
  let rec disambiguate (k0, v0) k0_hash (k1, v1) k1_hash cmper level = begin
    assert (level < max_height);

    (* Create a 2-element node, where elm0's index is less than elm1's. *)
    let node_of_kvs kv0 k0_present_index kv1 k1_present_index = begin
      assert (k0_present_index < k1_present_index);
      let k0_present_bit = present_bit_of_index k0_present_index in
      let k1_present_bit = present_bit_of_index k1_present_index in
      let present_kv = Bitset.bit_or k0_present_bit k1_present_bit in
      let elms_kv = [|kv0; kv1|] in
      {present_kv; present_child=Bitset.zero; elms_kv; elms_child=[||]}
    end in

    let k0_present_index = present_index_at_level k0_hash level in
    let k1_present_index = present_index_at_level k1_hash level in
    match Usize.cmp k0_present_index k1_present_index with
    | Lt -> node_of_kvs (k0, v0) k0_present_index (k1, v1) k1_present_index
    | Eq -> begin
        let present_child = present_bit_of_index k0_present_index in
        let child = match level + 1 < max_height with
          | false -> node_of_collision cmper (k0, v0) (k1, v1)
          | true ->
            disambiguate (k0, v0) k0_hash (k1, v1) k1_hash cmper (succ level)
        in
        let elms_child = [|child|] in
        {present_kv=Bitset.zero; present_child; elms_kv=[||]; elms_child}
      end
    | Gt -> node_of_kvs (k1, v1) k1_present_index (k0, v0) k0_present_index
  end in
  let rec fn k f k_hash cmper node level = begin
    let present_index = present_index_at_level k_hash level in
    let present_bit = present_bit_of_index present_index in
    match Bitset.((bit_and present_bit node.present_kv) = zero) with
    | true -> begin
        match Bitset.((bit_and present_bit node.present_child) = zero) with
        | true -> begin
            (* Not present in node. *)
            match f None with
            | Some v -> begin
                (* Insert. *)
                let elms_kv_index =
                  elm_index_of_bit node.present_kv present_bit in
                let present_kv' = Bitset.bit_or present_bit node.present_kv in
                let elms_kv' = Array.insert elms_kv_index (k, v) node.elms_kv in
                Some {node with present_kv=present_kv'; elms_kv=elms_kv'}, 1
              end
            | None -> None, 0
          end
        | false -> begin
            let elms_child_index =
              elm_index_of_bit node.present_child present_bit in
            let child = Array.get elms_child_index node.elms_child in
            match level + 1 < max_height with
            | true -> begin
                match fn k f k_hash cmper child (succ level) with
                | None, delta -> None, delta
                | Some child', delta -> begin
                    let elms_child' =
                      Array.set elms_child_index child' node.elms_child in
                    Some {node with elms_child=elms_child'}, delta
                  end
              end
            | false -> begin
                (* Collision node. *)
                let collision_insert i kv child elms_child_index node = begin
                  let child_elms_kv' = Array.insert i kv child.elms_kv in
                  let child' = {child with elms_kv=child_elms_kv'} in
                  let elms_child' =
                    Array.set elms_child_index child' node.elms_child in
                  Some {node with elms_child=elms_child'}
                end in
                let collision_replace i kv child elms_child_index node = begin
                  let child_elms_kv' = Array.set i kv child.elms_kv in
                  let child' = {child with elms_kv=child_elms_kv'} in
                  let elms_child' =
                    Array.set elms_child_index child' node.elms_child in
                  Some {node with elms_child=elms_child'}
                end in
                let k_ = k__of_k_kvs k child.elms_kv in
                match Array.nsearch k_ ~cmp:(k_cmp cmper.cmp) child.elms_kv with
                | Some (Lt, _) -> begin
                    match f None with
                    | Some v ->
                      (collision_insert 0 (k, v) child elms_child_index node), 1
                    | None -> None, 0
                  end
                | Some (Eq, i) -> begin
                    (* Key already in collision node. *)
                    let _, v = Array.get i child.elms_kv in
                    match f (Some v) with
                    | Some v' ->
                      (collision_replace i (k, v') child elms_child_index node),
                      0
                    | None -> None, 0
                  end
                | Some (Gt, i) -> begin
                    match f None with
                    | Some v ->
                      (collision_insert (succ i) (k, v) child elms_child_index
                          node), 1
                    | None -> None, 0
                  end
                | None -> not_reached ()
              end
          end
      end
    | false -> begin
        let elms_kv_index = elm_index_of_bit node.present_kv present_bit in
        let kx, vx = Array.get elms_kv_index node.elms_kv in
        match Cmp.is_eq (cmper.cmp k kx) with
        | false -> begin
            (* Unequal key already present. *)
            match f None with
            | Some v -> begin
                let elms_child_index =
                  elm_index_of_bit node.present_child present_bit in
                let child = match level + 1 < max_height with
                  | false -> node_of_collision cmper (k, v) (kx, vx)
                  | true -> begin
                      (* Create a subtree which tries to disambiguate the two
                       * keys.  In the worst case, this creates a chain of nodes
                       * linking to a leaf of maximal depth containing a
                       * collision. *)
                      let kx_hash = Hash.State.seed |> cmper.hash_fold kx
                                    |> Hash.t_of_state in
                      disambiguate (k, v) k_hash (kx, vx) kx_hash cmper
                        (succ level)
                    end
                in
                let present_kv' = Bitset.bit_xor present_bit node.present_kv in
                let present_child' =
                  Bitset.bit_xor present_bit node.present_child in
                let elms_kv' = Array.remove elms_kv_index node.elms_kv in
                let elms_child' =
                  Array.insert elms_child_index child node.elms_child in
                Some {present_kv=present_kv'; present_child=present_child';
                  elms_kv=elms_kv'; elms_child=elms_child'}, 1
              end
            | None -> None, 0
          end
        | true -> begin
            (* Key already present. *)
            match f (Some vx) with
            | Some v' -> begin
                let elms_kv' = Array.set elms_kv_index (k, v') node.elms_kv in
                Some {node with elms_kv=elms_kv'}, 0
              end
            | None -> None, 0
          end
      end
  end in
  let k_hash = Hash.State.seed |> t.cmper.hash_fold k |> Hash.t_of_state in
  match fn k f k_hash t.cmper t.root 0 with
  | None, _ -> t
  | Some root', delta -> {t with length=(t.length + delta); root=root'}

let insert ~k ~v t =
  insert_impl k ~f:(function
    | None -> Some v
    | Some _ -> None
  ) t

let insert_hlt ~k ~v t =
  insert_impl k ~f:(function
    | None -> Some v
    | Some _ -> halt "Key already bound in map"
  ) t

let upsert ~k ~v t =
  insert_impl k ~f:(fun _ -> Some v) t

let update ~k ~v t =
  insert_impl k ~f:(function
    | None -> None
    | Some _ -> Some v
  ) t

let update_hlt ~k ~v t =
  insert_impl k ~f:(function
    | None -> halt "Key not bound in map"
    | Some _ -> Some v
  ) t

let of_alist m kvs =
  match kvs with
  | [] -> empty m
  | (k, v) :: kvs' -> begin
      let rec fn kvs map = begin
        match kvs with
        | [] -> map
        | (k, v) :: kvs' -> fn kvs' (insert_hlt ~k ~v map)
      end in
      fn kvs' (singleton m ~k ~v)
    end

let remove_impl a ~f t =
  let open Cmper in
  (* Collapse a child to a single binding. *)
  let collapse_child kv present_bit elms_child_index node = begin
    match node.elms_kv, node.elms_child with
    | [||], [|_|] -> Some (Kv kv)
    | _ -> begin
        let elms_kv_index = elm_index_of_bit node.present_kv present_bit in
        let present_kv' = Bitset.bit_xor present_bit node.present_kv in
        let present_child' = Bitset.bit_xor present_bit node.present_child in
        let elms_kv' = Array.insert elms_kv_index kv node.elms_kv in
        let elms_child' = Array.remove elms_child_index node.elms_child in
        let node' = {present_kv=present_kv'; present_child=present_child';
          elms_kv=elms_kv'; elms_child=elms_child'} in
        Some (Child node')
      end
  end in
  (* Remove binding from node; remove node if no children and one binding
   * remaining. *)
  let remove_kv present_bit elms_kv_index node = begin
    match node.elms_kv, node.elms_child with
    | [|_; _|], [||] -> begin
        let kv_index = match elms_kv_index with
          | 0 -> 1
          | 1 -> 0
          | _ -> not_reached ()
        in
        let kv = Array.get kv_index node.elms_kv in
        Some (Kv kv)
      end
    | _ -> begin
        let present_kv' = Bitset.bit_xor present_bit node.present_kv in
        let elms_kv' = Array.remove elms_kv_index node.elms_kv in
        let node' = {node with present_kv=present_kv'; elms_kv=elms_kv'} in
        Some (Child node')
      end
  end in
  let rec fn a a_hash ~f cmper node level = begin
    let present_index = present_index_at_level a_hash level in
    let present_bit = present_bit_of_index present_index in
    match Bitset.((bit_and present_bit node.present_kv) = zero) with
    | true -> begin
        match Bitset.((bit_and present_bit node.present_child) = zero) with
        | true -> (f None), None (* Key not bound; no-op. *)
        | false -> begin
            let elms_child_index =
              elm_index_of_bit node.present_child present_bit in
            let child = Array.get elms_child_index node.elms_child in
            match level + 1 < max_height with
            | true -> begin
                match fn a a_hash ~f cmper child (succ level) with
                | false, _ -> false, None
                | true, Some (Child child') -> begin
                    let elms_child' =
                      Array.set elms_child_index child' node.elms_child in
                    true, Some (Child {node with elms_child=elms_child'})
                  end
                | true, Some (Kv kv) ->
                  true, collapse_child kv present_bit elms_child_index node
                | true, None -> not_reached ()
              end
            | false -> begin
                (* Collision node. *)
                let k_ = k__of_k_kvs a child.elms_kv in
                match Array.search k_ ~cmp:(k_cmp cmper.cmp) child.elms_kv with
                | None -> (f None), None (* Key not bound; no-op. *)
                | Some found_index -> begin
                    let _, v = Array.get found_index child.elms_kv in
                    match f (Some v) with
                    | true -> begin
                        match child.elms_kv with
                        | [|_; _|] -> begin
                            (* No remaining collisions (single remaining
                             * binding). *)
                            let kv_index = match found_index with
                              | 0 -> 1
                              | 1 -> 0
                              | _ -> not_reached ()
                            in
                            let kv = Array.get kv_index child.elms_kv in
                            true,
                            collapse_child kv present_bit elms_child_index node
                          end
                        | _ -> begin
                            (* Remaining collisions. *)
                            let child_elms_kv' =
                              Array.remove found_index child.elms_kv in
                            let child' = {child with elms_kv=child_elms_kv'} in
                            let elms_child' = Array.set elms_child_index child'
                                node.elms_child in
                            true,
                            Some (Child {node with elms_child=elms_child'})
                          end
                      end
                    | false -> false, None
                  end
              end
          end
      end
    | false -> begin
        let elms_kv_index = elm_index_of_bit node.present_kv present_bit in
        let k, v = Array.get elms_kv_index node.elms_kv in
        match Cmp.is_eq (cmper.cmp a k) with
        | false -> (f None), None (* Key not bound; no-op. *)
        | true -> begin
            match f (Some v) with
            | true -> true, remove_kv present_bit elms_kv_index node
            | false -> false, None
          end
      end
  end in
  let a_hash = Hash.State.seed |> t.cmper.hash_fold a |> Hash.t_of_state in
  match fn a a_hash ~f t.cmper t.root 0 with
  | false, _ -> t
  | true, Some (Child node) -> {t with length=pred t.length; root=node}
  | true, Some (Kv (k, v)) -> begin
      (* Singleton. *)
      let k_hash = Hash.State.seed |> t.cmper.hash_fold k |> Hash.t_of_state in
      let present_index = present_index_at_level k_hash 0 in
      let present_kv = present_bit_of_index present_index in
      let root' = {present_kv; present_child=Bitset.zero;
        elms_kv=[|(k, v)|]; elms_child=[||]} in
      {t with length=1; root=root'}
    end
  | true, None -> begin
      (* Empty. *)
      let root' = {present_kv=Bitset.zero; present_child=Bitset.zero;
        elms_kv=[||]; elms_child=[||]} in
      {t with length=0; root=root'}
    end

let remove k t =
  remove_impl k ~f:(function
    | None -> false
    | Some _ -> true
  ) t

let remove_hlt k t =
  remove_impl k ~f:(function
    | None -> halt "Key not bound in map"
    | Some _ -> true
  ) t

let amend k ~f t =
  let v_opt = get k t in
  let v_opt' = f v_opt in
  match v_opt, v_opt' with
  | None, None -> t
  | None, Some v' -> insert_hlt ~k ~v:v' t
  | Some _, Some v' -> update_hlt ~k ~v:v' t
  | Some _, None -> remove_hlt k t

let fold_until ~init ~f t =
  let rec fn accum f node = begin
    let accum', until = Array.fold_until node.elms_kv ~init:(accum, false)
      ~f:(fun (accum, _) kv ->
        let accum', until = f accum kv in
        (accum', until), until
      )
    in
    match until with
    | false ->
      Array.fold_until node.elms_child ~init:(accum', until)
        ~f:(fun (accum, _) child ->
          let accum', until = fn accum f child in
          (accum', until), until
        )
    | true -> accum', until
  end in
  let accum, _ = fn init f t.root in
  accum

let fold ~init ~f t =
  fold_until ~init ~f:(fun accum a -> (f accum a), false) t

let iter ~f t =
  fold ~init:() ~f:(fun _ a -> f a) t

let hash_fold hash_fold_v t state =
  (* Fold ordering is not stable for unequal maps, but order permutation does
   * increase risk of accidental collision for maps, because each key occurs
   * precisely once regardless of order. *)
  let n, state' = fold t ~init:(0, state) ~f:(fun (i, state) (k, v) ->
    (succ i),
    (
      state
      |> Usize.hash_fold i
      |> t.cmper.hash_fold k
      |> hash_fold_v v
    )
  ) in
  state' |> Usize.hash_fold n

(* Seq.  Note that internal iteration via fold* traverses bindings, then
 * children, but that ordering is not stable, and cannot be used here.  External
 * iteration must do a stable in-order traversal, so that the orderings of
 * unequal maps are monotonic relative to their union. *)
module Seq_poly3_fold2 = struct
  type ('k, 'v, 'cmp) container = ('k, 'v, 'cmp) t
  type 'k key = 'k
  type 'v value = 'v
  type ('k, 'v) node_pos = {
    node: ('k, 'v) node;
    present_bit: Bitset.t;
  }
  type ('k, 'v, 'cmp) t = {
    ind: usize;
    len: usize;
    path: ('k, 'v) node_pos list;
  }

  let is_collision_node node =
    Bitset.(node.present_kv = max_value) &&
    Bitset.(node.present_child = max_value)

  let rec leftmost_path node path =
    match node.elms_kv, node.elms_child with
    | [||], [||] -> path
    | [||], [|_|] -> begin
        (* Tail optimization. *)
        let child = Array.get 0 node.elms_child in
        leftmost_path child path
      end
    | _ -> begin
        match is_collision_node node with
        | false -> begin
            let kv_present_index = Bitset.bit_ctz node.present_kv in
            let child_present_index = Bitset.bit_ctz node.present_child in
            match kv_present_index < elms_per_level &&
                  (kv_present_index < child_present_index) with
            | true -> begin
                let kv_present_bit = present_bit_of_index kv_present_index in
                {node; present_bit=kv_present_bit} :: path
              end
            | false -> begin
                let child_present_bit =
                  present_bit_of_index child_present_index in
                let node_pos = {node; present_bit=child_present_bit} in
                let child = Array.get 0 node.elms_child in
                leftmost_path child (node_pos :: path)
              end
          end
        | true -> begin
            (* Collision node. *)
            {node; present_bit=Bitset.one} :: path
          end
      end

  let kv_of_path = function
    | [] -> not_reached ()
    | node_pos :: _ ->
      let elm_ind =
        elm_index_of_bit node_pos.node.present_kv node_pos.present_bit in
      Array.get elm_ind node_pos.node.elms_kv

  let rec next_path = function
    | [] -> []
    | node_pos :: path_tl -> begin
        match is_collision_node node_pos.node with
        | false -> begin
            let mask =
              Bitset.(bit_not ((bit_sl ~shift:1 node_pos.present_bit) - one)) in
            let kv_present_index' =
              Bitset.(bit_ctz (bit_and mask node_pos.node.present_kv)) in
            let child_present_index' =
              Bitset.(bit_ctz (bit_and mask node_pos.node.present_child)) in
            match (kv_present_index' < elms_per_level) &&
                  (kv_present_index' < child_present_index') with
            | true -> begin
                let kv_present_bit' =
                  present_bit_of_index kv_present_index' in
                {node_pos with present_bit=kv_present_bit'} :: path_tl
              end
            | false -> begin
                match child_present_index' >= elms_per_level with
                | true -> next_path path_tl
                | false -> begin
                    let child_present_bit' =
                      present_bit_of_index child_present_index' in
                    let node_pos' =
                      {node_pos with present_bit=child_present_bit'} in
                    let path' = node_pos' :: path_tl in
                    let elm_ind = elm_index_of_bit node_pos.node.present_child
                        child_present_bit' in
                    let child = Array.get elm_ind node_pos.node.elms_child in
                    leftmost_path child path'
                  end
              end
          end
        | true -> begin
            (* Collision node. *)
            let elm_index' = succ (Bitset.bit_ctz node_pos.present_bit) in
            match elm_index' < Array.length node_pos.node.elms_kv with
            | false -> next_path path_tl
            | true -> begin
                let kv_present_bit' = present_bit_of_index elm_index' in
                let node_pos' = {node_pos with present_bit=kv_present_bit'} in
                node_pos' :: path_tl
              end
          end
      end

  let init map =
    {ind=0; len=map.length; path=leftmost_path map.root []}

  let index t =
    t.ind

  let length t =
    t.len

  let next t =
    assert (index t < length t);
    let kv = kv_of_path t.path in
    let t' = {t with ind=succ t.ind; path=next_path t.path} in
    kv, t'

  let next_opt t =
    match (index t) < (length t) with
    | false -> None
    | true -> Some (next t)

  let cmper = cmper

  let cmp cmper k0 k1 =
    let open Cmper in
    let open Cmp in
    let k0_hash = Hash.State.seed |> cmper.hash_fold k0 |> Hash.t_of_state in
    let k1_hash = Hash.State.seed |> cmper.hash_fold k1 |> Hash.t_of_state in
    match U128.cmp k0_hash k1_hash with
    | Lt -> Lt
    | Eq -> cmper.cmp k0 k1
    | Gt -> Gt
end
include Seq.Make_poly3_fold2(Seq_poly3_fold2)
module Seq = Seq_poly3_fold2

let equal veq t0 t1 =
  let open Cmp in
  let open Cmper in
  let vcmp veq v0 v1 = begin
    match veq v0 v1 with
    | false -> Lt
    | true -> Eq
  end in
  let rec fn kvcmp node0 node1 = begin
    Bitset.(node0.present_kv = node1.present_kv) &&
    Bitset.(node0.present_child = node1.present_child) &&
    Cmp.is_eq (Array.cmp kvcmp node0.elms_kv node1.elms_kv) &&
    Cmp.is_eq (Array.cmp (fun node0 node1 ->
      match fn kvcmp node0 node1 with
      | false -> Lt
      | true -> Eq
    ) node0.elms_child node1.elms_child)
  end in
  (* Check lengths first, since it's cheap and easy to do so. *)
  match (length t0) = (length t1) with
  | false -> false
  | true -> fn (kvcmp t0.cmper.cmp (vcmp veq)) t0.root t1.root

let subset veq t0 t1 =
  fold_until ~init:true ~f:(fun _ (k, v) ->
    match get k t0 with
    | None -> false, true
    | Some v0 -> begin
        match veq v0 v with
        | false -> false, true
        | true -> true, false
      end
  ) t1

let disjoint t0 t1 =
  let small, large = match (length t0) <= (length t1) with
    | true -> t0, t1
    | false -> t1, t0
  in
  fold_until ~init:true ~f:(fun _ (k, _) ->
    match get k large with
    | None -> true, false
    | Some _ -> false, true
  ) small

let union ~f t0 t1 =
  (* Initialize the union with the larger of the two input maps, in order to
   * minimize number of insertions. *)
  let small, big = match Cmp.is_le (cmp (length t0) (length t1)) with
    | true -> t0, t1
    | false -> t1, t0
  in
  fold2 ~init:big ~f:(fun accum kv_small_opt kv_big_opt ->
    match kv_small_opt, kv_big_opt with
    | Some (k, v), None -> insert_hlt ~k ~v accum
    | None, Some _ -> accum
    | Some (k, v0), Some (_, v1) -> update_hlt ~k ~v:(f k v0 v1) accum
    | None, None -> not_reached ()
  ) small big

let inter ~f t0 t1 =
  fold2 ~init:(empty (cmper_m t0)) ~f:(fun accum kv0_opt kv1_opt ->
    match kv0_opt, kv1_opt with
    | Some (k, v0), Some (_, v1) -> insert_hlt ~k ~v:(f k v0 v1) accum
    | Some _, None
    | None, Some _ -> accum
    | None, None -> not_reached ()
  ) t0 t1

let diff t0 t1 =
  fold2 ~init:t0 ~f:(fun accum kv0_opt kv1_opt ->
    match kv0_opt, kv1_opt with
    | Some (k, _), Some _ -> remove_hlt k accum
    | Some _, None
    | None, Some _ -> accum
    | None, None -> not_reached ()
  ) t0 t1

let count ~f t =
  fold ~init:0 ~f:(fun accum kv ->
    match f kv with
    | false -> accum
    | true -> succ accum
  ) t

let for_any ~f t =
  fold_until ~init:false ~f:(fun _ kv ->
    let until = f kv in
    until, until
  ) t

let for_all ~f t =
  fold_until ~init:true ~f:(fun _ kv ->
    let unless = f kv in
    let until = not unless in
    unless, until
  ) t

let find ~f t =
  fold_until ~init:None ~f:(fun _ kv ->
    match f kv with
    | true -> (Some kv), true
    | false -> None, false
  ) t

let find_map ~f t =
  fold_until ~init:None ~f:(fun _ kv ->
    match f kv with
    | Some a -> (Some a), true
    | None -> None, false
  ) t

let filter ~f t =
  fold ~init:(empty (cmper_m t)) ~f:(fun accum (k, v) ->
    match f (k, v) with
    | false -> accum
    | true -> insert_hlt ~k ~v accum
  ) t

let filter_map ~f t =
  fold ~init:(empty (cmper_m t)) ~f:(fun accum (k, v) ->
    match f (k, v) with
    | None -> accum
    | Some v2 -> insert_hlt ~k ~v:v2 accum
  ) t

let partition_tf ~f t =
  let e = empty (cmper_m t) in
  fold ~init:(e, e) ~f:(fun (t_map, f_map) (k, v) ->
    match f (k, v) with
    | true -> (insert_hlt ~k ~v t_map), f_map
    | false -> t_map, (insert_hlt ~k ~v f_map)
  ) t

let partition_map ~f t =
  let open Either in
  let a_map = empty (cmper_m t) in
  let b_map = empty (cmper_m t) in
  fold ~init:(a_map, b_map) ~f:(fun (a_map, b_map) (k, v) ->
    match f (k, v) with
    | First v2 -> (insert_hlt ~k ~v:v2 a_map), b_map
    | Second v3 -> a_map, (insert_hlt ~k ~v:v3 b_map)
  ) t

let kreduce ~f t =
  let reduce2 ~f (k0, v) (k1, _) = begin
    (f k0 k1), v
  end in
  let reduce_kvs ~reduce2 kvs = begin
    Array.reduce_hlt ~f:(fun kv0 kv1 -> reduce2 kv0 kv1) kvs
  end in
  let rec reduce_children ~reduce2 children = begin
    match Array.reduce_hlt ~f:(fun elm0 elm1 ->
      Kv (
        match elm0, elm1 with
        | Child child0, Child child1 -> reduce2 (reduce_node ~reduce2 child0)
          (reduce_node ~reduce2 child1)
        | Child child, Kv kv
        | Kv kv, Child child -> reduce2 (reduce_node ~reduce2 child) kv
        | Kv kv0, Kv kv1 -> reduce2 kv0 kv1
      )
    ) (Array.map ~f:(fun child -> Child child) children) with
    | Kv kv -> kv
    | Child child -> reduce_node ~reduce2 child
  end
  and reduce_node ~reduce2 node = begin
    match node.elms_kv, node.elms_child with
    | [||], [||] -> not_reached ()
    | _, [||] -> reduce_kvs ~reduce2 node.elms_kv
    | [||], _ -> reduce_children ~reduce2 node.elms_child
    | _ -> reduce2 (reduce_kvs ~reduce2 node.elms_kv)
      (reduce_children ~reduce2 node.elms_child)
  end in
  match t.length with
  | 0 -> None
  | _ -> begin
      let k, _ = reduce_node ~reduce2:(reduce2 ~f) t.root in
      Some k
    end

let kreduce_hlt ~f t =
  match kreduce ~f t with
  | None -> halt "Empty map"
  | Some k -> k

let reduce ~f t =
  let reduce2 ~f (k, v0) (_, v1) = begin
    k, (f v0 v1)
  end in
  let reduce_kvs ~reduce2 kvs = begin
    Array.reduce_hlt ~f:(fun kv0 kv1 -> reduce2 kv0 kv1) kvs
  end in
  let rec reduce_children ~reduce2 children = begin
    match Array.reduce_hlt ~f:(fun elm0 elm1 ->
      Kv (
        match elm0, elm1 with
        | Child child0, Child child1 -> reduce2 (reduce_node ~reduce2 child0)
          (reduce_node ~reduce2 child1)
        | Child child, Kv kv
        | Kv kv, Child child -> reduce2 (reduce_node ~reduce2 child) kv
        | Kv kv0, Kv kv1 -> reduce2 kv0 kv1
      )
    ) (Array.map ~f:(fun child -> Child child) children) with
    | Kv kv -> kv
    | Child child -> reduce_node ~reduce2 child
  end
  and reduce_node ~reduce2 node = begin
    match node.elms_kv, node.elms_child with
    | [||], [||] -> not_reached ()
    | _, [||] -> reduce_kvs ~reduce2 node.elms_kv
    | [||], _ -> reduce_children ~reduce2 node.elms_child
    | _ -> reduce2 (reduce_kvs ~reduce2 node.elms_kv)
      (reduce_children ~reduce2 node.elms_child)
  end in
  match t.length with
  | 0 -> None
  | _ -> begin
      let _, v = reduce_node ~reduce2:(reduce2 ~f) t.root in
      Some v
    end

let reduce_hlt ~f t =
  match reduce ~f t with
  | None -> halt "Empty map"
  | Some v -> v

let to_alist t =
  fold ~init:[] ~f:(fun accum kv -> kv :: accum) t

module Set_to_array = struct
  include Seq
  include Array.Seq.Make_poly3(Seq)
end

let to_array t =
  Set_to_array.(to_array (init t))

(******************************************************************************)
(* Begin tests. *)

let pp pp_v ppf t =
  let open Format in
  let rec pp_kvs ppf kvs = begin
    fprintf ppf "@[<v>elms_kv=[|";
    if Array.length kvs > 0 then fprintf ppf "@;<0 2>@[<v>";
    Array.iteri kvs ~f:(fun i (k, v) ->
      if i > 0 then fprintf ppf ";@,";
      fprintf ppf "@[<h>(%a,@ %a)@]" t.cmper.pp k pp_v v
    );
    if Array.length kvs > 0 then fprintf ppf "@]@,";
    fprintf ppf "|]@]"
  end
  and pp_children ppf children = begin
    fprintf ppf "@[<v>elms_child=[|";
    if Array.length children > 0 then fprintf ppf "@;<0 2>@[<v>";
    Array.iteri children ~f:(fun i child ->
      if i > 0 then fprintf ppf ";@,";
      fprintf ppf "%a" pp_node child
    );
    if Array.length children > 0 then fprintf ppf "@]@,";
    fprintf ppf "|]@]"
  end
  and pp_node ppf node = begin
    fprintf ppf "@[<v>present_kv=   %a;@,present_child=%a;@,%a;@,%a@]"
      Bitset.pp_x node.present_kv
      Bitset.pp_x node.present_child
      pp_kvs node.elms_kv
      pp_children node.elms_child
  end
  and pp_root ppf root = begin
    fprintf ppf "@[<v>root={@;<0 2>%a@,}@]" pp_node root
  end
  in
  fprintf ppf "@[<v>Map {@;<0 2>@[<v>length=%a;@,%a@]@,}@]"
    Usize.pp t.length
    pp_root t.root

let pp_kv pp_v ppf (k, v) =
  Format.fprintf ppf "(%a,@ %a)" Usize.pp k pp_v v

let validate t =
  let open Cmper in
  let is_collision_node node = begin
    Bitset.(node.present_kv = max_value) &&
    Bitset.(node.present_child = max_value)
  end in
  let rec fn t node level  = begin
    let () = Array.iter ~f:(fun (k, _) -> assert (mem k t)) node.elms_kv in
    match level < max_height with
    | true -> begin
        assert (not (is_collision_node node));
        let () = match node.elms_kv, node.elms_child with
          | [||], [||]
          | [|_|], [||] -> assert (level = 0)
          | _ -> ()
        in
        assert ((Bitset.bit_pop node.present_kv) = (Array.length node.elms_kv));
        assert ((Bitset.bit_pop node.present_child)
          = (Array.length node.elms_child));
        Array.fold ~init:(Array.length node.elms_kv) ~f:(fun accum child ->
          accum + (fn t child (succ level))
        ) node.elms_child
      end
    | false -> begin
        assert (level = max_height);
        assert (is_collision_node node);
        assert ((Array.length node.elms_kv) > 1);
        assert ((Array.length node.elms_child) = 0);
        assert (Array.is_sorted ~cmp:(k_cmp t.cmper.cmp) node.elms_kv);
        Array.length node.elms_kv
      end
  end in
  assert ((fn t t.root 0) = t.length)

(* Test comparator module for usize that uses unseeded hashing, with several
 * specially handled values for the purpose of collision testing.  This allows
 * deterministic hashing across test runs. *)
module UsizeTestCmper = struct
  type t = usize
  module T = struct
    type nonrec t = t
    let hash_fold a _state =
      match a with
      | 42 | 420 | 4200 -> Hash.State.of_u128 U128.zero
      | 421 ->
        (* Set the least significant consumed hash bit. *)
        Hash.State.of_u128
          (U128.bit_sl ~shift:(bits_per_hash % bits_per_level) U128.one)
      | _ -> Usize.hash_fold a Hash.State.empty
    let cmp = Usize.cmp
    let pp = Usize.pp
  end
  include Cmper.Make_mono(T)
end

let of_klist ks =
  List.fold ks ~init:(empty (module UsizeTestCmper)) ~f:(fun map k ->
    insert_hlt ~k ~v:(k * 100) map
  )

let veq v0 v1 =
  Cmp.is_eq (Usize.cmp v0 v1)

let merge k v0 v1 =
  assert Usize.(k * 100 = v0);
  assert (veq v0 v1);
  v0

let%expect_test "hash_fold" =
  let open Format in
  printf "@[";
  let rec fn = function
    | [] -> ()
    | l :: lists' -> begin
        let map = of_klist l in
        printf "hash_fold (of_klist %a) -> %a@\n"
          (List.pp Usize.pp) l
          Hash.pp (Hash.t_of_state
            (hash_fold Usize.hash_fold map Hash.State.empty));
        fn lists'
      end
  in
  (* NB: [0; 1] and [0; 2] collide.  This is because we're using UsizeTestCmper
   * to get stable test output; the hashing results from all but the last
   * binding hashed are discarded. *)
  let lists = [
    [];
    [0];
    [0; 1];
    [0; 2];
    [2; 3]
  ] in
  fn lists;
  printf "@]";

  [%expect{|
    hash_fold (of_klist []) -> 0xb465_a9ec_cd79_1cb6_4bbd_1bf2_7da9_18d6u128
    hash_fold (of_klist [0]) -> 0x53a3_5e44_9415_8ff4_24a3_88ce_df7b_e5a4u128
    hash_fold (of_klist [0; 1]) -> 0xcc6a_388f_cbb9_0721_e04c_974f_50b8_4baau128
    hash_fold (of_klist [0; 2]) -> 0xcc6a_388f_cbb9_0721_e04c_974f_50b8_4baau128
    hash_fold (of_klist [2; 3]) -> 0x9918_3447_d883_5d42_0a2c_4ef8_8fdd_b8deu128
    |}]

let%expect_test "hash_fold empty" =
  let hash_empty state = begin
    state
    |> hash_fold Unit.hash_fold (empty (module UsizeTestCmper))
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
  assert U128.((Hash.t_of_state e1) <> (Hash.t_of_state e2));

  [%expect{|
    |}]

let%expect_test "empty,cmper_m,singleton,length" =
  let open Format in
  printf "@[";
  let e = empty (module UsizeTestCmper) in
  validate e;
  assert (length e = 0);
  printf "%a@\n" (pp Unit.pp) e;

  let s = singleton (cmper_m e) ~k:0 ~v:"0" in
  validate s;
  assert (length s = 1);
  printf "%a@\n" (pp String.pp) s;
  printf "@]";

  [%expect{|
    Map {
      length=0;
      root={
        present_kv=   0x0000000000000000;
        present_child=0x0000000000000000;
        elms_kv=[||];
        elms_child=[||]
      }
    }
    Map {
      length=1;
      root={
        present_kv=   0x0000000000400000;
        present_child=0x0000000000000000;
        elms_kv=[|
          (0, "0")
        |];
        elms_child=[||]
      }
    }
    |}]

let%expect_test "mem,get,insert,subset" =
  let rec test ks map = begin
    match ks with
    | [] -> ()
    | k :: ks' -> begin
        assert (not (mem k map));
        assert (Option.is_none (get k map));
        let v = k * 100 in
        let map' = insert ~k ~v map in
        validate map';
        assert (mem k map');
        assert ((get_hlt k map') = v);
        assert (subset veq map' map);
        assert (not (subset veq map map'));
        test ks' map'
      end
  end in
  let ks = [1; 3; 2; 42; 44; 45; 56; 60; 66; 75; 81; 91; 420; 421; 4200] in
  test ks (empty (module UsizeTestCmper));

  [%expect{|
    |}]

let%expect_test "mem,get,insert,insert_hlt" =
  let rec test ks map = begin
    match ks with
    | [] -> ()
    | k :: ks' -> begin
        assert (not (mem k map));
        assert (Option.is_none (get k map));
        let v = k * 100 in
        let map' = insert_hlt ~k ~v map in
        assert (mem k map');
        assert ((get_hlt k map') = v);
        validate map';
        let v' = k * 10000 in
        let map'' = insert ~k ~v:v' map' in
        assert (mem k map'');
        assert ((get_hlt k map'') = v);
        validate map'';
        test ks' map''
      end
  end in
  let ks = [1; 3; 2; 42; 44; 45; 56; 60; 66; 75; 81; 91; 420; 421; 4200] in
  test ks (empty (module UsizeTestCmper));

  [%expect{|
    |}]

let%expect_test "mem,get,update,upsert,update_hlt,subset" =
  let rec test ks map = begin
    match ks with
    | [] -> ()
    | k :: ks' -> begin
        assert (not (mem k map));
        assert (Option.is_none (get k map));
        (* update (silently fail) *)
        let v = k * 100 in
        let map' = update ~k ~v map in
        assert (not (mem k map'));
        validate map';
        (* upsert *)
        let map'' = upsert ~k ~v map' in
        assert (mem k map'');
        assert ((get_hlt k map'') = v);
        validate map'';
        (* update_hlt *)
        let v' = k * 10000 in
        let map''' = update_hlt ~k ~v:v' map'' in
        assert (mem k map''');
        assert ((get_hlt k map''') = v');
        assert (not (subset veq map'' map'''));
        assert (not (subset veq map''' map''));
        validate map''';
        (* update *)
        let v'' = k * 1000000 in
        let map'''' = update ~k ~v:v'' map''' in
        assert (mem k map'''');
        assert ((get_hlt k map'''') = v'');
        validate map'''';
        test ks' map''''
      end
  end in
  let ks = [1; 3; 2; 42; 44; 45; 56; 60; 66; 75; 81; 91; 420; 421; 4200] in
  test ks (empty (module UsizeTestCmper));

  [%expect{|
    |}]

let%expect_test "mem,get,amend" =
  let rec test ks map = begin
    match ks with
    | [] -> ()
    | k :: ks' -> begin
        assert (not (mem k map));
        assert (Option.is_none (get k map));
        let v = k * 100 in
        let map' = amend k ~f:(function
          | None -> Some v
          | Some _ -> not_reached ()
        ) map in
        assert (mem k map');
        assert ((get_hlt k map') = v);
        validate map';
        let v' = k * 10000 in
        let map'' = amend k ~f:(function
          | Some vx -> begin
              assert (vx = v);
              Some v'
            end
          | None -> not_reached ()
        ) map' in
        assert (mem k map'');
        assert ((get_hlt k map'') = v');
        validate map'';
        test ks' map''
      end
  end in
  let ks = [1; 3; 2; 42; 44; 45; 56; 60; 66; 75; 81; 91; 420; 421; 4200] in
  test ks (empty (module UsizeTestCmper));

  [%expect{|
    |}]

let%expect_test "of_alist,remove" =
  let open Format in
  printf "@[";
  let test k map descr = begin
    validate map;
    printf "--- %s ---@\n" descr;
    let map' = remove k map in
    validate map';
    printf "@[<v>remove %a@;<0 2>@[<v>%a ->@,%a@]@]@\n"
      Usize.pp k (pp String.pp) map (pp String.pp) map'
  end in
  let test_tuples = [
    ([(0, "0"); (1, "1")], 2,           "Not member.");
    ([(0, "0")], 0,                     "Member, length 1 -> 0.");
    ([(0, "0"); (1, "1")], 1,           "Member, length 2 -> 1.");
    ([(0, "0"); (1, "1"); (2, "2")], 2, "Member, length 3 -> 2.");
  ] in
  List.iter test_tuples ~f:(fun (kvs, k, descr) ->
    let map = of_alist (module UsizeTestCmper) kvs in
    test k map descr
  );
  printf "@]";

  [%expect{|
    --- Not member. ---
    remove 2
      Map {
        length=2;
        root={
          present_kv=   0x0000000000400004;
          present_child=0x0000000000000000;
          elms_kv=[|
            (1, "1");
            (0, "0")
          |];
          elms_child=[||]
        }
      } ->
      Map {
        length=2;
        root={
          present_kv=   0x0000000000400004;
          present_child=0x0000000000000000;
          elms_kv=[|
            (1, "1");
            (0, "0")
          |];
          elms_child=[||]
        }
      }
    --- Member, length 1 -> 0. ---
    remove 0
      Map {
        length=1;
        root={
          present_kv=   0x0000000000400000;
          present_child=0x0000000000000000;
          elms_kv=[|
            (0, "0")
          |];
          elms_child=[||]
        }
      } ->
      Map {
        length=0;
        root={
          present_kv=   0x0000000000000000;
          present_child=0x0000000000000000;
          elms_kv=[||];
          elms_child=[||]
        }
      }
    --- Member, length 2 -> 1. ---
    remove 1
      Map {
        length=2;
        root={
          present_kv=   0x0000000000400004;
          present_child=0x0000000000000000;
          elms_kv=[|
            (1, "1");
            (0, "0")
          |];
          elms_child=[||]
        }
      } ->
      Map {
        length=1;
        root={
          present_kv=   0x0000000000400000;
          present_child=0x0000000000000000;
          elms_kv=[|
            (0, "0")
          |];
          elms_child=[||]
        }
      }
    --- Member, length 3 -> 2. ---
    remove 2
      Map {
        length=3;
        root={
          present_kv=   0x0000000000500004;
          present_child=0x0000000000000000;
          elms_kv=[|
            (1, "1");
            (2, "2");
            (0, "0")
          |];
          elms_child=[||]
        }
      } ->
      Map {
        length=2;
        root={
          present_kv=   0x0000000000400004;
          present_child=0x0000000000000000;
          elms_kv=[|
            (1, "1");
            (0, "0")
          |];
          elms_child=[||]
        }
      }
    |}]

let%expect_test "of_alist,remove_hlt" =
  let open Format in
  printf "@[";
  let test k map descr = begin
    validate map;
    printf "--- %s ---@\n" descr;
    let map' = remove_hlt k map in
    validate map';
    printf "@[<v>remove_hlt %a@;<0 2>@[<v>%a ->@,%a@]@]@\n"
      Usize.pp k (pp String.pp) map (pp String.pp) map'
  end in
  let test_tuples = [
    ([(0, "0")], 0,                     "Member, length 1 -> 0.");
    ([(0, "0"); (1, "1")], 1,           "Member, length 2 -> 1.");
    ([(0, "0"); (1, "1"); (2, "2")], 2, "Member, length 3 -> 2.");
  ] in
  List.iter test_tuples ~f:(fun (kvs, k, descr) ->
    let map = of_alist (module UsizeTestCmper) kvs in
    test k map descr
  );
  printf "@]";

  [%expect{|
    --- Member, length 1 -> 0. ---
    remove_hlt 0
      Map {
        length=1;
        root={
          present_kv=   0x0000000000400000;
          present_child=0x0000000000000000;
          elms_kv=[|
            (0, "0")
          |];
          elms_child=[||]
        }
      } ->
      Map {
        length=0;
        root={
          present_kv=   0x0000000000000000;
          present_child=0x0000000000000000;
          elms_kv=[||];
          elms_child=[||]
        }
      }
    --- Member, length 2 -> 1. ---
    remove_hlt 1
      Map {
        length=2;
        root={
          present_kv=   0x0000000000400004;
          present_child=0x0000000000000000;
          elms_kv=[|
            (1, "1");
            (0, "0")
          |];
          elms_child=[||]
        }
      } ->
      Map {
        length=1;
        root={
          present_kv=   0x0000000000400000;
          present_child=0x0000000000000000;
          elms_kv=[|
            (0, "0")
          |];
          elms_child=[||]
        }
      }
    --- Member, length 3 -> 2. ---
    remove_hlt 2
      Map {
        length=3;
        root={
          present_kv=   0x0000000000500004;
          present_child=0x0000000000000000;
          elms_kv=[|
            (1, "1");
            (2, "2");
            (0, "0")
          |];
          elms_child=[||]
        }
      } ->
      Map {
        length=2;
        root={
          present_kv=   0x0000000000400004;
          present_child=0x0000000000000000;
          elms_kv=[|
            (1, "1");
            (0, "0")
          |];
          elms_child=[||]
        }
      }
    |}]

let%expect_test "of_alist,to_alist" =
  let open Format in
  printf "@[<h>";
  let test kvs = begin
    let map = of_alist (module UsizeTestCmper) kvs in
    printf "of_alist %a; to_alist -> %a\n"
      (List.pp (pp_kv String.pp)) kvs
      (List.pp (pp_kv String.pp)) (to_alist map)
  end in
  let test_alists = [
    [];
    [(0, "0")];
    [(0, "0"); (1, "1")];
    [(0, "0"); (1, "1"); (2, "2")];
    [(0, "0"); (1, "1"); (66, "66")];
    [(0, "0"); (1, "1"); (66, "66"); (91, "91")];
  ] in
  List.iter test_alists ~f:(fun kvs ->
    test kvs
  );
  printf "@]";

  [%expect{|
    of_alist []; to_alist -> []
    of_alist [(0, "0")]; to_alist -> [(0, "0")]
    of_alist [(0, "0"); (1, "1")]; to_alist -> [(0, "0"); (1, "1")]
    of_alist [(0, "0"); (1, "1"); (2, "2")]; to_alist -> [(0, "0"); (2, "2"); (1, "1")]
    of_alist [(0, "0"); (1, "1"); (66, "66")]; to_alist -> [(1, "1"); (66, "66"); (0, "0")]
    of_alist [(0, "0"); (1, "1"); (66, "66"); (91, "91")]; to_alist -> [(1, "1"); (91, "91"); (66, "66"); (0, "0")]
    |}]

let%expect_test "choose_hlt" =
  let open Format in
  printf "@[";
  (* test is n^2 time complexity, so keep n small. *)
  let rec test n i map = begin
    match i < n with
    | false -> map
    | true -> begin
        validate map;
        let map' = test n (succ i) (insert_hlt ~k:i ~v:(i * 100) map) in
        let k, v = choose_hlt map' in
        assert (k * 100 = v);
        let map'' = remove_hlt k map' in
        validate map'';
        assert ((length map') = (length map'') + 1);
        map''
      end
  end in
  let e = empty (module UsizeTestCmper) in
  let _ = test 100 0 e in
  printf "@]";

  [%expect{|
    |}]

let%expect_test "fold_until" =
  let test ks = begin
    let map = of_klist ks in
    (* Compute the number of elements in the triangle defined by folding n
     * times, each time terminating upon encounter of a distinct key.  The size
     * of the triangle is insensitive to fold order. *)
    assert ((List.length ks) = (length map));
    let n = length map in
    let triangle_sum = List.fold ks ~init:0 ~f:(fun accum k ->
      accum + fold_until map ~init:0 ~f:(fun accum (k1, _) ->
        (succ accum), (k = k1)
      )
    ) in
    assert (triangle_sum = (n + 1) * n / 2);
  end in
  let test_lists = [
    [];
    [0];
    [0; 1];
    [0; 1; 2];
    [0; 1; 66];
    [0; 1; 66; 91];
    [42; 420];
    [42; 420; 421];
    [42; 420; 4200];
  ] in
  List.iter test_lists ~f:(fun ks ->
    test ks
  );

  [%expect{|
    |}]

let%expect_test "fold2_until" =
  let test ks0 ks1 = begin
    let map0 = of_klist ks0 in
    let map1 = of_klist ks1 in
    let map = union ~f:merge map0 map1 in
    let kvs = to_alist map in
    (* Compute the number of elements in the triangle defined by folding n
     * times, each time terminating upon encounter of a distinct key.  The size
     * of the triangle is insensitive to fold order. *)
    assert ((List.length kvs) = (length map));
    let n = length map in
    let triangle_sum = List.fold kvs ~init:0 ~f:(fun accum (k, _) ->
      accum + fold2_until map0 map1 ~init:0
          ~f:(fun accum kv0_opt kv1_opt ->
            match kv0_opt, kv1_opt with
            | Some (kx, _), Some _
            | Some (kx, _), None
            | None, Some (kx, _) -> (succ accum), (k = kx)
            | None, None -> not_reached ()
          )
    ) in
    assert (triangle_sum = (n + 1) * n / 2);
  end in
  let test_lists = [
    [];
    [0];
    [0; 1];
    [0; 1; 2];
    [0; 1; 66];
    [0; 1; 66; 91];
    [42; 420];
    [42; 420; 421];
    [42; 420; 4200];
  ] in
  List.iteri test_lists ~f:(fun i ks0 ->
    List.iteri test_lists ~f:(fun j ks1 ->
      if i <= j then test ks0 ks1
    )
  );

  [%expect{|
    |}]

let%expect_test "fold2" =
  let open Format in
  printf "@[";
  let pp_pair ppf (kv0_opt, kv1_opt) = begin
    fprintf ppf "(%a, %a)"
      (Option.pp (pp_kv Usize.pp)) kv0_opt
      (Option.pp (pp_kv Usize.pp)) kv1_opt
  end in
  let test ks0 ks1 = begin
    let map0 = of_klist ks0 in
    let map1 = of_klist ks1 in
    let pairs = fold2 ~init:[] ~f:(fun accum kv0_opt kv1_opt ->
      (kv0_opt, kv1_opt) :: accum
    ) map0 map1 in
    printf "fold2 %a %a -> %a@\n"
      (List.pp Usize.pp) ks0
      (List.pp Usize.pp) ks1
      (List.pp pp_pair) pairs
  end in
  let test_lists = [
    [];
    [0];
    [0; 1];
    [0; 1; 2];
    [0; 1; 66];
    [0; 1; 66; 91];
    [42; 420];
    [42; 420; 421];
    [42; 420; 4200];
  ] in
  List.iteri test_lists ~f:(fun i ks0 ->
    List.iteri test_lists ~f:(fun j ks1 ->
      if i <= j then test ks0 ks1
    )
  );
  printf "@]";

  [%expect{|
    fold2 [] [] -> []
    fold2 [] [0] -> [(None, Some (0, 0))]
    fold2 [] [0; 1] -> [(None, Some (0, 0)); (None, Some (1, 100))]
    fold2 [] [0; 1; 2] -> [(None, Some (0, 0)); (None, Some (2, 200)); (None, Some (1, 100))]
    fold2 [] [0; 1; 66] -> [(None, Some (0, 0)); (None, Some (1, 100)); (None, Some (66, 6600))]
    fold2 [] [0; 1; 66; 91] -> [(None, Some (0, 0)); (None, Some (1, 100)); (None, Some (91, 9100)); (None, Some (66, 6600))]
    fold2 [] [42; 420] -> [(None, Some (420, 42000)); (None, Some (42, 4200))]
    fold2 [] [42; 420; 421] -> [(None, Some (421, 42100)); (None, Some (420, 42000)); (None, Some (42, 4200))]
    fold2 [] [42; 420; 4200] -> [(None, Some (4200, 420000)); (None, Some (420, 42000)); (None, Some (42, 4200))]
    fold2 [0] [0] -> [(Some (0, 0), Some (0, 0))]
    fold2 [0] [0; 1] -> [(Some (0, 0), Some (0, 0)); (None, Some (1, 100))]
    fold2 [0] [0; 1; 2] -> [(Some (0, 0), Some (0, 0)); (None, Some (2, 200)); (None, Some (1, 100))]
    fold2 [0] [0; 1; 66] -> [(Some (0, 0), Some (0, 0)); (None, Some (1, 100)); (None, Some (66, 6600))]
    fold2 [0] [0; 1; 66; 91] -> [(Some (0, 0), Some (0, 0)); (None, Some (1, 100)); (None, Some (91, 9100)); (None, Some (66, 6600))]
    fold2 [0] [42; 420] -> [(Some (0, 0), None); (None, Some (420, 42000)); (None, Some (42, 4200))]
    fold2 [0] [42; 420; 421] -> [(Some (0, 0), None); (None, Some (421, 42100)); (None, Some (420, 42000)); (None, Some (42, 4200))]
    fold2 [0] [42; 420; 4200] -> [(Some (0, 0), None); (None, Some (4200, 420000)); (None, Some (420, 42000)); (None, Some (42, 4200))]
    fold2 [0; 1] [0; 1] -> [(Some (0, 0), Some (0, 0)); (Some (1, 100), Some (1, 100))]
    fold2 [0; 1] [0; 1; 2] -> [(Some (0, 0), Some (0, 0)); (None, Some (2, 200)); (Some (1, 100), Some (1, 100))]
    fold2 [0; 1] [0; 1; 66] -> [(Some (0, 0), Some (0, 0)); (Some (1, 100), Some (1, 100)); (None, Some (66, 6600))]
    fold2 [0; 1] [0; 1; 66; 91] -> [(Some (0, 0), Some (0, 0)); (Some (1, 100), Some (1, 100)); (None, Some (91, 9100)); (None, Some (66, 6600))]
    fold2 [0; 1] [42; 420] -> [(Some (0, 0), None); (Some (1, 100), None); (None, Some (420, 42000)); (None, Some (42, 4200))]
    fold2 [0; 1] [42; 420; 421] -> [(Some (0, 0), None); (Some (1, 100), None); (None, Some (421, 42100)); (None, Some (420, 42000)); (None, Some (42, 4200))]
    fold2 [0; 1] [42; 420; 4200] -> [(Some (0, 0), None); (Some (1, 100), None); (None, Some (4200, 420000)); (None, Some (420, 42000)); (None, Some (42, 4200))]
    fold2 [0; 1; 2] [0; 1; 2] -> [(Some (0, 0), Some (0, 0)); (Some (2, 200), Some (2, 200)); (Some (1, 100), Some (1, 100))]
    fold2 [0; 1; 2] [0; 1; 66] -> [(Some (0, 0), Some (0, 0)); (Some (2, 200), None); (Some (1, 100), Some (1, 100)); (None, Some (66, 6600))]
    fold2 [0; 1; 2] [0; 1; 66; 91] -> [(Some (0, 0), Some (0, 0)); (Some (2, 200), None); (Some (1, 100), Some (1, 100)); (None, Some (91, 9100)); (None, Some (66, 6600))]
    fold2 [0; 1; 2] [42; 420] -> [(Some (0, 0), None); (Some (2, 200), None); (Some (1, 100), None); (None, Some (420, 42000)); (None, Some (42, 4200))]
    fold2 [0; 1; 2] [42; 420; 421] -> [(Some (0, 0), None); (Some (2, 200), None); (Some (1, 100), None); (None, Some (421, 42100)); (None, Some (420, 42000)); (None, Some (42, 4200))]
    fold2 [0; 1; 2] [42; 420; 4200] -> [(Some (0, 0), None); (Some (2, 200), None); (Some (1, 100), None); (None, Some (4200, 420000)); (None, Some (420, 42000)); (None, Some (42, 4200))]
    fold2 [0; 1; 66] [0; 1; 66] -> [(Some (0, 0), Some (0, 0)); (Some (1, 100), Some (1, 100)); (Some (66, 6600), Some (66, 6600))]
    fold2 [0; 1; 66] [0; 1; 66; 91] -> [(Some (0, 0), Some (0, 0)); (Some (1, 100), Some (1, 100)); (None, Some (91, 9100)); (Some (66, 6600), Some (66, 6600))]
    fold2 [0; 1; 66] [42; 420] -> [(Some (0, 0), None); (Some (1, 100), None); (Some (66, 6600), None); (None, Some (420, 42000)); (None, Some (42, 4200))]
    fold2 [0; 1; 66] [42; 420; 421] -> [(Some (0, 0), None); (Some (1, 100), None); (Some (66, 6600), None); (None, Some (421, 42100)); (None, Some (420, 42000)); (None, Some (42, 4200))]
    fold2 [0; 1; 66] [42; 420; 4200] -> [(Some (0, 0), None); (Some (1, 100), None); (Some (66, 6600), None); (None, Some (4200, 420000)); (None, Some (420, 42000)); (None, Some (42, 4200))]
    fold2 [0; 1; 66; 91] [0; 1; 66; 91] -> [(Some (0, 0), Some (0, 0)); (Some (1, 100), Some (1, 100)); (Some (91, 9100), Some (91, 9100)); (Some (66, 6600), Some (66, 6600))]
    fold2 [0; 1; 66; 91] [42; 420] -> [(Some (0, 0), None); (Some (1, 100), None); (Some (91, 9100), None); (Some (66, 6600), None); (None, Some (420, 42000)); (None, Some (42, 4200))]
    fold2 [0; 1; 66; 91] [42; 420; 421] -> [(Some (0, 0), None); (Some (1, 100), None); (Some (91, 9100), None); (Some (66, 6600), None); (None, Some (421, 42100)); (None, Some (420, 42000)); (None, Some (42, 4200))]
    fold2 [0; 1; 66; 91] [42; 420; 4200] -> [(Some (0, 0), None); (Some (1, 100), None); (Some (91, 9100), None); (Some (66, 6600), None); (None, Some (4200, 420000)); (None, Some (420, 42000)); (None, Some (42, 4200))]
    fold2 [42; 420] [42; 420] -> [(Some (420, 42000), Some (420, 42000)); (Some (42, 4200), Some (42, 4200))]
    fold2 [42; 420] [42; 420; 421] -> [(None, Some (421, 42100)); (Some (420, 42000), Some (420, 42000)); (Some (42, 4200), Some (42, 4200))]
    fold2 [42; 420] [42; 420; 4200] -> [(None, Some (4200, 420000)); (Some (420, 42000), Some (420, 42000)); (Some (42, 4200), Some (42, 4200))]
    fold2 [42; 420; 421] [42; 420; 421] -> [(Some (421, 42100), Some (421, 42100)); (Some (420, 42000), Some (420, 42000)); (Some (42, 4200), Some (42, 4200))]
    fold2 [42; 420; 421] [42; 420; 4200] -> [(Some (421, 42100), None); (None, Some (4200, 420000)); (Some (420, 42000), Some (420, 42000)); (Some (42, 4200), Some (42, 4200))]
    fold2 [42; 420; 4200] [42; 420; 4200] -> [(Some (4200, 420000), Some (4200, 420000)); (Some (420, 42000), Some (420, 42000)); (Some (42, 4200), Some (42, 4200))]
    |}]

let%expect_test "iter2,equal,subset,disjoint" =
  let open Format in
  printf "@[";
  let test_equal ks0 ks1 = begin
    let map0 = of_klist ks0 in
    let map1 = of_klist ks1 in
    assert (equal veq map0 map1);
    assert (subset veq map0 map1);
    assert (subset veq map1 map0);
    assert ((length map0 = 0) || (not (disjoint map0 map1)));
    iter2 ~f:(fun kv0_opt kv1_opt ->
      match kv0_opt, kv1_opt with
      | Some _, Some _ -> ()
      | None, Some _
      | Some _, None -> begin
          printf "Should be equal:@,%a@,%a@\n"
            (pp Usize.pp) map0 (pp Usize.pp) map1;
          assert false;
        end
      | None, None -> not_reached ()
    ) map0 map1
  end in
  let test_disjoint ks0 ks1 = begin
    let map0 = of_klist ks0 in
    let map1 = of_klist ks1 in
    assert (not (equal veq map0 map1));
    assert (not (subset veq map0 map1));
    assert ((length map0 = 0) || (not (subset veq map1 map0)));
    assert (disjoint map0 map1);
    iter2 ~f:(fun kv0_opt kv1_opt ->
      match kv0_opt, kv1_opt with
      | Some _, Some _ -> begin
          printf "Should be disjoint:@,%a@,%a@\n"
            (pp Usize.pp) map0 (pp Usize.pp) map1;
          assert false;
        end
      | None, Some _
      | Some _, None -> ()
      | None, None -> not_reached ()
    ) map0 map1
  end in
  let test_lists = [
    [];
    [0];
    [0; 1];
    [0; 1; 2];
    [0; 1; 66];
    [0; 1; 66; 91];
    [42; 420];
    [42; 420; 421];
    [42; 420; 4200];
  ] in
  let test_disjoint_list_pairs = [
    ([], [0]);
    ([0], [1]);
    ([0], [1; 2]);
    ([0; 1], [2; 3]);
    ([0; 1], [2; 3; 4]);
    ([0; 1; 2], [3; 4; 5])
  ] in
  List.iter test_lists ~f:(fun ks ->
    test_equal ks ks;
    test_equal ks (List.rev ks);
    test_equal (List.rev ks) ks;
    test_equal (List.rev ks) (List.rev ks);
  );
  List.iter test_disjoint_list_pairs ~f:(fun (ks0, ks1) ->
    test_disjoint ks0 ks1;
    test_disjoint ks0 (List.rev ks1);
    test_disjoint (List.rev ks0) ks1;
    test_disjoint (List.rev ks0) (List.rev ks1);
  );
  printf "@]";

  [%expect{|
    |}]

let%expect_test "union" =
  let test ks0 ks1 = begin
    let map0 = of_klist ks0 in
    let map1 = of_klist ks1 in
    let map = union ~f:merge map0 map1 in
    let kvs = to_alist map in
    List.iter ks0 ~f:(fun k -> assert ((mem k map) && (mem k map0)));
    List.iter ks1 ~f:(fun k -> assert ((mem k map) && (mem k map1)));
    List.iter kvs ~f:(fun (k, _) ->
      assert ((mem k map0) || (mem k map1)));
  end in
  let test_disjoint ks0 ks1 = begin
    let map0 = of_klist ks0 in
    let map1 = of_klist ks1 in
    let map = union ~f:merge map0 map1 in
    assert ((length map) = (length map0) + (length map1));
  end in
  let test_lists = [
    [];
    [0];
    [0; 1];
    [0; 1; 2];
    [0; 1; 66];
    [0; 1; 66; 91];
    [42; 420];
    [42; 420; 421];
    [42; 420; 4200];
  ] in
  let test_disjoint_list_pairs = [
    ([], [0]);
    ([0], [1]);
    ([0], [1; 2]);
    ([0; 1], [2; 3]);
    ([0; 1], [2; 3; 4]);
    ([0; 1; 2], [3; 4; 5])
  ] in
  List.iteri test_lists ~f:(fun i ks0 ->
    List.iteri test_lists ~f:(fun j ks1 ->
      if i <= j then begin
        test ks0 ks1;
        test ks1 ks0
      end
    )
  );
  List.iter test_disjoint_list_pairs ~f:(fun (ks0, ks1) ->
    test_disjoint ks0 ks1;
    test_disjoint ks0 (List.rev ks1);
    test_disjoint (List.rev ks0) ks1;
    test_disjoint (List.rev ks0) (List.rev ks1);
  );

  [%expect{|
    |}]

let%expect_test "inter" =
  let test ks0 ks1 = begin
    let map0 = of_klist ks0 in
    let map1 = of_klist ks1 in
    let map = inter ~f:merge map0 map1 in
    let kvs = to_alist map in
    List.iter ks0 ~f:(fun k ->
      assert ((mem k map) || (not (mem k map1))));
    List.iter ks1 ~f:(fun k ->
      assert ((mem k map) || (not (mem k map0))));
    List.iter kvs ~f:(fun (k, _) ->
      assert ((mem k map0) && (mem k map1)));
  end in
  let test_disjoint ks0 ks1 = begin
    let map0 = of_klist ks0 in
    let map1 = of_klist ks1 in
    let map = inter ~f:merge map0 map1 in
    assert ((length map) = 0);
  end in
  let test_lists = [
    [];
    [0];
    [0; 1];
    [0; 1; 2];
    [0; 1; 66];
    [0; 1; 66; 91];
    [42; 420];
    [42; 420; 421];
    [42; 420; 4200];
  ] in
  let test_disjoint_list_pairs = [
    ([], [0]);
    ([0], [1]);
    ([0], [1; 2]);
    ([0; 1], [2; 3]);
    ([0; 1], [2; 3; 4]);
    ([0; 1; 2], [3; 4; 5])
  ] in
  List.iteri test_lists ~f:(fun i ks0 ->
    List.iteri test_lists ~f:(fun j ks1 ->
      if i <= j then begin
        test ks0 ks1;
        test ks1 ks0
      end
    )
  );
  List.iter test_disjoint_list_pairs ~f:(fun (ks0, ks1) ->
    test_disjoint ks0 ks1;
    test_disjoint ks0 (List.rev ks1);
    test_disjoint (List.rev ks0) ks1;
    test_disjoint (List.rev ks0) (List.rev ks1);
  );

  [%expect{|
    |}]

let%expect_test "diff" =
  let test ks0 ks1 = begin
    let map0 = of_klist ks0 in
    let map1 = of_klist ks1 in
    let map = diff map0 map1 in
    let kvs = to_alist map in
    List.iter ks0 ~f:(fun k -> assert ((mem k map) || (mem k map1)));
    List.iter ks1 ~f:(fun k -> assert (not (mem k map)));
    List.iter kvs ~f:(fun (k, _) ->
      assert ((mem k map0) && (not (mem k map1))));
  end in
  let test_disjoint ks0 ks1 = begin
    let map0 = of_klist ks0 in
    let map1 = of_klist ks1 in
    let map = diff map0 map1 in
    assert ((length map) = (length map0));
  end in
  let test_lists = [
    [];
    [0];
    [0; 1];
    [0; 1; 2];
    [0; 1; 66];
    [0; 1; 66; 91];
    [42; 420];
    [42; 420; 421];
    [42; 420; 4200];
  ] in
  let test_disjoint_list_pairs = [
    ([], [0]);
    ([0], [1]);
    ([0], [1; 2]);
    ([0; 1], [2; 3]);
    ([0; 1], [2; 3; 4]);
    ([0; 1; 2], [3; 4; 5])
  ] in
  List.iteri test_lists ~f:(fun i ks0 ->
    List.iteri test_lists ~f:(fun j ks1 ->
      if i <= j then begin
        test ks0 ks1;
        test ks1 ks0;
      end
    )
  );
  List.iter test_disjoint_list_pairs ~f:(fun (ks0, ks1) ->
    test_disjoint ks0 ks1;
    test_disjoint ks0 (List.rev ks1);
    test_disjoint (List.rev ks0) ks1;
    test_disjoint (List.rev ks0) (List.rev ks1);
  );

  [%expect{|
    |}]

let%expect_test "filter" =
  let open Format in
  printf "@[<h>";
  let test ks = begin
    let map = of_klist ks in
    let map' = filter map ~f:(fun (k, _) -> k % 2 = 0) in
    let kvs = to_alist map' in
    printf "%a -> %a@\n"
      (List.pp Usize.pp) ks
      (List.pp (pp_kv Usize.pp)) kvs
  end in
  for n = 0 to 6 do
    let ks = Array.(to_list (init n ~f:(fun i -> i))) in
    test ks
  done;
  printf "@]";

  [%expect{|
    [] -> []
    [0] -> [(0, 0)]
    [0; 1] -> [(0, 0)]
    [0; 1; 2] -> [(0, 0); (2, 200)]
    [0; 1; 2; 3] -> [(0, 0); (2, 200)]
    [0; 1; 2; 3; 4] -> [(0, 0); (2, 200); (4, 400)]
    [0; 1; 2; 3; 4; 5] -> [(0, 0); (2, 200); (4, 400)]
    |}]

let%expect_test "filter_map" =
  let open Format in
  printf "@[<h>";
  let test ks = begin
    let map = of_klist ks in
    let map' = filter_map map ~f:(fun (k, v) ->
      match k % 2 = 0 with
      | true -> Some (Usize.to_string v)
      | false -> None
    ) in
    let kvs = to_alist map' in
    printf "%a -> %a@\n"
      (List.pp Usize.pp) ks
      (List.pp (pp_kv String.pp)) kvs
  end in
  for n = 0 to 6 do
    let ks = Array.(to_list (init n ~f:(fun i -> i))) in
    test ks
  done;
  printf "@]";

  [%expect{|
    [] -> []
    [0] -> [(0, "0")]
    [0; 1] -> [(0, "0")]
    [0; 1; 2] -> [(0, "0"); (2, "200")]
    [0; 1; 2; 3] -> [(0, "0"); (2, "200")]
    [0; 1; 2; 3; 4] -> [(0, "0"); (2, "200"); (4, "400")]
    [0; 1; 2; 3; 4; 5] -> [(0, "0"); (2, "200"); (4, "400")]
    |}]

let%expect_test "partition_tf" =
  let open Format in
  printf "@[<h>";
  let test ks = begin
    let map = of_klist ks in
    let t_map, f_map = partition_tf map ~f:(fun (k, _) -> k % 2 = 0) in
    let t_kvs = to_alist t_map in
    let f_kvs = to_alist f_map in
    printf "%a -> %a / %a@\n"
      (List.pp Usize.pp) ks
      (List.pp (pp_kv Usize.pp)) t_kvs
      (List.pp (pp_kv Usize.pp)) f_kvs
  end in
  for n = 0 to 6 do
    let ks = Array.(to_list (init n ~f:(fun i -> i))) in
    test ks
  done;
  printf "@]";

  [%expect{|
    [] -> [] / []
    [0] -> [(0, 0)] / []
    [0; 1] -> [(0, 0)] / [(1, 100)]
    [0; 1; 2] -> [(0, 0); (2, 200)] / [(1, 100)]
    [0; 1; 2; 3] -> [(0, 0); (2, 200)] / [(1, 100); (3, 300)]
    [0; 1; 2; 3; 4] -> [(0, 0); (2, 200); (4, 400)] / [(1, 100); (3, 300)]
    [0; 1; 2; 3; 4; 5] -> [(0, 0); (2, 200); (4, 400)] / [(5, 500); (1, 100); (3, 300)]
    |}]

let%expect_test "partition_map" =
  let open Format in
  printf "@[<h>";
  let test ks = begin
    let map = of_klist ks in
    let a_map, b_map = partition_map map ~f:(fun (k, v) ->
      match k % 2 = 0 with
      | true -> First (Usize.to_string v)
      | false -> Second (Usize.to_isize v)
    ) in
    let a_kvs = to_alist a_map in
    let b_kvs = to_alist b_map in
    printf "%a -> %a / %a@\n"
      (List.pp Usize.pp) ks
      (List.pp (pp_kv String.pp)) a_kvs
      (List.pp (pp_kv Isize.pp)) b_kvs
  end in
  for n = 0 to 6 do
    let ks = Array.(to_list (init n ~f:(fun i -> i))) in
    test ks
  done;
  printf "@]";

  [%expect{|
    [] -> [] / []
    [0] -> [(0, "0")] / []
    [0; 1] -> [(0, "0")] / [(1, 100i)]
    [0; 1; 2] -> [(0, "0"); (2, "200")] / [(1, 100i)]
    [0; 1; 2; 3] -> [(0, "0"); (2, "200")] / [(1, 100i); (3, 300i)]
    [0; 1; 2; 3; 4] -> [(0, "0"); (2, "200"); (4, "400")] / [(1, 100i); (3, 300i)]
    [0; 1; 2; 3; 4; 5] -> [(0, "0"); (2, "200"); (4, "400")] / [(5, 500i); (1, 100i); (3, 300i)]
    |}]

let%expect_test "kreduce" =
  let open Format in
  printf "@[<h>";
  let test ks = begin
    let map = of_klist ks in
    let sum = kreduce ~f:( + ) map in
    printf "kreduce ~f:( + ) %a -> %a\n"
      (List.pp Usize.pp) ks
      (Option.pp Usize.pp) sum
  end in
  let test_lists = [
    [];
    [0];
    [0; 1];
    [0; 1; 2];
    [0; 1; 66];
    [0; 1; 66; 91];
  ] in
  List.iter test_lists ~f:(fun ks ->
    test ks
  );
  printf "@]";

  [%expect{|
    kreduce ~f:( + ) [] -> None
    kreduce ~f:( + ) [0] -> Some 0
    kreduce ~f:( + ) [0; 1] -> Some 1
    kreduce ~f:( + ) [0; 1; 2] -> Some 3
    kreduce ~f:( + ) [0; 1; 66] -> Some 67
    kreduce ~f:( + ) [0; 1; 66; 91] -> Some 158
    |}]

let%expect_test "reduce" =
  let open Format in
  printf "@[<h>";
  let test ks = begin
    let map = of_klist ks in
    let sum = reduce ~f:( + ) map in
    printf "reduce ~f:( + ) %a -> %a\n"
      (List.pp Usize.pp) ks
      (Option.pp Usize.pp) sum
  end in
  let test_lists = [
    [];
    [0];
    [0; 1];
    [0; 1; 2];
    [0; 1; 66];
    [0; 1; 66; 91];
  ] in
  List.iter test_lists ~f:(fun ks ->
    test ks
  );
  printf "@]";

  [%expect{|
    reduce ~f:( + ) [] -> None
    reduce ~f:( + ) [0] -> Some 0
    reduce ~f:( + ) [0; 1] -> Some 100
    reduce ~f:( + ) [0; 1; 2] -> Some 300
    reduce ~f:( + ) [0; 1; 66] -> Some 6700
    reduce ~f:( + ) [0; 1; 66; 91] -> Some 15800
    |}]

let%expect_test "stress" =
  let open Format in
  printf "@[";
  (* test is n^2 time complexity, so keep n small. *)
  let rec test n i e map = begin
    match i < n with
    | false -> map
    | true -> begin
        let map' = remove_hlt i
            (test n (succ i) e (insert_hlt ~k:i ~v:(i * 100) map)) in
        assert (equal veq map map');
        assert (equal veq map (union ~f:merge map map'));
        assert (equal veq map (inter ~f:merge map map'));
        assert (equal veq e (diff map map'));
        validate map';
        map'
      end
  end in
  let e = empty (module UsizeTestCmper) in
  let _ = test 100 0 e e in
  printf "@]";

  [%expect{|
    |}]
