(* Hash array mapped trie (HAMT) implementation of sets.  See the following for
   background information:

   - [Ideal Hash
     Trees](http://infoscience.epfl.ch/record/64398/files/idealhashtrees.pdf)
   - [Optimizing Hash-Array Mapped Tries for Fast and Lean Immutable JVM
     Collections](http://michael.steindorfer.name/publications/oopsla15.pdf)

   This implementation uses a canonical representation that results in stable
   (but arbitrary) member ordering.  The canonical representation differs
   slightly from that described in the referenced papers with regard to
   collisions.  Following is an informal description of canonical
   representation.

   - No subtree is empty, with the single exception of a root node representing
     an empty set.
   - No subtree contains a single member, with the single exception of a root
     node representing a singleton.
   - Collisions are always stored as children (specially interpreted nodes) of
     maximal-depth leaf nodes; their depth uniquely defines them as collisions,
     as does their present_{mem,child} bitmaps.
     Collisions could in principle be stored closer to the root, but assuming
     a well defined hash function, doing so would increase common-case
     complexity for the sake of an edge condition that is already degenerate
     performance-wise.
   - Collisions are stored in sorted order within each collision node.  This has
     the side benefit of making membership testing degrade from O(1) to O(lg n)
     in the presence of collisions (rather than O(n)), but the primary benefit
     is that it improves coupled iteration performance. *)

open Rudiments

(* Number of bits per HAMT level.  The data type used for node's
   present_{mem,child} fields must have at least elms_per_level bits. *)
let bits_per_level = 6
let elms_per_level = bit_sl ~shift:bits_per_level 1

(* Number of hash bits available per hash value. *)
let bits_per_hash = 128

(* Maximum HAMT height, ignoring collision nodes. *)
let max_height = bits_per_hash / bits_per_level

type 'a node = {
  (* The present_{mem,child} bitmaps encode which node elements are non-empty.
     Each node logically has elms_per_level elements, but only the non-empty
     ones are stored in elms_{mem,child}.  The two bitmaps are non-intersecting
     because each node element can only be a child *or* a member.

     The tree is limited to max_height levels, but leaves may need to store
     collisions.  Collisions are stored as nodes at depth max_height, but
     they must be interpreted specially.  We set all bits in both bitmaps for
     collision nodes so that code which does not track recursion depth can still
     recognize collision nodes. *)
  present_mem: u64;
  present_child: u64;
  (* Independent variable-length compressed element arrays, where the summed
     length of the arrays is in [0..elms_per_level].  For example, if the least
     significant bit of the present_mem bitmap is 1, then the corresponding
     element of present_mem is at index 0, regardless of
     {present,elms}_child. *)
  elms_mem: 'a array;
  elms_child: 'a node array;
}

(* Used only ephemerally during remove and reduce. *)
type 'a elm =
  | Child of 'a node
  | Mem of 'a

type ('a, 'cmp) t = {
  cmper: ('a, 'cmp) Cmper.t;
  length: usize;
  root: 'a node;
}

type ('a, 'cmp) cmper =
  (module Cmper.S_mono with type t = 'a and type cmper_witness = 'cmp)

let cmper_m (type a cmp) t : (a, cmp) cmper =
  (module struct
    type t = a
    type cmper_witness = cmp
    let cmper = t.cmper
  end)

(* Extract cmper from first-class module compatible with Cmper.S_mono . *)
let m_cmper (type a cmp) ((module M) : (a, cmp) cmper) =
  M.cmper

let cmper t =
  t.cmper

let shift_of_level level =
  bits_per_hash - (level + 1) * bits_per_level

let present_index_at_level hash level =
  let shift = shift_of_level level in
  let mask = U128.of_usize (elms_per_level - 1) in
  U128.(to_usize (bit_and mask (bit_usr ~shift hash)))

let present_bit_of_index index =
  U64.bit_sl ~shift:index U64.one

let elm_index_of_bit present present_bit =
  let trail_mask = U64.(present_bit - one) in
  U64.(bit_pop (bit_and trail_mask present))

let empty m =
  {cmper=m_cmper m; length=0;
    root={present_mem=U64.zero; present_child=U64.zero;
      elms_mem=[||]; elms_child=[||]}}

let singleton m a =
  let cmper = m_cmper m in
  let a_hash = Hash.State.seed |> cmper.hash_fold a |> Hash.t_of_state in
  let present_index = present_index_at_level a_hash 0 in
  let present_mem = present_bit_of_index present_index in
  {cmper; length=1;
    root={present_mem; present_child=U64.zero; elms_mem=[|a|]; elms_child=[||]}}

let length t =
  t.length

let is_empty t =
  t.length = 0

let mem a t =
  let open Cmper in
  let rec fn a a_hash cmper node level = begin
    match level < max_height with
    | true -> begin
        let present_index = present_index_at_level a_hash level in
        let present_bit = present_bit_of_index present_index in
        match U64.((bit_and present_bit node.present_mem) = zero) with
        | true -> begin
            match U64.((bit_and present_bit node.present_child) = zero) with
            | true -> false
            | false -> begin
                let elms_child_index =
                  elm_index_of_bit node.present_child present_bit in
                let child = Array.get elms_child_index node.elms_child in
                fn a a_hash cmper child (succ level)
              end
          end
        | false -> begin
            let elms_mem_index =
              elm_index_of_bit node.present_mem present_bit in
            let m = Array.get elms_mem_index node.elms_mem in
            Cmp.is_eq (cmper.cmp a m)
          end
      end
    | false ->
      (* Collision node. *)
      Option.is_some (Array.search a ~cmp:cmper.cmp node.elms_mem)
  end in
  let a_hash = Hash.State.seed |> t.cmper.hash_fold a |> Hash.t_of_state in
  fn a a_hash t.cmper t.root 0

let insert a t =
  let open Cmper in
  (* Create a collision node, with members canonically ordered (sorted). *)
  let node_of_collision a b cmper = begin
    let m0, m1 = match cmper.cmp a b with
      | Lt -> a, b
      | Eq -> not_reached ()
      | Gt -> b, a
    in
    {present_mem=U64.max_value; present_child=U64.max_value;
      elms_mem=[|m0; m1|]; elms_child=[||]}
  end in
  (* Create a disambiguating subtree for a and b. *)
  let rec disambiguate a a_hash b b_hash cmper level = begin
    assert (level < max_height);

    (* Create a 2-element node, where elm0's index is less than elm1's. *)
    let node_of_elms elm0 elm0_present_index elm1 elm1_present_index = begin
      assert (elm0_present_index < elm1_present_index);
      let elm0_present_bit = present_bit_of_index elm0_present_index in
      let elm1_present_bit = present_bit_of_index elm1_present_index in
      let present_mem = U64.bit_or elm0_present_bit elm1_present_bit in
      let elms_mem = [|elm0; elm1|] in
      {present_mem; present_child=U64.zero; elms_mem; elms_child=[||]}
    end in

    let a_present_index = present_index_at_level a_hash level in
    let b_present_index = present_index_at_level b_hash level in
    match Usize.cmp a_present_index b_present_index with
    | Lt -> node_of_elms a a_present_index b b_present_index
    | Eq -> begin
        let present_child = present_bit_of_index a_present_index in
        let child = match level + 1 < max_height with
          | false -> node_of_collision a b cmper
          | true -> disambiguate a a_hash b b_hash cmper (succ level)
        in
        let elms_child = [|child|] in
        {present_mem=U64.zero; present_child; elms_mem=[||]; elms_child}
      end
    | Gt -> node_of_elms b b_present_index a a_present_index
  end in
  let rec fn a a_hash cmper node level = begin
    let present_index = present_index_at_level a_hash level in
    let present_bit = present_bit_of_index present_index in
    match U64.((bit_and present_bit node.present_mem) = zero) with
    | true -> begin
        match U64.((bit_and present_bit node.present_child) = zero) with
        | true -> begin
            (* Not present in node; insert. *)
            let elms_mem_index =
              elm_index_of_bit node.present_mem present_bit in
            let present_mem' = U64.bit_or present_bit node.present_mem in
            let elms_mem' = Array.insert elms_mem_index a node.elms_mem in
            Some {node with present_mem=present_mem'; elms_mem=elms_mem'}
          end
        | false -> begin
            let elms_child_index =
              elm_index_of_bit node.present_child present_bit in
            let child = Array.get elms_child_index node.elms_child in
            match level + 1 < max_height with
            | true -> begin
                match fn a a_hash cmper child (succ level) with
                | None -> None
                | Some child' -> begin
                    let elms_child' =
                      Array.set elms_child_index child' node.elms_child in
                    Some {node with elms_child=elms_child'}
                  end
              end
            | false -> begin
                (* Collision node. *)
                let collision_insert i a child elms_child_index node = begin
                  let child_elms_mem' = Array.insert i a child.elms_mem in
                  let child' = {child with elms_mem=child_elms_mem'} in
                  let elms_child' =
                    Array.set elms_child_index child' node.elms_child in
                  Some {node with elms_child=elms_child'}
                end in
                match Array.nsearch a ~cmp:cmper.cmp child.elms_mem with
                | Some (Lt, _) ->
                  collision_insert 0 a child elms_child_index node
                | Some (Eq, _) -> None (* Already in collision node. *)
                | Some (Gt, i) ->
                  collision_insert (succ i) a child elms_child_index node
                | None -> not_reached ()
              end
          end
      end
    | false -> begin
        let elms_mem_index = elm_index_of_bit node.present_mem present_bit in
        let m = Array.get elms_mem_index node.elms_mem in
        match Cmp.is_eq (cmper.cmp a m) with
        | false -> begin
            (* Unequal member already present. *)
            let elms_child_index =
              elm_index_of_bit node.present_child present_bit in
            let child = match level + 1 < max_height with
              | false -> node_of_collision a m cmper
              | true -> begin
                  (* Create a subtree which tries to disambiguate the two
                     members.  In the worst case, this creates a chain of
                     nodes linking to a leaf of maximal depth containing a
                     collision. *)
                  let m_hash = Hash.State.seed |> cmper.hash_fold m
                               |> Hash.t_of_state in
                  disambiguate a a_hash m m_hash cmper (succ level)
                end
            in
            let present_mem' = U64.bit_xor present_bit node.present_mem in
            let present_child' =
              U64.bit_xor present_bit node.present_child in
            let elms_mem' = Array.remove elms_mem_index node.elms_mem in
            let elms_child' =
              Array.insert elms_child_index child node.elms_child in
            Some {present_mem=present_mem'; present_child=present_child';
              elms_mem=elms_mem'; elms_child=elms_child'}
          end
        | true -> None
      end
  end in
  let a_hash = Hash.State.seed |> t.cmper.hash_fold a |> Hash.t_of_state in
  match fn a a_hash t.cmper t.root 0 with
  | None -> t
  | Some root' -> {t with length=succ t.length; root=root'}

let of_list m alist =
  match alist with
  | [] -> empty m
  | a :: alist' -> begin
      let rec fn alist set = begin
        match alist with
        | [] -> set
        | a :: alist' -> fn alist' (insert a set)
      end in
      fn alist' (singleton m a)
    end

let remove a t =
  let open Cmper in
  (* Collapse a child to a single member. *)
  let collapse_child m present_bit elms_child_index node = begin
    match node.elms_mem, node.elms_child with
    | [||], [|_|] -> Some (Mem m)
    | _ -> begin
        let elms_mem_index = elm_index_of_bit node.present_mem present_bit in
        let present_mem' = U64.bit_xor present_bit node.present_mem in
        let present_child' = U64.bit_xor present_bit node.present_child in
        let elms_mem' = Array.insert elms_mem_index m node.elms_mem in
        let elms_child' = Array.remove elms_child_index node.elms_child in
        let node' = {present_mem=present_mem'; present_child=present_child';
          elms_mem=elms_mem'; elms_child=elms_child'} in
        Some (Child node')
      end
  end in
  (* Remove member from node; remove node if no children and one member
     remaining. *)
  let remove_mem present_bit elms_mem_index node = begin
    match node.elms_mem, node.elms_child with
    | [|_; _|], [||] -> begin
        let m_index = match elms_mem_index with
          | 0 -> 1
          | 1 -> 0
          | _ -> not_reached ()
        in
        let m = Array.get m_index node.elms_mem in
        Some (Mem m)
      end
    | _ -> begin
        let present_mem' = U64.bit_xor present_bit node.present_mem in
        let elms_mem' = Array.remove elms_mem_index node.elms_mem in
        let node' = {node with present_mem=present_mem'; elms_mem=elms_mem'} in
        Some (Child node')
      end
  end in
  let rec fn a a_hash cmper node level = begin
    let present_index = present_index_at_level a_hash level in
    let present_bit = present_bit_of_index present_index in
    match U64.((bit_and present_bit node.present_mem) = zero) with
    | true -> begin
        match U64.((bit_and present_bit node.present_child) = zero) with
        | true -> false, None (* Not a member; no-op. *)
        | false -> begin
            let elms_child_index =
              elm_index_of_bit node.present_child present_bit in
            let child = Array.get elms_child_index node.elms_child in
            match level + 1 < max_height with
            | true -> begin
                match fn a a_hash cmper child (succ level) with
                | false, _ -> false, None
                | true, Some (Child child') -> begin
                    let elms_child' =
                      Array.set elms_child_index child' node.elms_child in
                    true, Some (Child {node with elms_child=elms_child'})
                  end
                | true, Some (Mem m) ->
                  true, collapse_child m present_bit elms_child_index node
                | true, None -> not_reached ()
              end
            | false -> begin
                (* Collision node. *)
                match Array.search a ~cmp:cmper.cmp child.elms_mem with
                | None -> false, None (* Not a member; no-op. *)
                | Some found_index -> begin
                    match child.elms_mem with
                    | [|_; _|] -> begin
                        (* No remaining collisions (single remaining member). *)
                        let m_index = match found_index with
                          | 0 -> 1
                          | 1 -> 0
                          | _ -> not_reached ()
                        in
                        let m = Array.get m_index child.elms_mem in
                        true, collapse_child m present_bit elms_child_index node
                      end
                    | _ -> begin
                        (* Remaining collisions. *)
                        let child_elms_mem' =
                          Array.remove found_index child.elms_mem in
                        let child' = {child with elms_mem=child_elms_mem'} in
                        let elms_child' =
                          Array.set elms_child_index child' node.elms_child in
                        true, Some (Child {node with elms_child=elms_child'})
                      end
                  end
              end
          end
      end
    | false -> begin
        let elms_mem_index = elm_index_of_bit node.present_mem present_bit in
        let m = Array.get elms_mem_index node.elms_mem in
        match Cmp.is_eq (cmper.cmp a m) with
        | false -> false, None (* Not a member; no-op. *)
        | true -> true, (remove_mem present_bit elms_mem_index node)
      end
  end in
  let a_hash = Hash.State.seed |> t.cmper.hash_fold a |> Hash.t_of_state in
  match fn a a_hash t.cmper t.root 0 with
  | false, _ -> t
  | true, Some (Child node) -> {t with length=pred t.length; root=node}
  | true, Some (Mem m) -> begin
      (* Singleton. *)
      let m_hash = Hash.State.seed |> t.cmper.hash_fold m |> Hash.t_of_state in
      let present_index = present_index_at_level m_hash 0 in
      let present_mem = present_bit_of_index present_index in
      let root' = {present_mem; present_child=U64.zero;
        elms_mem=[|m|]; elms_child=[||]} in
      {t with length=1; root=root'}
    end
  | true, None -> begin
      (* Empty. *)
      let root' = {present_mem=U64.zero; present_child=U64.zero;
        elms_mem=[||]; elms_child=[||]} in
      {t with length=0; root=root'}
    end

let fold_until ~init ~f t =
  let rec fn accum f node = begin
    let accum', until = Array.fold_until node.elms_mem ~init:(accum, false)
      ~f:(fun (accum, _) m ->
        let accum', until = f accum m in
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

let hash_fold t state =
  (* Fold ordering is not stable for unequal sets, but order permutation does
     increase risk of accidental collision for sets, because each member occurs
     precisely once regardless of order. *)
  let n, state' = fold t ~init:(0, state) ~f:(fun (i, state) mem ->
    (succ i),
    (
      state
      |> Usize.hash_fold i
      |> t.cmper.hash_fold mem
    )
  ) in
  state' |> Usize.hash_fold n

(* Seq.  Note that internal iteration via fold* traverses members, then
   children, but that ordering is not stable, and cannot be used here.  External
   iteration must do a stable in-order traversal, so that the orderings of
   unequal sets are monotonic relative to their union. *)
module Seq_poly2_fold2 = struct
  type ('a, 'cmp) container = ('a, 'cmp) t
  type 'a elm = 'a
  type 'a node_pos = {
    node: 'a node;
    present_bit: u64;
  }
  type ('a, 'cmp) t = {
    ind: usize;
    len: usize;
    path: 'a node_pos list;
  }

  let is_collision_node node =
    U64.(node.present_mem = max_value) && U64.(node.present_child = max_value)

  let rec leftmost_path node path =
    match node.elms_mem, node.elms_child with
    | [||], [||] -> path
    | [||], [|_|] -> begin
        (* Tail optimization. *)
        let child = Array.get 0 node.elms_child in
        leftmost_path child path
      end
    | _ -> begin
        match is_collision_node node with
        | false -> begin
            let mem_present_index = U64.bit_ctz node.present_mem in
            let child_present_index = U64.bit_ctz node.present_child in
            match mem_present_index <> 64 &&
                  (mem_present_index < child_present_index) with
            | true -> begin
                let mem_present_bit = present_bit_of_index mem_present_index in
                {node; present_bit=mem_present_bit} :: path
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
            {node; present_bit=U64.one} :: path
          end
      end

  let a_of_path = function
    | [] -> not_reached ()
    | node_pos :: _ ->
      let elm_ind =
        elm_index_of_bit node_pos.node.present_mem node_pos.present_bit in
      Array.get elm_ind node_pos.node.elms_mem

  let rec next_path = function
    | [] -> []
    | node_pos :: path_tl -> begin
        match is_collision_node node_pos.node with
        | false -> begin
            let mask =
              U64.(bit_not ((bit_sl ~shift:1 node_pos.present_bit) - one)) in
            let mem_present_index' =
              U64.(bit_ctz (bit_and mask node_pos.node.present_mem)) in
            let child_present_index' =
              U64.(bit_ctz (bit_and mask node_pos.node.present_child)) in
            match (mem_present_index' <> 64) &&
                  (mem_present_index' < child_present_index') with
            | true -> begin
                let mem_present_bit' =
                  present_bit_of_index mem_present_index' in
                {node_pos with present_bit=mem_present_bit'} :: path_tl
              end
            | false -> begin
                match child_present_index' = 64 with
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
            let elm_index' = succ (U64.bit_ctz node_pos.present_bit) in
            match elm_index' < Array.length node_pos.node.elms_mem with
            | false -> next_path path_tl
            | true -> begin
                let mem_present_bit' = present_bit_of_index elm_index' in
                let node_pos' = {node_pos with present_bit=mem_present_bit'} in
                node_pos' :: path_tl
              end
          end
      end

  let init set =
    {ind=0; len=set.length; path=leftmost_path set.root []}

  let index t =
    t.ind

  let length t =
    t.len

  let next t =
    assert (index t < length t);
    let a = a_of_path t.path in
    let t' = {t with ind=succ t.ind; path=next_path t.path} in
    a, t'

  let next_opt t =
    match (index t) < (length t) with
    | false -> None
    | true -> Some (next t)

  let cmper = cmper

  let cmp cmper a0 a1 =
    let open Cmper in
    let open Cmp in
    let a0_hash = Hash.State.seed |> cmper.hash_fold a0 |> Hash.t_of_state in
    let a1_hash = Hash.State.seed |> cmper.hash_fold a1 |> Hash.t_of_state in
    match U128.cmp a0_hash a1_hash with
    | Lt -> Lt
    | Eq -> cmper.cmp a0 a1
    | Gt -> Gt
end
include Seq.Make_poly2_fold2(Seq_poly2_fold2)
module Seq = Seq_poly2_fold2

let equal t0 t1 =
  let open Cmper in
  let rec fn cmper node0 node1 = begin
    U64.(node0.present_mem = node1.present_mem) &&
    U64.(node0.present_child = node1.present_child) &&
    Cmp.is_eq (Array.cmp cmper.cmp node0.elms_mem node1.elms_mem) &&
    Cmp.is_eq (Array.cmp (fun node0 node1 ->
      match fn cmper node0 node1 with
      | false -> Lt
      | true -> Eq
    ) node0.elms_child node1.elms_child)
  end in
  (* Check lengths first, since it's cheap and easy to do so. *)
  match (length t0) = (length t1) with
  | false -> false
  | true -> fn t0.cmper t0.root t1.root

let union t0 t1 =
  (* Initialize the union with the larger of the two input sets, in order to
     minimize number of insertions. *)
  let small, big = match Cmp.is_le (cmp (length t0) (length t1)) with
    | true -> t0, t1
    | false -> t1, t0
  in
  fold2 ~init:big ~f:(fun accum a_small_opt a_big_opt ->
    match a_small_opt, a_big_opt with
    | Some a, None -> insert a accum
    | None, Some _
    | Some _, Some _ -> accum
    | None, None -> not_reached ()
  ) small big

let inter t0 t1 =
  fold2 ~init:(empty (cmper_m t0)) ~f:(fun accum a0_opt a1_opt ->
    match a0_opt, a1_opt with
    | Some a, Some _ -> insert a accum
    | Some _, None
    | None, Some _ -> accum
    | None, None -> not_reached ()
  ) t0 t1

let diff t0 t1 =
  fold2 ~init:t0 ~f:(fun accum a0_opt a1_opt ->
    match a0_opt, a1_opt with
    | Some a, Some _ -> remove a accum
    | Some _, None
    | None, Some _ -> accum
    | None, None -> not_reached ()
  ) t0 t1

let count ~f t =
  fold ~init:0 ~f:(fun accum a ->
    match f a with
    | false -> accum
    | true -> succ accum
  ) t

let for_any ~f t =
  fold_until ~init:false ~f:(fun _ a ->
    let until = f a in
    until, until
  ) t

let for_all ~f t =
  fold_until ~init:true ~f:(fun _ a ->
    let unless = f a in
    let until = not unless in
    unless, until
  ) t

let find ~f t =
  fold_until ~init:None ~f:(fun _ a ->
    match f a with
    | true -> (Some a), true
    | false -> None, false
  ) t

let find_map ~f t =
  fold_until ~init:None ~f:(fun _ a ->
    match f a with
    | Some b -> (Some b), true
    | None -> None, false
  ) t

let filter ~f t =
  fold ~init:(empty (cmper_m t)) ~f:(fun accum a ->
    match f a with
    | false -> accum
    | true -> insert a accum
  ) t

let reduce ~f t =
  let reduce_mems ~reduce2 mems = begin
    Array.reduce_hlt ~f:(fun m0 m1 -> reduce2 m0 m1) mems
  end in
  let rec reduce_children ~reduce2 children = begin
    match Array.reduce_hlt ~f:(fun elm0 elm1 ->
      Mem (
        match elm0, elm1 with
        | Child child0, Child child1 -> reduce2 (reduce_node ~reduce2 child0)
          (reduce_node ~reduce2 child1)
        | Child child, Mem m
        | Mem m, Child child -> reduce2 (reduce_node ~reduce2 child) m
        | Mem m0, Mem m1 -> reduce2 m0 m1
      )
    ) (Array.map ~f:(fun child -> Child child) children) with
    | Mem m -> m
    | Child child -> reduce_node ~reduce2 child
  end
  and reduce_node ~reduce2 node = begin
    match node.elms_mem, node.elms_child with
    | [||], [||] -> not_reached ()
    | _, [||] -> reduce_mems ~reduce2 node.elms_mem
    | [||], _ -> reduce_children ~reduce2 node.elms_child
    | _ -> reduce2 (reduce_mems ~reduce2 node.elms_mem)
      (reduce_children ~reduce2 node.elms_child)
  end in
  match t.length with
  | 0 -> None
  | _ -> Some (reduce_node ~reduce2:f t.root)

let reduce_hlt ~f t =
  match reduce ~f t with
  | None -> halt "Empty set"
  | Some set -> set

let to_list t =
  fold ~init:[] ~f:(fun accum a -> a :: accum) t

module Set_to_array = struct
  include Seq
  include Array.Seq.Make_poly2(Seq)
end

let to_array t =
  Set_to_array.(to_array (init t))

let pp ppf t =
  let open Format in
  let rec pp_mems ppf mems = begin
    fprintf ppf "@[<v>elms_mem=[|";
    if Array.length mems > 0 then fprintf ppf "@;<0 2>@[<v>";
    Array.iteri mems ~f:(fun i m ->
      if i > 0 then fprintf ppf ";@,";
      fprintf ppf "%a" t.cmper.pp m
    );
    if Array.length mems > 0 then fprintf ppf "@]@,";
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
    fprintf ppf "@[<v>present_mem=  %a;@,present_child=%a;@,%a;@,%a@]"
      U64.pp_x node.present_mem
      U64.pp_x node.present_child
      pp_mems node.elms_mem
      pp_children node.elms_child
  end
  and pp_root ppf root = begin
    fprintf ppf "@[<v>root={@;<0 2>%a@,}@]" pp_node root
  end
  in
  fprintf ppf "@[<v>Set {@;<0 2>@[<v>length=%a;@,%a@]@,}@]"
    Usize.pp t.length
    pp_root t.root

(******************************************************************************)
(* Begin tests. *)

(* Test comparator module for usize that uses unseeded hashing, with several
   specially handled values for the purpose of collision testing.  This allows
   deterministic hashing across test runs. *)
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

let%expect_test "hash_fold" =
  let open Format in
  printf "@[";
  let rec fn = function
    | [] -> ()
    | l :: lists' -> begin
        let set = of_list (module UsizeTestCmper) l in
        printf "hash_fold (of_list (module UsizeTestCmper) %a) -> %a@\n"
          (List.pp Usize.pp) l
          Hash.pp (Hash.t_of_state (hash_fold set Hash.State.empty));
        fn lists'
      end
  in
  (* NB: [0; 1] and [0; 2] collide.  This is because we're using UsizeTestCmper
     to get stable test output; the hashing results from all but the last member
     hashed are discarded. *)
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
    hash_fold (of_list (module UsizeTestCmper) []) -> 0xb465_a9ec_cd79_1cb6_4bbd_1bf2_7da9_18d6u128
    hash_fold (of_list (module UsizeTestCmper) [0]) -> 0xcffe_8f1d_4ece_31b1_0231_7d19_4ec8_ede7u128
    hash_fold (of_list (module UsizeTestCmper) [0; 1]) -> 0x55b0_b3fa_271e_6e95_175a_851f_44f1_920bu128
    hash_fold (of_list (module UsizeTestCmper) [0; 2]) -> 0x55b0_b3fa_271e_6e95_175a_851f_44f1_920bu128
    hash_fold (of_list (module UsizeTestCmper) [2; 3]) -> 0xd8c3_705a_8b39_1dcb_e50c_11fa_a29a_11bdu128
    |}]

let%expect_test "hash_fold empty" =
  let hash_empty state = begin
    state
    |> hash_fold (empty (module UsizeTestCmper))
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
  assert (length e = 0);
  printf "%a@\n" pp e;

  let s = singleton (cmper_m e) 0 in
  assert (length s = 1);
  printf "%a@\n" pp s;
  printf "@]";

  [%expect{|
    Set {
      length=0;
      root={
        present_mem=  0x0000_0000_0000_0000u64;
        present_child=0x0000_0000_0000_0000u64;
        elms_mem=[||];
        elms_child=[||]
      }
    }
    Set {
      length=1;
      root={
        present_mem=  0x0000_2000_0000_0000u64;
        present_child=0x0000_0000_0000_0000u64;
        elms_mem=[|
          0
        |];
        elms_child=[||]
      }
    }
    |}]

let%expect_test "mem,insert" =
  let open Format in
  printf "@[";
  let rec test ms set = begin
    match ms with
    | [] -> printf "%a@\n" pp set
    | m :: ms' -> begin
        assert (not (mem m set));
        let set' = insert m set in
        assert (mem m set');
        test ms' set'
      end
  end in
  let ms = [1; 3; 2; 44; 45; 56; 60; 66; 75; 81; 91] in
  test ms (empty (module UsizeTestCmper));
  printf "@]";

  [%expect{|
    Set {
      length=11;
      root={
        present_mem=  0x0040_2000_8000_000eu64;
        present_child=0x0000_0200_0000_0020u64;
        elms_mem=[|
          56;
          3;
          75;
          45;
          44;
          60
        |];
        elms_child=[|
          present_mem=  0x4001_0000_0010_0000u64;
          present_child=0x0000_0000_0000_0000u64;
          elms_mem=[|
            66;
            91;
            1
          |];
          elms_child=[||];
          present_mem=  0x0000_0200_0000_8000u64;
          present_child=0x0000_0000_0000_0000u64;
          elms_mem=[|
            81;
            2
          |];
          elms_child=[||]
        |]
      }
    }
    |}]

let%expect_test "of_list duplicate" =
  let open Format in
  printf "@[";
  printf "%a@\n" pp (of_list (module UsizeTestCmper) [0; 0]);
  printf "@]";

  [%expect{|
    Set {
      length=1;
      root={
        present_mem=  0x0000_2000_0000_0000u64;
        present_child=0x0000_0000_0000_0000u64;
        elms_mem=[|
          0
        |];
        elms_child=[||]
      }
    }
    |}]

let%expect_test "of_list collision" =
  let open Format in
  let geometry = get_geometry () in
  safe_set_geometry ~max_indent:100 ~margin:200;
  printf "@[";
  (* 1) Collision in root, pushed to leaf.
     2) Collision with existing collision list. *)
  printf "%a@\n" pp (of_list (module UsizeTestCmper) [42; 420; 4200]);
  (* New collision in existing leaf. *)
  printf "%a@\n" pp (of_list (module UsizeTestCmper) [420; 421; 42]);
  printf "@]";
  safe_set_geometry ~max_indent:geometry.max_indent ~margin:geometry.margin;

  [%expect{|
    Set {
      length=3;
      root={
        present_mem=  0x0000_0000_0000_0000u64;
        present_child=0x0000_0000_0000_0001u64;
        elms_mem=[||];
        elms_child=[|
          present_mem=  0x0000_0000_0000_0000u64;
          present_child=0x0000_0000_0000_0001u64;
          elms_mem=[||];
          elms_child=[|
            present_mem=  0x0000_0000_0000_0000u64;
            present_child=0x0000_0000_0000_0001u64;
            elms_mem=[||];
            elms_child=[|
              present_mem=  0x0000_0000_0000_0000u64;
              present_child=0x0000_0000_0000_0001u64;
              elms_mem=[||];
              elms_child=[|
                present_mem=  0x0000_0000_0000_0000u64;
                present_child=0x0000_0000_0000_0001u64;
                elms_mem=[||];
                elms_child=[|
                  present_mem=  0x0000_0000_0000_0000u64;
                  present_child=0x0000_0000_0000_0001u64;
                  elms_mem=[||];
                  elms_child=[|
                    present_mem=  0x0000_0000_0000_0000u64;
                    present_child=0x0000_0000_0000_0001u64;
                    elms_mem=[||];
                    elms_child=[|
                      present_mem=  0x0000_0000_0000_0000u64;
                      present_child=0x0000_0000_0000_0001u64;
                      elms_mem=[||];
                      elms_child=[|
                        present_mem=  0x0000_0000_0000_0000u64;
                        present_child=0x0000_0000_0000_0001u64;
                        elms_mem=[||];
                        elms_child=[|
                          present_mem=  0x0000_0000_0000_0000u64;
                          present_child=0x0000_0000_0000_0001u64;
                          elms_mem=[||];
                          elms_child=[|
                            present_mem=  0x0000_0000_0000_0000u64;
                            present_child=0x0000_0000_0000_0001u64;
                            elms_mem=[||];
                            elms_child=[|
                              present_mem=  0x0000_0000_0000_0000u64;
                              present_child=0x0000_0000_0000_0001u64;
                              elms_mem=[||];
                              elms_child=[|
                                present_mem=  0x0000_0000_0000_0000u64;
                                present_child=0x0000_0000_0000_0001u64;
                                elms_mem=[||];
                                elms_child=[|
                                  present_mem=  0x0000_0000_0000_0000u64;
                                  present_child=0x0000_0000_0000_0001u64;
                                  elms_mem=[||];
                                  elms_child=[|
                                    present_mem=  0x0000_0000_0000_0000u64;
                                    present_child=0x0000_0000_0000_0001u64;
                                    elms_mem=[||];
                                    elms_child=[|
                                      present_mem=  0x0000_0000_0000_0000u64;
                                      present_child=0x0000_0000_0000_0001u64;
                                      elms_mem=[||];
                                      elms_child=[|
                                        present_mem=  0x0000_0000_0000_0000u64;
                                        present_child=0x0000_0000_0000_0001u64;
                                        elms_mem=[||];
                                        elms_child=[|
                                          present_mem=  0x0000_0000_0000_0000u64;
                                          present_child=0x0000_0000_0000_0001u64;
                                          elms_mem=[||];
                                          elms_child=[|
                                            present_mem=  0x0000_0000_0000_0000u64;
                                            present_child=0x0000_0000_0000_0001u64;
                                            elms_mem=[||];
                                            elms_child=[|
                                              present_mem=  0x0000_0000_0000_0000u64;
                                              present_child=0x0000_0000_0000_0001u64;
                                              elms_mem=[||];
                                              elms_child=[|
                                                present_mem=  0x0000_0000_0000_0000u64;
                                                present_child=0x0000_0000_0000_0001u64;
                                                elms_mem=[||];
                                                elms_child=[|
                                                  present_mem=  0xffff_ffff_ffff_ffffu64;
                                                  present_child=0xffff_ffff_ffff_ffffu64;
                                                  elms_mem=[|
                                                    42;
                                                    420;
                                                    4200
                                                  |];
                                                  elms_child=[||]
                                                |]
                                              |]
                                            |]
                                          |]
                                        |]
                                      |]
                                    |]
                                  |]
                                |]
                              |]
                            |]
                          |]
                        |]
                      |]
                    |]
                  |]
                |]
              |]
            |]
          |]
        |]
      }
    }
    Set {
      length=3;
      root={
        present_mem=  0x0000_0000_0000_0000u64;
        present_child=0x0000_0000_0000_0001u64;
        elms_mem=[||];
        elms_child=[|
          present_mem=  0x0000_0000_0000_0000u64;
          present_child=0x0000_0000_0000_0001u64;
          elms_mem=[||];
          elms_child=[|
            present_mem=  0x0000_0000_0000_0000u64;
            present_child=0x0000_0000_0000_0001u64;
            elms_mem=[||];
            elms_child=[|
              present_mem=  0x0000_0000_0000_0000u64;
              present_child=0x0000_0000_0000_0001u64;
              elms_mem=[||];
              elms_child=[|
                present_mem=  0x0000_0000_0000_0000u64;
                present_child=0x0000_0000_0000_0001u64;
                elms_mem=[||];
                elms_child=[|
                  present_mem=  0x0000_0000_0000_0000u64;
                  present_child=0x0000_0000_0000_0001u64;
                  elms_mem=[||];
                  elms_child=[|
                    present_mem=  0x0000_0000_0000_0000u64;
                    present_child=0x0000_0000_0000_0001u64;
                    elms_mem=[||];
                    elms_child=[|
                      present_mem=  0x0000_0000_0000_0000u64;
                      present_child=0x0000_0000_0000_0001u64;
                      elms_mem=[||];
                      elms_child=[|
                        present_mem=  0x0000_0000_0000_0000u64;
                        present_child=0x0000_0000_0000_0001u64;
                        elms_mem=[||];
                        elms_child=[|
                          present_mem=  0x0000_0000_0000_0000u64;
                          present_child=0x0000_0000_0000_0001u64;
                          elms_mem=[||];
                          elms_child=[|
                            present_mem=  0x0000_0000_0000_0000u64;
                            present_child=0x0000_0000_0000_0001u64;
                            elms_mem=[||];
                            elms_child=[|
                              present_mem=  0x0000_0000_0000_0000u64;
                              present_child=0x0000_0000_0000_0001u64;
                              elms_mem=[||];
                              elms_child=[|
                                present_mem=  0x0000_0000_0000_0000u64;
                                present_child=0x0000_0000_0000_0001u64;
                                elms_mem=[||];
                                elms_child=[|
                                  present_mem=  0x0000_0000_0000_0000u64;
                                  present_child=0x0000_0000_0000_0001u64;
                                  elms_mem=[||];
                                  elms_child=[|
                                    present_mem=  0x0000_0000_0000_0000u64;
                                    present_child=0x0000_0000_0000_0001u64;
                                    elms_mem=[||];
                                    elms_child=[|
                                      present_mem=  0x0000_0000_0000_0000u64;
                                      present_child=0x0000_0000_0000_0001u64;
                                      elms_mem=[||];
                                      elms_child=[|
                                        present_mem=  0x0000_0000_0000_0000u64;
                                        present_child=0x0000_0000_0000_0001u64;
                                        elms_mem=[||];
                                        elms_child=[|
                                          present_mem=  0x0000_0000_0000_0000u64;
                                          present_child=0x0000_0000_0000_0001u64;
                                          elms_mem=[||];
                                          elms_child=[|
                                            present_mem=  0x0000_0000_0000_0000u64;
                                            present_child=0x0000_0000_0000_0001u64;
                                            elms_mem=[||];
                                            elms_child=[|
                                              present_mem=  0x0000_0000_0000_0000u64;
                                              present_child=0x0000_0000_0000_0001u64;
                                              elms_mem=[||];
                                              elms_child=[|
                                                present_mem=  0x0000_0000_0000_0002u64;
                                                present_child=0x0000_0000_0000_0001u64;
                                                elms_mem=[|
                                                  421
                                                |];
                                                elms_child=[|
                                                  present_mem=  0xffff_ffff_ffff_ffffu64;
                                                  present_child=0xffff_ffff_ffff_ffffu64;
                                                  elms_mem=[|
                                                    42;
                                                    420
                                                  |];
                                                  elms_child=[||]
                                                |]
                                              |]
                                            |]
                                          |]
                                        |]
                                      |]
                                    |]
                                  |]
                                |]
                              |]
                            |]
                          |]
                        |]
                      |]
                    |]
                  |]
                |]
              |]
            |]
          |]
        |]
      }
    }
    |}]

let%expect_test "of_list,remove" =
  let open Format in
  let geometry = get_geometry () in
  safe_set_geometry ~max_indent:100 ~margin:200;
  printf "@[";
  let test m set descr = begin
    printf "--- %s ---@\n" descr;
    printf "@[<v>remove %a@;<0 2>@[<v>%a ->@,%a@]@]@\n"
      Usize.pp m pp set pp (remove m set)
  end in
  let test_tuples = [
    ([0; 1], 2,            "Not member, elm empty.");
    ([1], 91,              "Not member, elm of different value.");
    ([42; 420], 4200,      "Not member, missing from collision list.");
    ([0], 0,               "Member, length 1 -> 0.");
    ([0; 1], 1,            "Member, length 2 -> 1.");
    ([0; 1; 2], 2,         "Member, length 3 -> 2.");
    ([0; 1; 66], 66,       "Member, subnode elms 2 -> 1.");
    ([0; 1; 66; 91], 91,   "Member, subnode elms 3 -> 2.");
    ([42; 420; 4200], 420, "Member, collision list 3 -> 2.");
    ([42; 420; 421], 420,  "Member, collision list 2 -> 1, leaf elms 2 -> 2.");
    ([42; 420], 420,       "Member, collision list 2 -> 1, leaf elms 2 -> 1.");
  ] in
  List.iter test_tuples ~f:(fun (ms, m, descr) ->
    let set = of_list (module UsizeTestCmper) ms in
    test m set descr
  );
  printf "@]";
  safe_set_geometry ~max_indent:geometry.max_indent ~margin:geometry.margin;

  [%expect{|
    --- Not member, elm empty. ---
    remove 2
      Set {
        length=2;
        root={
          present_mem=  0x0000_2000_0000_0020u64;
          present_child=0x0000_0000_0000_0000u64;
          elms_mem=[|
            1;
            0
          |];
          elms_child=[||]
        }
      } ->
      Set {
        length=2;
        root={
          present_mem=  0x0000_2000_0000_0020u64;
          present_child=0x0000_0000_0000_0000u64;
          elms_mem=[|
            1;
            0
          |];
          elms_child=[||]
        }
      }
    --- Not member, elm of different value. ---
    remove 91
      Set {
        length=1;
        root={
          present_mem=  0x0000_0000_0000_0020u64;
          present_child=0x0000_0000_0000_0000u64;
          elms_mem=[|
            1
          |];
          elms_child=[||]
        }
      } ->
      Set {
        length=1;
        root={
          present_mem=  0x0000_0000_0000_0020u64;
          present_child=0x0000_0000_0000_0000u64;
          elms_mem=[|
            1
          |];
          elms_child=[||]
        }
      }
    --- Not member, missing from collision list. ---
    remove 4200
      Set {
        length=2;
        root={
          present_mem=  0x0000_0000_0000_0000u64;
          present_child=0x0000_0000_0000_0001u64;
          elms_mem=[||];
          elms_child=[|
            present_mem=  0x0000_0000_0000_0000u64;
            present_child=0x0000_0000_0000_0001u64;
            elms_mem=[||];
            elms_child=[|
              present_mem=  0x0000_0000_0000_0000u64;
              present_child=0x0000_0000_0000_0001u64;
              elms_mem=[||];
              elms_child=[|
                present_mem=  0x0000_0000_0000_0000u64;
                present_child=0x0000_0000_0000_0001u64;
                elms_mem=[||];
                elms_child=[|
                  present_mem=  0x0000_0000_0000_0000u64;
                  present_child=0x0000_0000_0000_0001u64;
                  elms_mem=[||];
                  elms_child=[|
                    present_mem=  0x0000_0000_0000_0000u64;
                    present_child=0x0000_0000_0000_0001u64;
                    elms_mem=[||];
                    elms_child=[|
                      present_mem=  0x0000_0000_0000_0000u64;
                      present_child=0x0000_0000_0000_0001u64;
                      elms_mem=[||];
                      elms_child=[|
                        present_mem=  0x0000_0000_0000_0000u64;
                        present_child=0x0000_0000_0000_0001u64;
                        elms_mem=[||];
                        elms_child=[|
                          present_mem=  0x0000_0000_0000_0000u64;
                          present_child=0x0000_0000_0000_0001u64;
                          elms_mem=[||];
                          elms_child=[|
                            present_mem=  0x0000_0000_0000_0000u64;
                            present_child=0x0000_0000_0000_0001u64;
                            elms_mem=[||];
                            elms_child=[|
                              present_mem=  0x0000_0000_0000_0000u64;
                              present_child=0x0000_0000_0000_0001u64;
                              elms_mem=[||];
                              elms_child=[|
                                present_mem=  0x0000_0000_0000_0000u64;
                                present_child=0x0000_0000_0000_0001u64;
                                elms_mem=[||];
                                elms_child=[|
                                  present_mem=  0x0000_0000_0000_0000u64;
                                  present_child=0x0000_0000_0000_0001u64;
                                  elms_mem=[||];
                                  elms_child=[|
                                    present_mem=  0x0000_0000_0000_0000u64;
                                    present_child=0x0000_0000_0000_0001u64;
                                    elms_mem=[||];
                                    elms_child=[|
                                      present_mem=  0x0000_0000_0000_0000u64;
                                      present_child=0x0000_0000_0000_0001u64;
                                      elms_mem=[||];
                                      elms_child=[|
                                        present_mem=  0x0000_0000_0000_0000u64;
                                        present_child=0x0000_0000_0000_0001u64;
                                        elms_mem=[||];
                                        elms_child=[|
                                          present_mem=  0x0000_0000_0000_0000u64;
                                          present_child=0x0000_0000_0000_0001u64;
                                          elms_mem=[||];
                                          elms_child=[|
                                            present_mem=  0x0000_0000_0000_0000u64;
                                            present_child=0x0000_0000_0000_0001u64;
                                            elms_mem=[||];
                                            elms_child=[|
                                              present_mem=  0x0000_0000_0000_0000u64;
                                              present_child=0x0000_0000_0000_0001u64;
                                              elms_mem=[||];
                                              elms_child=[|
                                                present_mem=  0x0000_0000_0000_0000u64;
                                                present_child=0x0000_0000_0000_0001u64;
                                                elms_mem=[||];
                                                elms_child=[|
                                                  present_mem=  0x0000_0000_0000_0000u64;
                                                  present_child=0x0000_0000_0000_0001u64;
                                                  elms_mem=[||];
                                                  elms_child=[|
                                                    present_mem=  0xffff_ffff_ffff_ffffu64;
                                                    present_child=0xffff_ffff_ffff_ffffu64;
                                                    elms_mem=[|
                                                      42;
                                                      420
                                                    |];
                                                    elms_child=[||]
                                                  |]
                                                |]
                                              |]
                                            |]
                                          |]
                                        |]
                                      |]
                                    |]
                                  |]
                                |]
                              |]
                            |]
                          |]
                        |]
                      |]
                    |]
                  |]
                |]
              |]
            |]
          |]
        }
      } ->
      Set {
        length=2;
        root={
          present_mem=  0x0000_0000_0000_0000u64;
          present_child=0x0000_0000_0000_0001u64;
          elms_mem=[||];
          elms_child=[|
            present_mem=  0x0000_0000_0000_0000u64;
            present_child=0x0000_0000_0000_0001u64;
            elms_mem=[||];
            elms_child=[|
              present_mem=  0x0000_0000_0000_0000u64;
              present_child=0x0000_0000_0000_0001u64;
              elms_mem=[||];
              elms_child=[|
                present_mem=  0x0000_0000_0000_0000u64;
                present_child=0x0000_0000_0000_0001u64;
                elms_mem=[||];
                elms_child=[|
                  present_mem=  0x0000_0000_0000_0000u64;
                  present_child=0x0000_0000_0000_0001u64;
                  elms_mem=[||];
                  elms_child=[|
                    present_mem=  0x0000_0000_0000_0000u64;
                    present_child=0x0000_0000_0000_0001u64;
                    elms_mem=[||];
                    elms_child=[|
                      present_mem=  0x0000_0000_0000_0000u64;
                      present_child=0x0000_0000_0000_0001u64;
                      elms_mem=[||];
                      elms_child=[|
                        present_mem=  0x0000_0000_0000_0000u64;
                        present_child=0x0000_0000_0000_0001u64;
                        elms_mem=[||];
                        elms_child=[|
                          present_mem=  0x0000_0000_0000_0000u64;
                          present_child=0x0000_0000_0000_0001u64;
                          elms_mem=[||];
                          elms_child=[|
                            present_mem=  0x0000_0000_0000_0000u64;
                            present_child=0x0000_0000_0000_0001u64;
                            elms_mem=[||];
                            elms_child=[|
                              present_mem=  0x0000_0000_0000_0000u64;
                              present_child=0x0000_0000_0000_0001u64;
                              elms_mem=[||];
                              elms_child=[|
                                present_mem=  0x0000_0000_0000_0000u64;
                                present_child=0x0000_0000_0000_0001u64;
                                elms_mem=[||];
                                elms_child=[|
                                  present_mem=  0x0000_0000_0000_0000u64;
                                  present_child=0x0000_0000_0000_0001u64;
                                  elms_mem=[||];
                                  elms_child=[|
                                    present_mem=  0x0000_0000_0000_0000u64;
                                    present_child=0x0000_0000_0000_0001u64;
                                    elms_mem=[||];
                                    elms_child=[|
                                      present_mem=  0x0000_0000_0000_0000u64;
                                      present_child=0x0000_0000_0000_0001u64;
                                      elms_mem=[||];
                                      elms_child=[|
                                        present_mem=  0x0000_0000_0000_0000u64;
                                        present_child=0x0000_0000_0000_0001u64;
                                        elms_mem=[||];
                                        elms_child=[|
                                          present_mem=  0x0000_0000_0000_0000u64;
                                          present_child=0x0000_0000_0000_0001u64;
                                          elms_mem=[||];
                                          elms_child=[|
                                            present_mem=  0x0000_0000_0000_0000u64;
                                            present_child=0x0000_0000_0000_0001u64;
                                            elms_mem=[||];
                                            elms_child=[|
                                              present_mem=  0x0000_0000_0000_0000u64;
                                              present_child=0x0000_0000_0000_0001u64;
                                              elms_mem=[||];
                                              elms_child=[|
                                                present_mem=  0x0000_0000_0000_0000u64;
                                                present_child=0x0000_0000_0000_0001u64;
                                                elms_mem=[||];
                                                elms_child=[|
                                                  present_mem=  0x0000_0000_0000_0000u64;
                                                  present_child=0x0000_0000_0000_0001u64;
                                                  elms_mem=[||];
                                                  elms_child=[|
                                                    present_mem=  0xffff_ffff_ffff_ffffu64;
                                                    present_child=0xffff_ffff_ffff_ffffu64;
                                                    elms_mem=[|
                                                      42;
                                                      420
                                                    |];
                                                    elms_child=[||]
                                                  |]
                                                |]
                                              |]
                                            |]
                                          |]
                                        |]
                                      |]
                                    |]
                                  |]
                                |]
                              |]
                            |]
                          |]
                        |]
                      |]
                    |]
                  |]
                |]
              |]
            |]
          |]
        }
      }
    --- Member, length 1 -> 0. ---
    remove 0
      Set {
        length=1;
        root={
          present_mem=  0x0000_2000_0000_0000u64;
          present_child=0x0000_0000_0000_0000u64;
          elms_mem=[|
            0
          |];
          elms_child=[||]
        }
      } ->
      Set {
        length=0;
        root={
          present_mem=  0x0000_0000_0000_0000u64;
          present_child=0x0000_0000_0000_0000u64;
          elms_mem=[||];
          elms_child=[||]
        }
      }
    --- Member, length 2 -> 1. ---
    remove 1
      Set {
        length=2;
        root={
          present_mem=  0x0000_2000_0000_0020u64;
          present_child=0x0000_0000_0000_0000u64;
          elms_mem=[|
            1;
            0
          |];
          elms_child=[||]
        }
      } ->
      Set {
        length=1;
        root={
          present_mem=  0x0000_2000_0000_0000u64;
          present_child=0x0000_0000_0000_0000u64;
          elms_mem=[|
            0
          |];
          elms_child=[||]
        }
      }
    --- Member, length 3 -> 2. ---
    remove 2
      Set {
        length=3;
        root={
          present_mem=  0x0000_2200_0000_0020u64;
          present_child=0x0000_0000_0000_0000u64;
          elms_mem=[|
            1;
            2;
            0
          |];
          elms_child=[||]
        }
      } ->
      Set {
        length=2;
        root={
          present_mem=  0x0000_2000_0000_0020u64;
          present_child=0x0000_0000_0000_0000u64;
          elms_mem=[|
            1;
            0
          |];
          elms_child=[||]
        }
      }
    --- Member, subnode elms 2 -> 1. ---
    remove 66
      Set {
        length=3;
        root={
          present_mem=  0x0000_2000_0000_0000u64;
          present_child=0x0000_0000_0000_0020u64;
          elms_mem=[|
            0
          |];
          elms_child=[|
            present_mem=  0x4000_0000_0010_0000u64;
            present_child=0x0000_0000_0000_0000u64;
            elms_mem=[|
              66;
              1
            |];
            elms_child=[||]
          |]
        }
      } ->
      Set {
        length=2;
        root={
          present_mem=  0x0000_2000_0000_0020u64;
          present_child=0x0000_0000_0000_0000u64;
          elms_mem=[|
            1;
            0
          |];
          elms_child=[||]
        }
      }
    --- Member, subnode elms 3 -> 2. ---
    remove 91
      Set {
        length=4;
        root={
          present_mem=  0x0000_2000_0000_0000u64;
          present_child=0x0000_0000_0000_0020u64;
          elms_mem=[|
            0
          |];
          elms_child=[|
            present_mem=  0x4001_0000_0010_0000u64;
            present_child=0x0000_0000_0000_0000u64;
            elms_mem=[|
              66;
              91;
              1
            |];
            elms_child=[||]
          |]
        }
      } ->
      Set {
        length=3;
        root={
          present_mem=  0x0000_2000_0000_0000u64;
          present_child=0x0000_0000_0000_0020u64;
          elms_mem=[|
            0
          |];
          elms_child=[|
            present_mem=  0x4000_0000_0010_0000u64;
            present_child=0x0000_0000_0000_0000u64;
            elms_mem=[|
              66;
              1
            |];
            elms_child=[||]
          |]
        }
      }
    --- Member, collision list 3 -> 2. ---
    remove 420
      Set {
        length=3;
        root={
          present_mem=  0x0000_0000_0000_0000u64;
          present_child=0x0000_0000_0000_0001u64;
          elms_mem=[||];
          elms_child=[|
            present_mem=  0x0000_0000_0000_0000u64;
            present_child=0x0000_0000_0000_0001u64;
            elms_mem=[||];
            elms_child=[|
              present_mem=  0x0000_0000_0000_0000u64;
              present_child=0x0000_0000_0000_0001u64;
              elms_mem=[||];
              elms_child=[|
                present_mem=  0x0000_0000_0000_0000u64;
                present_child=0x0000_0000_0000_0001u64;
                elms_mem=[||];
                elms_child=[|
                  present_mem=  0x0000_0000_0000_0000u64;
                  present_child=0x0000_0000_0000_0001u64;
                  elms_mem=[||];
                  elms_child=[|
                    present_mem=  0x0000_0000_0000_0000u64;
                    present_child=0x0000_0000_0000_0001u64;
                    elms_mem=[||];
                    elms_child=[|
                      present_mem=  0x0000_0000_0000_0000u64;
                      present_child=0x0000_0000_0000_0001u64;
                      elms_mem=[||];
                      elms_child=[|
                        present_mem=  0x0000_0000_0000_0000u64;
                        present_child=0x0000_0000_0000_0001u64;
                        elms_mem=[||];
                        elms_child=[|
                          present_mem=  0x0000_0000_0000_0000u64;
                          present_child=0x0000_0000_0000_0001u64;
                          elms_mem=[||];
                          elms_child=[|
                            present_mem=  0x0000_0000_0000_0000u64;
                            present_child=0x0000_0000_0000_0001u64;
                            elms_mem=[||];
                            elms_child=[|
                              present_mem=  0x0000_0000_0000_0000u64;
                              present_child=0x0000_0000_0000_0001u64;
                              elms_mem=[||];
                              elms_child=[|
                                present_mem=  0x0000_0000_0000_0000u64;
                                present_child=0x0000_0000_0000_0001u64;
                                elms_mem=[||];
                                elms_child=[|
                                  present_mem=  0x0000_0000_0000_0000u64;
                                  present_child=0x0000_0000_0000_0001u64;
                                  elms_mem=[||];
                                  elms_child=[|
                                    present_mem=  0x0000_0000_0000_0000u64;
                                    present_child=0x0000_0000_0000_0001u64;
                                    elms_mem=[||];
                                    elms_child=[|
                                      present_mem=  0x0000_0000_0000_0000u64;
                                      present_child=0x0000_0000_0000_0001u64;
                                      elms_mem=[||];
                                      elms_child=[|
                                        present_mem=  0x0000_0000_0000_0000u64;
                                        present_child=0x0000_0000_0000_0001u64;
                                        elms_mem=[||];
                                        elms_child=[|
                                          present_mem=  0x0000_0000_0000_0000u64;
                                          present_child=0x0000_0000_0000_0001u64;
                                          elms_mem=[||];
                                          elms_child=[|
                                            present_mem=  0x0000_0000_0000_0000u64;
                                            present_child=0x0000_0000_0000_0001u64;
                                            elms_mem=[||];
                                            elms_child=[|
                                              present_mem=  0x0000_0000_0000_0000u64;
                                              present_child=0x0000_0000_0000_0001u64;
                                              elms_mem=[||];
                                              elms_child=[|
                                                present_mem=  0x0000_0000_0000_0000u64;
                                                present_child=0x0000_0000_0000_0001u64;
                                                elms_mem=[||];
                                                elms_child=[|
                                                  present_mem=  0x0000_0000_0000_0000u64;
                                                  present_child=0x0000_0000_0000_0001u64;
                                                  elms_mem=[||];
                                                  elms_child=[|
                                                    present_mem=  0xffff_ffff_ffff_ffffu64;
                                                    present_child=0xffff_ffff_ffff_ffffu64;
                                                    elms_mem=[|
                                                      42;
                                                      420;
                                                      4200
                                                    |];
                                                    elms_child=[||]
                                                  |]
                                                |]
                                              |]
                                            |]
                                          |]
                                        |]
                                      |]
                                    |]
                                  |]
                                |]
                              |]
                            |]
                          |]
                        |]
                      |]
                    |]
                  |]
                |]
              |]
            |]
          |]
        }
      } ->
      Set {
        length=2;
        root={
          present_mem=  0x0000_0000_0000_0000u64;
          present_child=0x0000_0000_0000_0001u64;
          elms_mem=[||];
          elms_child=[|
            present_mem=  0x0000_0000_0000_0000u64;
            present_child=0x0000_0000_0000_0001u64;
            elms_mem=[||];
            elms_child=[|
              present_mem=  0x0000_0000_0000_0000u64;
              present_child=0x0000_0000_0000_0001u64;
              elms_mem=[||];
              elms_child=[|
                present_mem=  0x0000_0000_0000_0000u64;
                present_child=0x0000_0000_0000_0001u64;
                elms_mem=[||];
                elms_child=[|
                  present_mem=  0x0000_0000_0000_0000u64;
                  present_child=0x0000_0000_0000_0001u64;
                  elms_mem=[||];
                  elms_child=[|
                    present_mem=  0x0000_0000_0000_0000u64;
                    present_child=0x0000_0000_0000_0001u64;
                    elms_mem=[||];
                    elms_child=[|
                      present_mem=  0x0000_0000_0000_0000u64;
                      present_child=0x0000_0000_0000_0001u64;
                      elms_mem=[||];
                      elms_child=[|
                        present_mem=  0x0000_0000_0000_0000u64;
                        present_child=0x0000_0000_0000_0001u64;
                        elms_mem=[||];
                        elms_child=[|
                          present_mem=  0x0000_0000_0000_0000u64;
                          present_child=0x0000_0000_0000_0001u64;
                          elms_mem=[||];
                          elms_child=[|
                            present_mem=  0x0000_0000_0000_0000u64;
                            present_child=0x0000_0000_0000_0001u64;
                            elms_mem=[||];
                            elms_child=[|
                              present_mem=  0x0000_0000_0000_0000u64;
                              present_child=0x0000_0000_0000_0001u64;
                              elms_mem=[||];
                              elms_child=[|
                                present_mem=  0x0000_0000_0000_0000u64;
                                present_child=0x0000_0000_0000_0001u64;
                                elms_mem=[||];
                                elms_child=[|
                                  present_mem=  0x0000_0000_0000_0000u64;
                                  present_child=0x0000_0000_0000_0001u64;
                                  elms_mem=[||];
                                  elms_child=[|
                                    present_mem=  0x0000_0000_0000_0000u64;
                                    present_child=0x0000_0000_0000_0001u64;
                                    elms_mem=[||];
                                    elms_child=[|
                                      present_mem=  0x0000_0000_0000_0000u64;
                                      present_child=0x0000_0000_0000_0001u64;
                                      elms_mem=[||];
                                      elms_child=[|
                                        present_mem=  0x0000_0000_0000_0000u64;
                                        present_child=0x0000_0000_0000_0001u64;
                                        elms_mem=[||];
                                        elms_child=[|
                                          present_mem=  0x0000_0000_0000_0000u64;
                                          present_child=0x0000_0000_0000_0001u64;
                                          elms_mem=[||];
                                          elms_child=[|
                                            present_mem=  0x0000_0000_0000_0000u64;
                                            present_child=0x0000_0000_0000_0001u64;
                                            elms_mem=[||];
                                            elms_child=[|
                                              present_mem=  0x0000_0000_0000_0000u64;
                                              present_child=0x0000_0000_0000_0001u64;
                                              elms_mem=[||];
                                              elms_child=[|
                                                present_mem=  0x0000_0000_0000_0000u64;
                                                present_child=0x0000_0000_0000_0001u64;
                                                elms_mem=[||];
                                                elms_child=[|
                                                  present_mem=  0x0000_0000_0000_0000u64;
                                                  present_child=0x0000_0000_0000_0001u64;
                                                  elms_mem=[||];
                                                  elms_child=[|
                                                    present_mem=  0xffff_ffff_ffff_ffffu64;
                                                    present_child=0xffff_ffff_ffff_ffffu64;
                                                    elms_mem=[|
                                                      42;
                                                      4200
                                                    |];
                                                    elms_child=[||]
                                                  |]
                                                |]
                                              |]
                                            |]
                                          |]
                                        |]
                                      |]
                                    |]
                                  |]
                                |]
                              |]
                            |]
                          |]
                        |]
                      |]
                    |]
                  |]
                |]
              |]
            |]
          |]
        }
      }
    --- Member, collision list 2 -> 1, leaf elms 2 -> 2. ---
    remove 420
      Set {
        length=3;
        root={
          present_mem=  0x0000_0000_0000_0000u64;
          present_child=0x0000_0000_0000_0001u64;
          elms_mem=[||];
          elms_child=[|
            present_mem=  0x0000_0000_0000_0000u64;
            present_child=0x0000_0000_0000_0001u64;
            elms_mem=[||];
            elms_child=[|
              present_mem=  0x0000_0000_0000_0000u64;
              present_child=0x0000_0000_0000_0001u64;
              elms_mem=[||];
              elms_child=[|
                present_mem=  0x0000_0000_0000_0000u64;
                present_child=0x0000_0000_0000_0001u64;
                elms_mem=[||];
                elms_child=[|
                  present_mem=  0x0000_0000_0000_0000u64;
                  present_child=0x0000_0000_0000_0001u64;
                  elms_mem=[||];
                  elms_child=[|
                    present_mem=  0x0000_0000_0000_0000u64;
                    present_child=0x0000_0000_0000_0001u64;
                    elms_mem=[||];
                    elms_child=[|
                      present_mem=  0x0000_0000_0000_0000u64;
                      present_child=0x0000_0000_0000_0001u64;
                      elms_mem=[||];
                      elms_child=[|
                        present_mem=  0x0000_0000_0000_0000u64;
                        present_child=0x0000_0000_0000_0001u64;
                        elms_mem=[||];
                        elms_child=[|
                          present_mem=  0x0000_0000_0000_0000u64;
                          present_child=0x0000_0000_0000_0001u64;
                          elms_mem=[||];
                          elms_child=[|
                            present_mem=  0x0000_0000_0000_0000u64;
                            present_child=0x0000_0000_0000_0001u64;
                            elms_mem=[||];
                            elms_child=[|
                              present_mem=  0x0000_0000_0000_0000u64;
                              present_child=0x0000_0000_0000_0001u64;
                              elms_mem=[||];
                              elms_child=[|
                                present_mem=  0x0000_0000_0000_0000u64;
                                present_child=0x0000_0000_0000_0001u64;
                                elms_mem=[||];
                                elms_child=[|
                                  present_mem=  0x0000_0000_0000_0000u64;
                                  present_child=0x0000_0000_0000_0001u64;
                                  elms_mem=[||];
                                  elms_child=[|
                                    present_mem=  0x0000_0000_0000_0000u64;
                                    present_child=0x0000_0000_0000_0001u64;
                                    elms_mem=[||];
                                    elms_child=[|
                                      present_mem=  0x0000_0000_0000_0000u64;
                                      present_child=0x0000_0000_0000_0001u64;
                                      elms_mem=[||];
                                      elms_child=[|
                                        present_mem=  0x0000_0000_0000_0000u64;
                                        present_child=0x0000_0000_0000_0001u64;
                                        elms_mem=[||];
                                        elms_child=[|
                                          present_mem=  0x0000_0000_0000_0000u64;
                                          present_child=0x0000_0000_0000_0001u64;
                                          elms_mem=[||];
                                          elms_child=[|
                                            present_mem=  0x0000_0000_0000_0000u64;
                                            present_child=0x0000_0000_0000_0001u64;
                                            elms_mem=[||];
                                            elms_child=[|
                                              present_mem=  0x0000_0000_0000_0000u64;
                                              present_child=0x0000_0000_0000_0001u64;
                                              elms_mem=[||];
                                              elms_child=[|
                                                present_mem=  0x0000_0000_0000_0000u64;
                                                present_child=0x0000_0000_0000_0001u64;
                                                elms_mem=[||];
                                                elms_child=[|
                                                  present_mem=  0x0000_0000_0000_0002u64;
                                                  present_child=0x0000_0000_0000_0001u64;
                                                  elms_mem=[|
                                                    421
                                                  |];
                                                  elms_child=[|
                                                    present_mem=  0xffff_ffff_ffff_ffffu64;
                                                    present_child=0xffff_ffff_ffff_ffffu64;
                                                    elms_mem=[|
                                                      42;
                                                      420
                                                    |];
                                                    elms_child=[||]
                                                  |]
                                                |]
                                              |]
                                            |]
                                          |]
                                        |]
                                      |]
                                    |]
                                  |]
                                |]
                              |]
                            |]
                          |]
                        |]
                      |]
                    |]
                  |]
                |]
              |]
            |]
          |]
        }
      } ->
      Set {
        length=2;
        root={
          present_mem=  0x0000_0000_0000_0000u64;
          present_child=0x0000_0000_0000_0001u64;
          elms_mem=[||];
          elms_child=[|
            present_mem=  0x0000_0000_0000_0000u64;
            present_child=0x0000_0000_0000_0001u64;
            elms_mem=[||];
            elms_child=[|
              present_mem=  0x0000_0000_0000_0000u64;
              present_child=0x0000_0000_0000_0001u64;
              elms_mem=[||];
              elms_child=[|
                present_mem=  0x0000_0000_0000_0000u64;
                present_child=0x0000_0000_0000_0001u64;
                elms_mem=[||];
                elms_child=[|
                  present_mem=  0x0000_0000_0000_0000u64;
                  present_child=0x0000_0000_0000_0001u64;
                  elms_mem=[||];
                  elms_child=[|
                    present_mem=  0x0000_0000_0000_0000u64;
                    present_child=0x0000_0000_0000_0001u64;
                    elms_mem=[||];
                    elms_child=[|
                      present_mem=  0x0000_0000_0000_0000u64;
                      present_child=0x0000_0000_0000_0001u64;
                      elms_mem=[||];
                      elms_child=[|
                        present_mem=  0x0000_0000_0000_0000u64;
                        present_child=0x0000_0000_0000_0001u64;
                        elms_mem=[||];
                        elms_child=[|
                          present_mem=  0x0000_0000_0000_0000u64;
                          present_child=0x0000_0000_0000_0001u64;
                          elms_mem=[||];
                          elms_child=[|
                            present_mem=  0x0000_0000_0000_0000u64;
                            present_child=0x0000_0000_0000_0001u64;
                            elms_mem=[||];
                            elms_child=[|
                              present_mem=  0x0000_0000_0000_0000u64;
                              present_child=0x0000_0000_0000_0001u64;
                              elms_mem=[||];
                              elms_child=[|
                                present_mem=  0x0000_0000_0000_0000u64;
                                present_child=0x0000_0000_0000_0001u64;
                                elms_mem=[||];
                                elms_child=[|
                                  present_mem=  0x0000_0000_0000_0000u64;
                                  present_child=0x0000_0000_0000_0001u64;
                                  elms_mem=[||];
                                  elms_child=[|
                                    present_mem=  0x0000_0000_0000_0000u64;
                                    present_child=0x0000_0000_0000_0001u64;
                                    elms_mem=[||];
                                    elms_child=[|
                                      present_mem=  0x0000_0000_0000_0000u64;
                                      present_child=0x0000_0000_0000_0001u64;
                                      elms_mem=[||];
                                      elms_child=[|
                                        present_mem=  0x0000_0000_0000_0000u64;
                                        present_child=0x0000_0000_0000_0001u64;
                                        elms_mem=[||];
                                        elms_child=[|
                                          present_mem=  0x0000_0000_0000_0000u64;
                                          present_child=0x0000_0000_0000_0001u64;
                                          elms_mem=[||];
                                          elms_child=[|
                                            present_mem=  0x0000_0000_0000_0000u64;
                                            present_child=0x0000_0000_0000_0001u64;
                                            elms_mem=[||];
                                            elms_child=[|
                                              present_mem=  0x0000_0000_0000_0000u64;
                                              present_child=0x0000_0000_0000_0001u64;
                                              elms_mem=[||];
                                              elms_child=[|
                                                present_mem=  0x0000_0000_0000_0000u64;
                                                present_child=0x0000_0000_0000_0001u64;
                                                elms_mem=[||];
                                                elms_child=[|
                                                  present_mem=  0x0000_0000_0000_0003u64;
                                                  present_child=0x0000_0000_0000_0000u64;
                                                  elms_mem=[|
                                                    42;
                                                    421
                                                  |];
                                                  elms_child=[||]
                                                |]
                                              |]
                                            |]
                                          |]
                                        |]
                                      |]
                                    |]
                                  |]
                                |]
                              |]
                            |]
                          |]
                        |]
                      |]
                    |]
                  |]
                |]
              |]
            |]
          |]
        }
      }
    --- Member, collision list 2 -> 1, leaf elms 2 -> 1. ---
    remove 420
      Set {
        length=2;
        root={
          present_mem=  0x0000_0000_0000_0000u64;
          present_child=0x0000_0000_0000_0001u64;
          elms_mem=[||];
          elms_child=[|
            present_mem=  0x0000_0000_0000_0000u64;
            present_child=0x0000_0000_0000_0001u64;
            elms_mem=[||];
            elms_child=[|
              present_mem=  0x0000_0000_0000_0000u64;
              present_child=0x0000_0000_0000_0001u64;
              elms_mem=[||];
              elms_child=[|
                present_mem=  0x0000_0000_0000_0000u64;
                present_child=0x0000_0000_0000_0001u64;
                elms_mem=[||];
                elms_child=[|
                  present_mem=  0x0000_0000_0000_0000u64;
                  present_child=0x0000_0000_0000_0001u64;
                  elms_mem=[||];
                  elms_child=[|
                    present_mem=  0x0000_0000_0000_0000u64;
                    present_child=0x0000_0000_0000_0001u64;
                    elms_mem=[||];
                    elms_child=[|
                      present_mem=  0x0000_0000_0000_0000u64;
                      present_child=0x0000_0000_0000_0001u64;
                      elms_mem=[||];
                      elms_child=[|
                        present_mem=  0x0000_0000_0000_0000u64;
                        present_child=0x0000_0000_0000_0001u64;
                        elms_mem=[||];
                        elms_child=[|
                          present_mem=  0x0000_0000_0000_0000u64;
                          present_child=0x0000_0000_0000_0001u64;
                          elms_mem=[||];
                          elms_child=[|
                            present_mem=  0x0000_0000_0000_0000u64;
                            present_child=0x0000_0000_0000_0001u64;
                            elms_mem=[||];
                            elms_child=[|
                              present_mem=  0x0000_0000_0000_0000u64;
                              present_child=0x0000_0000_0000_0001u64;
                              elms_mem=[||];
                              elms_child=[|
                                present_mem=  0x0000_0000_0000_0000u64;
                                present_child=0x0000_0000_0000_0001u64;
                                elms_mem=[||];
                                elms_child=[|
                                  present_mem=  0x0000_0000_0000_0000u64;
                                  present_child=0x0000_0000_0000_0001u64;
                                  elms_mem=[||];
                                  elms_child=[|
                                    present_mem=  0x0000_0000_0000_0000u64;
                                    present_child=0x0000_0000_0000_0001u64;
                                    elms_mem=[||];
                                    elms_child=[|
                                      present_mem=  0x0000_0000_0000_0000u64;
                                      present_child=0x0000_0000_0000_0001u64;
                                      elms_mem=[||];
                                      elms_child=[|
                                        present_mem=  0x0000_0000_0000_0000u64;
                                        present_child=0x0000_0000_0000_0001u64;
                                        elms_mem=[||];
                                        elms_child=[|
                                          present_mem=  0x0000_0000_0000_0000u64;
                                          present_child=0x0000_0000_0000_0001u64;
                                          elms_mem=[||];
                                          elms_child=[|
                                            present_mem=  0x0000_0000_0000_0000u64;
                                            present_child=0x0000_0000_0000_0001u64;
                                            elms_mem=[||];
                                            elms_child=[|
                                              present_mem=  0x0000_0000_0000_0000u64;
                                              present_child=0x0000_0000_0000_0001u64;
                                              elms_mem=[||];
                                              elms_child=[|
                                                present_mem=  0x0000_0000_0000_0000u64;
                                                present_child=0x0000_0000_0000_0001u64;
                                                elms_mem=[||];
                                                elms_child=[|
                                                  present_mem=  0x0000_0000_0000_0000u64;
                                                  present_child=0x0000_0000_0000_0001u64;
                                                  elms_mem=[||];
                                                  elms_child=[|
                                                    present_mem=  0xffff_ffff_ffff_ffffu64;
                                                    present_child=0xffff_ffff_ffff_ffffu64;
                                                    elms_mem=[|
                                                      42;
                                                      420
                                                    |];
                                                    elms_child=[||]
                                                  |]
                                                |]
                                              |]
                                            |]
                                          |]
                                        |]
                                      |]
                                    |]
                                  |]
                                |]
                              |]
                            |]
                          |]
                        |]
                      |]
                    |]
                  |]
                |]
              |]
            |]
          |]
        }
      } ->
      Set {
        length=1;
        root={
          present_mem=  0x0000_0000_0000_0001u64;
          present_child=0x0000_0000_0000_0000u64;
          elms_mem=[|
            42
          |];
          elms_child=[||]
        }
      }
    |}]

let%expect_test "of_list,to_list,to_array" =
  let open Format in
  printf "@[<h>";
  let test ms = begin
    let set = of_list (module UsizeTestCmper) ms in
    printf "of_list %a; to_list -> %a; to_array -> %a\n"
      (List.pp Usize.pp) ms
      (List.pp Usize.pp) (to_list set)
      (Array.pp Usize.pp) (to_array set)
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
  List.iter test_lists ~f:(fun ms ->
    test ms
  );
  printf "@]";

  [%expect{|
    of_list []; to_list -> []; to_array -> [||]
    of_list [0]; to_list -> [0]; to_array -> [|0|]
    of_list [0; 1]; to_list -> [0; 1]; to_array -> [|1; 0|]
    of_list [0; 1; 2]; to_list -> [0; 2; 1]; to_array -> [|1; 2; 0|]
    of_list [0; 1; 66]; to_list -> [1; 66; 0]; to_array -> [|66; 1; 0|]
    of_list [0; 1; 66; 91]; to_list -> [1; 91; 66; 0]; to_array -> [|66; 91; 1; 0|]
    of_list [42; 420]; to_list -> [420; 42]; to_array -> [|42; 420|]
    of_list [42; 420; 421]; to_list -> [420; 42; 421]; to_array -> [|42; 420; 421|]
    of_list [42; 420; 4200]; to_list -> [4200; 420; 42]; to_array -> [|42; 420; 4200|]
    |}]

let%expect_test "fold_until" =
  let test ms = begin
    let set = of_list (module UsizeTestCmper) ms in
    (* Compute the number of elements in the triangle defined by folding n
       times, each time terminating upon encounter of a distinct set member.
       The size of the triangle is insensitive to fold order. *)
    assert ((List.length ms) = (length set));
    let n = length set in
    let triangle_sum = List.fold ms ~init:0 ~f:(fun accum m ->
      accum + fold_until set ~init:0 ~f:(fun accum a ->
        (succ accum), (m = a)
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
  List.iter test_lists ~f:(fun ms ->
    test ms
  );

  [%expect{|
    |}]

let%expect_test "fold2_until" =
  let test ms0 ms1 = begin
    let set0 = of_list (module UsizeTestCmper) ms0 in
    let set1 = of_list (module UsizeTestCmper) ms1 in
    let set = union set0 set1 in
    let ms = to_list set in
    (* Compute the number of elements in the triangle defined by folding n
       times, each time terminating upon encounter of a distinct set member.
       The size of the triangle is insensitive to fold order. *)
    assert ((List.length ms) = (length set));
    let n = length set in
    let triangle_sum = List.fold ms ~init:0 ~f:(fun accum m ->
      accum + fold2_until set0 set1 ~init:0 ~f:(fun accum a0_opt a1_opt ->
        match a0_opt, a1_opt with
        | Some a, Some _
        | Some a, None
        | None, Some a -> (succ accum), (m = a)
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
  List.iteri test_lists ~f:(fun i ms0 ->
    List.iteri test_lists ~f:(fun j ms1 ->
      if i <= j then test ms0 ms1
    )
  );

  [%expect{|
    |}]

let%expect_test "fold2" =
  let open Format in
  printf "@[";
  let pp_pair ppf (a0_opt, a1_opt) = begin
    fprintf ppf "(%a, %a)"
      (Option.pp Usize.pp) a0_opt
      (Option.pp Usize.pp) a1_opt
  end in
  let test ms0 ms1 = begin
    let set0 = of_list (module UsizeTestCmper) ms0 in
    let set1 = of_list (module UsizeTestCmper) ms1 in
    let pairs = fold2 ~init:[] ~f:(fun accum a0_opt a1_opt ->
      (a0_opt, a1_opt) :: accum
    ) set0 set1 in
    printf "fold2 %a %a -> %a@\n"
      (List.pp Usize.pp) ms0
      (List.pp Usize.pp) ms1
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
  List.iteri test_lists ~f:(fun i ms0 ->
    List.iteri test_lists ~f:(fun j ms1 ->
      if i <= j then test ms0 ms1
    )
  );
  printf "@]";

  [%expect{|
    fold2 [] [] -> []
    fold2 [] [0] -> [(None, Some 0)]
    fold2 [] [0; 1] -> [(None, Some 0); (None, Some 1)]
    fold2 [] [0; 1; 2] -> [(None, Some 0); (None, Some 2); (None, Some 1)]
    fold2 [] [0; 1; 66] -> [(None, Some 0); (None, Some 1); (None, Some 66)]
    fold2 [] [0; 1; 66; 91] -> [(None, Some 0); (None, Some 1); (None, Some 91); (None, Some 66)]
    fold2 [] [42; 420] -> [(None, Some 420); (None, Some 42)]
    fold2 [] [42; 420; 421] -> [(None, Some 421); (None, Some 420); (None, Some 42)]
    fold2 [] [42; 420; 4200] -> [(None, Some 4200); (None, Some 420); (None, Some 42)]
    fold2 [0] [0] -> [(Some 0, Some 0)]
    fold2 [0] [0; 1] -> [(Some 0, Some 0); (None, Some 1)]
    fold2 [0] [0; 1; 2] -> [(Some 0, Some 0); (None, Some 2); (None, Some 1)]
    fold2 [0] [0; 1; 66] -> [(Some 0, Some 0); (None, Some 1); (None, Some 66)]
    fold2 [0] [0; 1; 66; 91] -> [(Some 0, Some 0); (None, Some 1); (None, Some 91); (None, Some 66)]
    fold2 [0] [42; 420] -> [(Some 0, None); (None, Some 420); (None, Some 42)]
    fold2 [0] [42; 420; 421] -> [(Some 0, None); (None, Some 421); (None, Some 420); (None, Some 42)]
    fold2 [0] [42; 420; 4200] -> [(Some 0, None); (None, Some 4200); (None, Some 420); (None, Some 42)]
    fold2 [0; 1] [0; 1] -> [(Some 0, Some 0); (Some 1, Some 1)]
    fold2 [0; 1] [0; 1; 2] -> [(Some 0, Some 0); (None, Some 2); (Some 1, Some 1)]
    fold2 [0; 1] [0; 1; 66] -> [(Some 0, Some 0); (Some 1, Some 1); (None, Some 66)]
    fold2 [0; 1] [0; 1; 66; 91] -> [(Some 0, Some 0); (Some 1, Some 1); (None, Some 91); (None, Some 66)]
    fold2 [0; 1] [42; 420] -> [(Some 0, None); (Some 1, None); (None, Some 420); (None, Some 42)]
    fold2 [0; 1] [42; 420; 421] -> [(Some 0, None); (Some 1, None); (None, Some 421); (None, Some 420); (None, Some 42)]
    fold2 [0; 1] [42; 420; 4200] -> [(Some 0, None); (Some 1, None); (None, Some 4200); (None, Some 420); (None, Some 42)]
    fold2 [0; 1; 2] [0; 1; 2] -> [(Some 0, Some 0); (Some 2, Some 2); (Some 1, Some 1)]
    fold2 [0; 1; 2] [0; 1; 66] -> [(Some 0, Some 0); (Some 2, None); (Some 1, Some 1); (None, Some 66)]
    fold2 [0; 1; 2] [0; 1; 66; 91] -> [(Some 0, Some 0); (Some 2, None); (Some 1, Some 1); (None, Some 91); (None, Some 66)]
    fold2 [0; 1; 2] [42; 420] -> [(Some 0, None); (Some 2, None); (Some 1, None); (None, Some 420); (None, Some 42)]
    fold2 [0; 1; 2] [42; 420; 421] -> [(Some 0, None); (Some 2, None); (Some 1, None); (None, Some 421); (None, Some 420); (None, Some 42)]
    fold2 [0; 1; 2] [42; 420; 4200] -> [(Some 0, None); (Some 2, None); (Some 1, None); (None, Some 4200); (None, Some 420); (None, Some 42)]
    fold2 [0; 1; 66] [0; 1; 66] -> [(Some 0, Some 0); (Some 1, Some 1); (Some 66, Some 66)]
    fold2 [0; 1; 66] [0; 1; 66; 91] -> [(Some 0, Some 0); (Some 1, Some 1); (None, Some 91); (Some 66, Some 66)]
    fold2 [0; 1; 66] [42; 420] -> [(Some 0, None); (Some 1, None); (Some 66, None); (None, Some 420); (None, Some 42)]
    fold2 [0; 1; 66] [42; 420; 421] -> [(Some 0, None); (Some 1, None); (Some 66, None); (None, Some 421); (None, Some 420); (None, Some 42)]
    fold2 [0; 1; 66] [42; 420; 4200] -> [(Some 0, None); (Some 1, None); (Some 66, None); (None, Some 4200); (None, Some 420); (None, Some 42)]
    fold2 [0; 1; 66; 91] [0; 1; 66; 91] -> [(Some 0, Some 0); (Some 1, Some 1); (Some 91, Some 91); (Some 66, Some 66)]
    fold2 [0; 1; 66; 91] [42; 420] -> [(Some 0, None); (Some 1, None); (Some 91, None); (Some 66, None); (None, Some 420); (None, Some 42)]
    fold2 [0; 1; 66; 91] [42; 420; 421] -> [(Some 0, None); (Some 1, None); (Some 91, None); (Some 66, None); (None, Some 421); (None, Some 420); (None, Some 42)]
    fold2 [0; 1; 66; 91] [42; 420; 4200] -> [(Some 0, None); (Some 1, None); (Some 91, None); (Some 66, None); (None, Some 4200); (None, Some 420); (None, Some 42)]
    fold2 [42; 420] [42; 420] -> [(Some 420, Some 420); (Some 42, Some 42)]
    fold2 [42; 420] [42; 420; 421] -> [(None, Some 421); (Some 420, Some 420); (Some 42, Some 42)]
    fold2 [42; 420] [42; 420; 4200] -> [(None, Some 4200); (Some 420, Some 420); (Some 42, Some 42)]
    fold2 [42; 420; 421] [42; 420; 421] -> [(Some 421, Some 421); (Some 420, Some 420); (Some 42, Some 42)]
    fold2 [42; 420; 421] [42; 420; 4200] -> [(Some 421, None); (None, Some 4200); (Some 420, Some 420); (Some 42, Some 42)]
    fold2 [42; 420; 4200] [42; 420; 4200] -> [(Some 4200, Some 4200); (Some 420, Some 420); (Some 42, Some 42)]
    |}]

let%expect_test "iter2,equal" =
  let open Format in
  printf "@[";
  let test_equal ms0 ms1 = begin
    let set0 = of_list (module UsizeTestCmper) ms0 in
    let set1 = of_list (module UsizeTestCmper) ms1 in
    assert (equal set0 set1);
    iter2 ~f:(fun a0_opt a1_opt ->
      match a0_opt, a1_opt with
      | Some _, Some _ -> ()
      | None, Some _
      | Some _, None -> begin
          printf "Should be equal:@,%a@,%a@\n" pp set0 pp set1;
          assert false;
        end
      | None, None -> not_reached ()
    ) set0 set1
  end in
  let test_disjoint ms0 ms1 = begin
    let set0 = of_list (module UsizeTestCmper) ms0 in
    let set1 = of_list (module UsizeTestCmper) ms1 in
    assert (not (equal set0 set1));
    iter2 ~f:(fun a0_opt a1_opt ->
      match a0_opt, a1_opt with
      | Some _, Some _ -> begin
          printf "Should be disjoint:@,%a@,%a@\n" pp set0 pp set1;
          assert false;
        end
      | None, Some _
      | Some _, None -> ()
      | None, None -> not_reached ()
    ) set0 set1
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
  List.iter test_lists ~f:(fun ms ->
    test_equal ms ms;
    test_equal ms (List.rev ms);
    test_equal (List.rev ms) ms;
    test_equal (List.rev ms) (List.rev ms);
  );
  List.iter test_disjoint_list_pairs ~f:(fun (ms0, ms1) ->
    test_disjoint ms0 ms1;
    test_disjoint ms0 (List.rev ms1);
    test_disjoint (List.rev ms0) ms1;
    test_disjoint (List.rev ms0) (List.rev ms1);
  );
  printf "@]";

  [%expect{|
    |}]

let%expect_test "union" =
  let test ms0 ms1 = begin
    let set0 = of_list (module UsizeTestCmper) ms0 in
    let set1 = of_list (module UsizeTestCmper) ms1 in
    let set = union set0 set1 in
    let ms = to_list set in
    List.iter ms0 ~f:(fun m -> assert ((mem m set) && (mem m set0)));
    List.iter ms1 ~f:(fun m -> assert ((mem m set) && (mem m set1)));
    List.iter ms ~f:(fun m -> assert ((mem m set0) || (mem m set1)));
  end in
  let test_disjoint ms0 ms1 = begin
    let set0 = of_list (module UsizeTestCmper) ms0 in
    let set1 = of_list (module UsizeTestCmper) ms1 in
    let set = union set0 set1 in
    assert ((length set) = (length set0) + (length set1));
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
  List.iteri test_lists ~f:(fun i ms0 ->
    List.iteri test_lists ~f:(fun j ms1 ->
      if i <= j then begin
        test ms0 ms1;
        test ms1 ms0
      end
    )
  );
  List.iter test_disjoint_list_pairs ~f:(fun (ms0, ms1) ->
    test_disjoint ms0 ms1;
    test_disjoint ms0 (List.rev ms1);
    test_disjoint (List.rev ms0) ms1;
    test_disjoint (List.rev ms0) (List.rev ms1);
  );

  [%expect{|
    |}]

let%expect_test "inter" =
  let test ms0 ms1 = begin
    let set0 = of_list (module UsizeTestCmper) ms0 in
    let set1 = of_list (module UsizeTestCmper) ms1 in
    let set = inter set0 set1 in
    let ms = to_list set in
    List.iter ms0 ~f:(fun m -> assert ((mem m set) || (not (mem m set1))));
    List.iter ms1 ~f:(fun m -> assert ((mem m set) || (not (mem m set0))));
    List.iter ms ~f:(fun m -> assert ((mem m set0) && (mem m set1)));
  end in
  let test_disjoint ms0 ms1 = begin
    let set0 = of_list (module UsizeTestCmper) ms0 in
    let set1 = of_list (module UsizeTestCmper) ms1 in
    let set = inter set0 set1 in
    assert ((length set) = 0);
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
  List.iteri test_lists ~f:(fun i ms0 ->
    List.iteri test_lists ~f:(fun j ms1 ->
      if i <= j then begin
        test ms0 ms1;
        test ms1 ms0
      end
    )
  );
  List.iter test_disjoint_list_pairs ~f:(fun (ms0, ms1) ->
    test_disjoint ms0 ms1;
    test_disjoint ms0 (List.rev ms1);
    test_disjoint (List.rev ms0) ms1;
    test_disjoint (List.rev ms0) (List.rev ms1);
  );

  [%expect{|
    |}]

let%expect_test "diff" =
  let test ms0 ms1 = begin
    let set0 = of_list (module UsizeTestCmper) ms0 in
    let set1 = of_list (module UsizeTestCmper) ms1 in
    let set = diff set0 set1 in
    let ms = to_list set in
    List.iter ms0 ~f:(fun m -> assert ((mem m set) || (mem m set1)));
    List.iter ms1 ~f:(fun m -> assert (not (mem m set)));
    List.iter ms ~f:(fun m -> assert ((mem m set0) && (not (mem m set1))));
  end in
  let test_disjoint ms0 ms1 = begin
    let set0 = of_list (module UsizeTestCmper) ms0 in
    let set1 = of_list (module UsizeTestCmper) ms1 in
    let set = diff set0 set1 in
    assert ((length set) = (length set0));
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
  List.iteri test_lists ~f:(fun i ms0 ->
    List.iteri test_lists ~f:(fun j ms1 ->
      if i <= j then begin
        test ms0 ms1;
        test ms1 ms0;
      end
    )
  );
  List.iter test_disjoint_list_pairs ~f:(fun (ms0, ms1) ->
    test_disjoint ms0 ms1;
    test_disjoint ms0 (List.rev ms1);
    test_disjoint (List.rev ms0) ms1;
    test_disjoint (List.rev ms0) (List.rev ms1);
  );

  [%expect{|
    |}]

let%expect_test "reduce" =
  let open Format in
  printf "@[<h>";
  let test ms = begin
    let set = of_list (module UsizeTestCmper) ms in
    let sum = reduce ~f:( + ) set in
    printf "reduce ~f:( + ) %a -> %a\n"
      (List.pp Usize.pp) ms
      (Option.pp Usize.pp) sum
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
  List.iter test_lists ~f:(fun ms ->
    test ms
  );
  printf "@]";

  [%expect{|
    reduce ~f:( + ) [] -> None
    reduce ~f:( + ) [0] -> Some 0
    reduce ~f:( + ) [0; 1] -> Some 1
    reduce ~f:( + ) [0; 1; 2] -> Some 3
    reduce ~f:( + ) [0; 1; 66] -> Some 67
    reduce ~f:( + ) [0; 1; 66; 91] -> Some 158
    reduce ~f:( + ) [42; 420] -> Some 462
    reduce ~f:( + ) [42; 420; 421] -> Some 883
    reduce ~f:( + ) [42; 420; 4200] -> Some 4662
    |}]

let%expect_test "stress" =
  let open Format in
  printf "@[";
  (* test is n^2 time complexity, so keep n small. *)
  let rec test n i e set = begin
    match i < n with
    | false -> set
    | true -> begin
        let set' = remove i (test n (succ i) e (insert i set)) in
        assert (equal set set');
        assert (equal set (union set set'));
        assert (equal set (inter set set'));
        assert (equal e (diff set set'));
        set'
      end
  end in
  let e = empty (module UsizeTestCmper) in
  let _ = test 100 0 e e in
  printf "@]";

  [%expect{|
    |}]
