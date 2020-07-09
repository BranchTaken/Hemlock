(* AVL tree implementation of ordered maps, based on the join operation.  The
 * join-based approach is both work-optimal and highly parallelizeable.
 *
 *   Just Join for Parallel Ordered Sets
 *   Guy E. Blelloch, Daniel Ferizovic, and Yihan Sun
 *   SPAA '16
 *   DOI: http://dx.doi.org/10.1145/2935764.2935768
 *   https://arxiv.org/pdf/1602.02120.pdf
 *   https://www.cs.ucr.edu/~yihans/papers/join.pdf
 *
 *   https://en.wikipedia.org/wiki/AVL_tree
 *   https://en.wikipedia.org/wiki/Join-based_tree_algorithms
 *
 * It is possible to implement AVL trees with as little as one bit of balance
 * metadata per node, but this implementation is less svelte:
 *
 * - Each node tracks the number of nodes in its subtree, in order to support
 *   O(lg n) indexed access.
 * - Each node tracks height rather than balance, in order to simplify the join
 *   logic.  Join heavily relies on node height, and although it is possible to
 *   efficiently compute heights based on imbalance metadata, doing so in join
 *   is complex and brittle.  Join is O(lg n), and the naïve approach of
 *   computing node height from scratch as needed would introduce O(n lg n)
 *   complexity to join.  The efficient alternative would be to compute the
 *   root's height, then incrementally compute node heights.  The complexity of
 *   this is that computed heights must be tightly coupled with their nodes, and
 *   the computations must take into account imbalance, recursive joins,
 *   rotations, etc. *)

open Rudiments

module T = struct
  type ('k, 'v) node =
    | Empty
    | Leaf of {
        (* Key. *)
        k: 'k;
        (* Value. *)
        v: 'v;
      }
    | Node of {
        (* Left subtree. *)
        l: ('k, 'v) node;
        (* Key. *)
        k: 'k;
        (* Value. *)
        v: 'v;
        (* Subtree node count, including this node. *)
        n: usize;
        (* Node height, inductively defined as (succ (max (height l) (height
         * r))), where an empty subtree has height 0, and a leaf has height
         * 1. *)
        h: usize;
        (* Right subtree. *)
        r: ('k, 'v) node;
      }
  type ('k, 'v, 'cmp) t = {
    (* Comparator. *)
    cmper: ('k, 'cmp) Cmper.t;
    (* Tree root. *)
    root: ('k, 'v) node;
  }
  type 'k key = 'k
  type 'v value = 'v

  type ('k, 'cmp) cmper =
    (module Cmper.S_mono with type t = 'k and type cmper_witness = 'cmp)

  let nnodes = function
    | Empty -> 0
    | Leaf _ -> 1
    | Node {l=_; k=_; v=_; n; h=_; r=_} -> n

  let height = function
    | Empty -> 0
    | Leaf _ -> 1
    | Node {l=_; k=_; v=_; n=_; h; r=_} -> h

  let leaf_init (k, v) =
    Leaf {k; v}

  let node_init l (k, v) r =
    match l, r with
    | Empty, Empty -> leaf_init (k, v)
    | _, _ -> begin
        let n = (nnodes l) + 1 + (nnodes r) in
        let h = succ (max (height l) (height r)) in
        Node {l; k; v; n; h; r}
      end

  let length t =
    nnodes t.root

  let is_empty t =
    (length t) = 0

  (* Path to node.  Incapable of expressing "no path".  Seek operations are
   * incremental, such that complete tree traversal is ϴ(n). *)
  module Path = struct
    type ('k, 'v) elm = {
      node: ('k, 'v) node;
      index: usize;
    }
    type ('k, 'v) t = ('k, 'v) elm list

    (* Record node's index in path, where base is the index of the leftmost node
     * in the subtree rooted at node.  Terminate when the path reaches the node
     * at index. *)
    let rec descend ~index base node path =
      assert ((index - base) < (nnodes node));
      match node with
      | Leaf _ ->
        assert (index = base);
        {node; index} :: path
      | Node {l=Empty; k=_; v=_; n=_; h=_; r} -> begin
          let node_index = base in
          match Usize.cmp index node_index with
          | Lt -> not_reached ()
          | Eq -> {node; index} :: path
          | Gt -> descend ~index (succ node_index) r
            ({node; index=node_index} :: path)
        end
      | Node {l; k=_; v=_; n=_; h=_; r=Empty} -> begin
          let node_index = base + (nnodes l) in
          match Usize.cmp index node_index with
          | Lt -> descend ~index base l ({node; index=node_index} :: path)
          | Eq -> {node; index} :: path
          | Gt -> not_reached ()
        end
      | Node {l; k=_; v=_; n=_; h=_; r} -> begin
          let node_index = base + (nnodes l) in
          match Usize.cmp index node_index with
          | Lt -> descend ~index base l ({node; index=node_index} :: path)
          | Eq -> {node; index} :: path
          | Gt -> descend ~index (succ node_index) r
            ({node; index=node_index} :: path)
        end
      | Empty -> not_reached ()

    let init index ordmap =
      descend ~index 0 ordmap.root []

    let index = function
      | [] -> not_reached ()
      | elm :: _ -> elm.index

    (* Ascend until the desired index is beneath node, then descend into the
     * appropriate subtree. *)
    let rec seek_nth index path =
      match path with
      | [] -> not_reached ()
      | elm :: tl -> begin
          match Usize.cmp index elm.index with
          | Lt -> begin
              match elm.node with
              | Empty
              | Leaf _ -> seek_nth index tl
              | Node {l; k=_; v=_; n=_; h=_; r=_} -> begin
                  let base = elm.index - (nnodes l) in
                  match index >= base with
                  | true -> descend ~index base l path
                  | false -> seek_nth index tl
                end
            end
          | Eq -> path
          | Gt -> begin
              match elm.node with
              | Empty
              | Leaf _ -> seek_nth index tl
              | Node {l=_; k=_; v=_; n=_; h=_; r} -> begin
                  let base = succ elm.index in
                  match index < base + (nnodes r) with
                  | true -> descend ~index base r path
                  | false -> seek_nth index tl
                end
            end
        end

    let seek_left offset t =
      seek_nth ((index t) - offset) t

    let seek_right offset t =
      seek_nth ((index t) + offset) t

    let succ t =
      seek_right 1 t

    let pred t =
      seek_left 1 t

    let kv = function
      | [] -> not_reached ()
      | elm :: _ -> begin
          match elm.node with
          | Leaf {k; v}
          | Node {l=_; k; v; n=_; h=_; r=_} -> k, v
          | Empty -> not_reached ()
        end

    let pp ppf t =
      let open Format in
      let pp_elm ppf elm = begin
        fprintf ppf "%a" Usize.pp elm.index
      end in
      fprintf ppf "@[<h>%a@]" (List.pp pp_elm) t
  end

  (* Path-based cursor.  If this were based on just index and calls to nth,
   * complete traversals would be Θ(n lg n) rather than Θ(n). *)
  module Cursor = struct
    module T = struct
      type ('k, 'v, 'cmp) container = ('k, 'v, 'cmp) t
      type ('k, 'v, 'cmp) t = {
        ordmap: ('k, 'v, 'cmp) container;
        index: usize;
        (* Separate paths to the nodes to the left and right of the cursor
         * efficiently handle edge conditions near the minimum/maximum nodes,
         * and they ensure that lget/rget are O(1). *)
        lpath_opt: ('k, 'v) Path.t option;
        rpath_opt: ('k, 'v) Path.t option;
      }

      let cmp t0 t1 =
        assert ((length t0.ordmap) = (length t1.ordmap));
        Usize.cmp t0.index t1.index

      let hd ordmap =
        let rpath_opt = match length ordmap with
          | 0 -> None
          | _ -> Some (Path.init 0 ordmap)
        in
        {ordmap; index=0; lpath_opt=None; rpath_opt}

      let tl ordmap =
        let index = length ordmap in
        let lpath_opt = match index with
          | 0 -> None
          | _ -> Some (Path.init (pred index) ordmap)
        in
        {ordmap; index; lpath_opt; rpath_opt=None}

      let seek i t =
        match Isize.cmp i (Isize.kv 0) with
        | Lt -> begin
            let u = (Usize.of_isize Isize.(neg i)) in
            match Usize.cmp t.index u with
            | Lt -> halt "Cannot seek before beginning of ordered map"
            | Eq -> begin
                {t with
                  index=0;
                  lpath_opt=None;
                  rpath_opt=Some (Path.seek_left (pred u)
                    (Option.value_hlt t.lpath_opt))
                }
              end
            | Gt -> begin
                let rpath' =
                  Path.seek_left (pred u) (Option.value_hlt t.lpath_opt) in
                let lpath' = Path.pred rpath' in
                {t with index=pred t.index; lpath_opt=Some lpath';
                        rpath_opt=Some rpath'}
              end
          end
        | Eq -> t
        | Gt -> begin
            let u = Usize.of_isize i in
            let index' = t.index + u in
            match Usize.cmp index' (length t.ordmap) with
            | Lt -> begin
                let lpath' =
                  Path.seek_right (pred u) (Option.value_hlt t.rpath_opt) in
                let rpath' = Path.succ lpath' in
                {t with index=index'; lpath_opt=Some lpath';
                        rpath_opt=Some rpath'}
              end
            | Eq -> begin
                {t with
                  index=index';
                  lpath_opt= Some (Path.seek_right (pred u)
                    (Option.value_hlt t.rpath_opt));
                  rpath_opt=None
                }
              end
            | Gt -> halt "Cannot seek past end of ordered map"
          end

      let succ t =
        seek Isize.one t

      let pred t =
        seek Isize.neg_one t

      let lget t =
        match t.lpath_opt with
        | None -> halt "Out of bounds"
        | Some lpath -> Path.kv lpath

      let rget t =
        match t.rpath_opt with
        | None -> halt "Out of bounds"
        | Some rpath -> Path.kv rpath

      let container t =
        t.ordmap

      let index t =
        t.index

      let pp ppf t =
        Format.fprintf ppf "@[<h>{index=%a;@ lpath_opt=%a;@ rpath_opt=%a}@]"
          Usize.pp t.index
          (Option.pp Path.pp) t.lpath_opt
          (Option.pp Path.pp) t.rpath_opt
    end
    include T
    include Cmpable.Make_poly3(T)
  end
end
include T
include Container_array.Make_poly3_array(T)

let fold_until ~init ~f t =
  let rec fn accum f = function
    | Empty -> accum, false
    | Leaf {k; v} -> f accum (k, v)
    | Node {l; k; v; n=_; h=_; r} -> begin
        let accum', until = fn accum f l in
        match until with
        | true -> accum', true
        | false -> begin
            let accum'', until = f accum' (k, v) in
            match until with
            | true -> accum'', true
            | false -> fn accum'' f r
          end
      end
  in
  let accum, _ = fn init f t.root in
  accum

let fold_right_until ~init ~f t =
  let rec fn accum f = function
    | Empty -> accum, false
    | Leaf {k; v} -> f (k, v) accum
    | Node {l; k; v; n=_; h=_; r} -> begin
        let accum', until = fn accum f r in
        match until with
        | true -> accum', true
        | false -> begin
            let accum'', until = f (k, v) accum' in
            match until with
            | true -> accum'', true
            | false -> fn accum'' f l
          end
      end
  in
  let accum, _ = fn init f t.root in
  accum

let foldi_until ~init ~f t =
  let _, accum = fold_until t ~init:(0, init)
    ~f:(fun (i, accum) (k, v) ->
      let i' = (Usize.succ i) in
      let accum', until = f i accum (k, v) in
      (i', accum'), until
    ) in
  accum

let fold ~init ~f t =
  fold_until t ~init ~f:(fun accum (k, v) -> (f accum (k, v)), false)

let fold_right ~init ~f t =
  fold_right_until t ~init ~f:(fun (k, v) accum -> (f (k, v) accum), false)

let foldi ~init ~f t =
  foldi_until t ~init ~f:(fun i accum (k, v) -> (f i accum (k, v)), false)

let iter ~f t =
  fold t ~init:() ~f:(fun _ (k, v) -> f (k, v))

let iteri ~f t =
  foldi t ~init:() ~f:(fun i _ (k, v) -> f i (k, v))

let count ~f t =
  fold t ~init:0 ~f:(fun accum (k, v) ->
    match f (k, v) with
    | false -> accum
    | true -> (Usize.succ accum)
  )

let for_any ~f t =
  fold_until t ~init:false ~f:(fun _ (k, v) ->
    let any' = f (k, v) in
    any', any'
  )

let for_all ~f t =
  fold_until t ~init:true ~f:(fun _ (k, v) ->
    let all' = f (k, v) in
    all', (not all')
  )

let find ~f t =
  fold_until t ~init:None ~f:(fun _ (k, v) ->
    match f (k, v) with
    | false -> None, false
    | true -> Some (k, v), true
  )

let find_map ~f t =
  fold_until t ~init:None ~f:(fun _ (k, v) ->
    match f (k, v) with
    | None -> None, false
    | Some a -> Some a, true
  )

let findi ~f t =
  foldi_until t ~init:None ~f:(fun i _ (k, v) ->
    match f i (k, v) with
    | false -> None, false
    | true -> Some (k, v), true
  )

let findi_map ~f t =
  foldi_until t ~init:None ~f:(fun i _ (k, v) ->
    match f i (k, v) with
    | None -> None, false
    | Some a -> Some a, true
  )

let min_elm ~cmp t =
  fold t ~init:None ~f:(fun accum (k, v) ->
    match accum with
    | None -> Some (k, v)
    | Some e -> begin
        match cmp e (k, v) with
        | Cmp.Lt -> Some e
        | Cmp.Eq
        | Cmp.Gt -> Some (k, v)
      end
  )

let max_elm ~cmp t =
  fold t ~init:None ~f:(fun accum (k, v) ->
    match accum with
    | None -> Some (k, v)
    | Some e -> begin
        match cmp e (k, v) with
        | Cmp.Lt
        | Cmp.Eq -> Some (k, v)
        | Cmp.Gt -> Some e
      end
  )

let hash_fold hash_fold_v t state =
  foldi t ~init:state ~f:(fun i state (k, v) ->
    state
    |> Usize.hash_fold i
    |> t.cmper.hash_fold k
    |> hash_fold_v v
  )
  |> Usize.hash_fold (length t)

let cmper_m (type k cmp) t : (k, cmp) cmper =
  (module struct
    type t = k
    type cmper_witness = cmp
    let cmper = t.cmper
  end)

(* Extract cmper from first-class module compatible with Cmper.S_mono . *)
let m_cmper (type k cmp) ((module M) : (k, cmp) cmper) =
  M.cmper

let cmper t =
  t.cmper

let empty m =
  {cmper=m_cmper m; root=Empty}

let singleton m ~k ~v =
  {cmper=m_cmper m; root=leaf_init (k, v)}

let rec get_node a cmper node =
  let open Cmper in
  match node with
  | Empty -> None
  | Leaf {k; v} -> begin
      match Cmp.is_eq (cmper.cmp a k) with
      | false -> None
      | true -> Some v
    end
  | Node {l; k; v; n=_; h=_; r} ->
    match cmper.cmp a k with
    | Lt -> get_node a cmper l
    | Eq -> Some v
    | Gt -> get_node a cmper r

let get a t =
  get_node a t.cmper t.root

let get_hlt a t =
  match get_node a t.cmper t.root with
  | Some v -> v
  | None -> halt "Key not found"

let mem a t =
  Option.is_some (get a t)

let choose t =
  let rec fn = function
    | Empty -> None
    | Leaf {k; v} -> Some (k, v)
    | Node {l; k; v; n=_; h=_; r=_} -> begin
        match l with
        | Empty -> Some (k, v)
        | Leaf _
        | Node _ -> fn l
      end
  in
  fn t.root

let choose_hlt t =
  match choose t with
  | Some (k, v) -> k, v
  | None -> halt "Empty map"

let nth_opt i t =
  let rec fn i = function
    | Empty -> None
    | Leaf {k; v} -> Some (k, v)
    | Node {l=Empty; k; v; n=_; h=_; r} -> begin
        match Usize.cmp i 0 with
        | Lt -> not_reached ()
        | Eq -> Some (k, v)
        | Gt -> fn (i - 1) r
      end
    | Node {l; k; v; n=_; h=_; r=Empty} -> begin
        let l_n = nnodes l in
        match Usize.cmp i l_n with
        | Lt -> fn i l
        | Eq -> Some (k, v)
        | Gt -> None
      end
    | Node {l; k; v; n=_; h=_; r} -> begin
        let l_n = nnodes l in
        match Usize.cmp i l_n with
        | Lt -> fn i l
        | Eq -> Some (k, v)
        | Gt -> fn (i - l_n - 1) r
      end
  in
  fn i t.root

let nth i t =
  match nth_opt i t with
  | None -> halt "Out of bounds"
  | Some (k, v) -> k, v

let search_impl a cmper mode node =
  let open Cmper in
  let open Cmp in
  let lt_empty mode base index = begin
    match mode with
    | Lt -> begin
        match base with
        | 0 -> Some (Lt, 0)
        | _ -> Some (Gt, pred index)
      end
    | Eq -> None
    | Gt -> Some (Lt, index)
  end in
  let gt_empty mode past index = begin
    match mode with
    | Lt -> Some (Gt, index)
    | Eq -> None
    | Gt -> begin
        match past - index with
        | 1 -> Some (Gt, index)
        | _ -> Some (Lt, succ index)
      end
  end in
  let rec fn a cmper mode ~base ~past = function
    | Empty -> None
    | Leaf {k; v=_} -> begin
        let index = base in
        match cmper.cmp a k with
        | Lt -> lt_empty mode base index
        | Eq -> Some (Eq, index)
        | Gt -> gt_empty mode past index
      end
    | Node {l; k; v=_; n=_; h=_; r} -> begin
        let index = match l with
          | Empty -> base
          | Leaf _ -> base + 1
          | Node {l=_; k=_; v=_; n; h=_; r=_} -> base + n
        in
        match cmper.cmp a k with
        | Lt -> begin
            match l with
            | Empty -> lt_empty mode base index
            | _ -> fn a cmper mode ~base ~past l
          end
        | Eq -> Some (Eq, index)
        | Gt -> begin
            match r with
            | Empty -> gt_empty mode past index
            | _ -> fn a cmper mode ~base:(succ index) ~past r
          end
      end
  in
  fn a cmper mode ~base:0 ~past:(nnodes node) node

let psearch a t =
  search_impl a t.cmper Cmp.Lt t.root

let search a t =
  match search_impl a t.cmper Cmp.Eq t.root with
  | Some (_, i) -> Some i
  | _ -> None

let nsearch a t =
  search_impl a t.cmper Cmp.Gt t.root

(* Seq. *)
module Seq_poly3_fold2 = struct
  type ('k, 'v, 'cmp) container = ('k, 'v, 'cmp) t
  type 'k key = 'k
  type 'v value = 'v
  type ('k, 'v, 'cmp) t = {
    cursor_opt: ('k, 'v, 'cmp) Cursor.t option;
  }

  let init ordmap =
    match length ordmap with
    | 0 -> {cursor_opt=None}
    | _ -> {cursor_opt=Some (Cursor.hd ordmap)}

  let index t =
    match t.cursor_opt with
    | None -> 0
    | Some cursor -> Cursor.index cursor

  let length t =
    match t.cursor_opt with
    | None -> 0
    | Some cursor -> length (Cursor.container cursor)

  let next t =
    assert (index t < length t);
    let cursor = Option.value_hlt t.cursor_opt in
    let (k, v) = Cursor.rget cursor in
    let cursor' = Cursor.succ cursor in
    (k, v), {cursor_opt=Some cursor'}

  let next_opt t =
    match (index t) < (length t) with
    | false -> None
    | true -> Some (next t)

  let cmper = cmper

  let cmp cmper k0 k1 =
    let open Cmper in
    cmper.cmp k0 k1
end
include Seq.Make_poly3_fold2(Seq_poly3_fold2)
module Seq = Seq_poly3_fold2

let cmp vcmp t0 t1 =
  let open Cmp in
  fold2_until ~init:Eq ~f:(fun _ kv0_opt kv1_opt ->
    match kv0_opt, kv1_opt with
    | Some (_, v0), Some (_, v1) -> begin
        match vcmp v0 v1 with
        | Lt -> Lt, true
        | Eq -> Eq, false
        | Gt -> Gt, true
      end
    | Some _, None -> Lt, true
    | None, Some _ -> Gt, true
    | None, None -> not_reached ()
  ) t0 t1

let equal veq t0 t1 =
  (* Equal AVL trees may have different internal structure.  fold2_until is
   * structure-agnostic, so the following produces correct results even when
   * coupled recursive traversal would fail. *)
  fold2_until ~init:true ~f:(fun _ kv0_opt kv1_opt ->
    match kv0_opt, kv1_opt with
    | Some (_, v0), Some (_, v1) -> begin
        let eq = veq v0 v1 in
        eq, (not eq)
      end
    | Some _, None -> false, true
    | None, Some _ -> false, true
    | None, None -> not_reached ()
  ) t0 t1

let expose = function
  | Leaf {k; v} -> Empty, (k, v), Empty
  | Node {l; k; v; n=_; h=_; r} -> l, (k, v), r
  | Empty -> not_reached ()

(* Join AVL trees (l, (singleton m ~k ~v), r) to form an AVL tree representing
 * their union, where l and r are AVL trees containing mappings strictly
 * preceding/following k in the total key ordering, respectively. *)
let join l kv r =
  let rec join_left_tall l kv r = begin
    let ll, l_kv, lr = expose l in
    match (height lr) <= (height r) + 1 with
    | true -> begin
        match (max (height lr) (height r)) <= (height ll) with
        | true -> node_init ll l_kv (node_init lr kv r)
        | false -> begin
            let n0, lr_kv, n1 = expose lr in
            node_init (node_init ll l_kv n0) lr_kv (node_init n1 kv r)
          end
      end
    | false -> begin
        let lr' = join_left_tall lr kv r in
        match (height lr') <= (height ll) + 1 with
        | true -> node_init ll l_kv lr'
        | false -> begin
            let n0, lr_kv', n1 = expose lr' in
            node_init (node_init ll l_kv n0) lr_kv' n1
          end
      end
  end in
  let rec join_right_tall l kv r = begin
    let rl, r_kv, rr = expose r in
    match (height rl) <= (height l) + 1 with
    | true -> begin
        match (max (height l) (height rl)) <= (height rr) with
        | true -> node_init (node_init l kv rl) r_kv rr
        | false -> begin
            let n0, rl_kv, n1 = expose rl in
            node_init (node_init l kv n0) rl_kv (node_init n1 r_kv rr)
          end
      end
    | false -> begin
        let rl' = join_right_tall l kv rl in
        match (height rl') <= (height rr) + 1 with
        | true -> node_init rl' r_kv rr
        | false -> begin
            let n0, rl_kv', n1 = expose rl' in
            node_init n0 rl_kv' (node_init n1 r_kv rr)
          end
      end
  end in
  let lh = height l in
  let rh = height r in
  match (lh > rh + 1), (lh + 1 < rh) with
  | true, _ -> join_left_tall l kv r
  | _, true -> join_right_tall l kv r
  | false, false -> node_init l kv r

(* Join AVL trees (l, r) to form an AVL tree representing their union, where l's
 * keys strictly precede r's keys in the total key ordering. *)
let join2 l r =
  let rec split_rightmost = function
    | Leaf {k; v} -> Empty, (k, v)
    | Node {l; k; v; n=_; h=_; r=Empty} -> l, (k, v)
    | Node {l; k; v; n=_; h=_; r} -> begin
        let r', (k', v') = split_rightmost r in
        (join l (k, v) r'), (k', v')
      end
    | Empty -> not_reached ()
  in
  match l, r with
  | Empty, _ -> r
  | _, Empty -> l
  | _, _ -> begin
      let l', (k, v) = split_rightmost l in
      join l' (k, v) r
    end

(* Split an AVL tree into (l, Some (a, v), r) if key a is in the tree, (l, None,
 * r) otherwise, where l and r are AVL trees containing all mappings
 * preceding/following a in the total key ordering, respectively.  split_node is
 * join's dual. *)
let rec split_node a cmper node =
  let open Cmper in
  match node with
  | Empty -> Empty, None, Empty
  | Leaf {k; v} -> begin
      match cmper.cmp a k with
      | Lt -> Empty, None, node
      | Eq -> Empty, (Some (k, v)), Empty
      | Gt -> node, None, Empty
    end
  | Node {l; k; v; n=_; h=_; r} -> begin
      match cmper.cmp a k with
      | Lt -> begin
          let ll, kv_opt, lr = split_node a cmper l in
          match ll, kv_opt with
          | Empty, None -> Empty, None, node
          | _, _ -> ll, kv_opt, (join lr (k, v) r)
        end
      | Eq -> l, (Some (k, v)), r
      | Gt -> begin
          let rl, kv_opt, rr = split_node a cmper r in
          match kv_opt, rr with
          | None, Empty -> node, None, Empty
          | _, _ -> (join l (k, v) rl), kv_opt, rr
        end
    end

(* Split an AVL tree into (Some (l, r)) if key a is not in the tree, None
 * otherwise, where l and r are AVL trees containing all mappings
 * preceding/following a in the total key ordering, respectively.  split2_node
 * is join2's dual. *)
let rec split2_node a cmper node =
  let open Cmper in
  match node with
  | Empty -> Some (Empty, Empty)
  | Leaf {k; v=_} -> begin
      match cmper.cmp a k with
      | Lt -> Some (Empty, node)
      | Eq -> None
      | Gt -> Some (node, Empty)
    end
  | Node {l; k; v; n=_; h=_; r} -> begin
      match cmper.cmp a k with
      | Lt -> begin
          match split2_node a cmper l with
          | None -> None
          | Some (ll, lr) -> begin
              match ll with
              | Empty -> Some (Empty, node)
              | _ -> Some (ll, (join lr (k, v) r))
            end
        end
      | Eq -> None
      | Gt -> begin
          match split2_node a cmper r with
          | None -> None
          | Some (rl, rr) -> begin
              match rr with
              | Empty -> Some (node, Empty)
              | _ -> Some ((join l (k, v) rl), rr)
            end
        end
    end

let subset veq t0 t1 =
  let rec fn cmper veq node0 node1 = begin
    match node1 with
    | Empty -> true
    | Leaf {k; v} -> begin
        let _, kv_of_opt, _ = split_node k cmper node0 in
        match kv_of_opt with
        | None -> false
        | Some (_, v0) -> veq v0 v
      end
    | Node {l; k; v; n=_; h=_; r} -> begin
        let l0, kv_of_opt, r0 = split_node k cmper node0 in
        match kv_of_opt with
        | None -> false
        | Some (_, v0) ->
          (veq v0 v) && (fn cmper veq l0 l) && (fn cmper veq r0 r)
      end
  end in
  fn t0.cmper veq t0.root t1.root

let disjoint t0 t1 =
  let rec fn cmper node0 node1 = begin
    match node0, node1 with
    | _, Empty
    | Empty, _ -> true
    | Leaf {k; v=_}, _ -> Option.is_none (get_node k cmper node1)
    | Node {l; k; v=_; n=_; h=_; r}, _ -> begin
        match split2_node k cmper node1 with
        | None -> false
        | Some (l1, r1) -> (fn cmper l l1) && (fn cmper r r1)
      end
  end in
  fn t0.cmper t0.root t1.root

let insert_node cmper ~k ~v node =
  let open Cmper in
  let open Cmp in
  let rec fn cmper (k_in, v_in) node = begin
    match node with
    | Empty -> Some (leaf_init (k, v))
    | Leaf {k; v=_} -> begin
        match cmper.cmp k_in k with
        | Lt -> Some (node_init Empty (k_in, v_in) node)
        | Eq -> None
        | Gt -> Some (node_init node (k_in, v_in) Empty)
      end
    | Node {l; k; v; n=_; h=_; r} -> begin
        match cmper.cmp k_in k with
        | Lt -> begin
            match fn cmper (k_in, v_in) l with
            | None -> None
            | Some l' -> Some (join l' (k, v) r)
          end
        | Eq -> None
        | Gt -> begin
            match fn cmper (k_in, v_in) r with
            | None -> None
            | Some r' -> Some (join l (k, v) r')
          end
      end
  end in
  fn cmper (k, v) node

let insert ~k ~v t =
  match insert_node t.cmper ~k ~v t.root with
  | None -> t
  | Some root -> {t with root}

let insert_hlt ~k ~v t =
  match insert_node t.cmper ~k ~v t.root with
  | None -> halt "Key already bound in map"
  | Some root -> {t with root}

let upsert ~k ~v t =
  let open Cmper in
  let open Cmp in
  let rec fn cmper (k_in, v_in) node = begin
    match node with
    | Empty -> leaf_init (k_in, v_in)
    | Leaf {k; v=_} -> begin
        match cmper.cmp k_in k with
        | Lt -> node_init Empty (k_in, v_in) node
        | Eq -> leaf_init (k_in, v_in)
        | Gt -> node_init node (k_in, v_in) Empty
      end
    | Node {l; k; v; n=_; h=_; r} -> begin
        match cmper.cmp k_in k with
        | Lt -> join (fn cmper (k_in, v_in) l) (k, v) r
        | Eq -> node_init l (k_in, v_in) r
        | Gt -> join l (k, v) (fn cmper (k_in, v_in) r)
      end
  end in
  {t with root=fn t.cmper (k, v) t.root}

let update_node cmper ~k ~v node =
  let open Cmper in
  let open Cmp in
  let rec fn cmper (k_in, v_in) = function
    | Empty -> None
    | Leaf {k; v=_} -> begin
        match cmper.cmp k_in k with
        | Lt -> None
        | Eq -> Some (leaf_init (k_in, v_in))
        | Gt -> None
      end
    | Node {l; k; v; n=_; h=_; r} -> begin
        match cmper.cmp k_in k with
        | Lt -> begin
            match fn cmper (k_in, v_in) l with
            | None -> None
            | Some l' -> Some (join l' (k, v) r)
          end
        | Eq -> Some (node_init l (k_in, v_in) r)
        | Gt -> begin
            match fn cmper (k_in, v_in) r with
            | None -> None
            | Some r' -> Some (join l (k, v) r')
          end
      end
  in
  fn cmper (k, v) node

let update ~k ~v t =
  match update_node t.cmper ~k ~v t.root with
  | Some root -> {t with root}
  | None -> t

let update_hlt ~k ~v t =
  match update_node t.cmper ~k ~v t.root with
  | Some root -> {t with root}
  | None -> halt "Key not bound in map"

let remove_node cmper k node =
  let open Cmper in
  let open Cmp in
  let rec fn cmper k_in = function
    | Empty -> None
    | Leaf {k; v=_} -> begin
        match cmper.cmp k_in k with
        | Lt -> None
        | Eq -> Some Empty
        | Gt -> None
      end
    | Node {l; k; v; n=_; h=_; r} -> begin
        match cmper.cmp k_in k with
        | Lt -> begin
            match fn cmper k_in l with
            | None -> None
            | Some l' -> Some (join l' (k, v) r)
          end
        | Eq -> Some (join2 l r)
        | Gt -> begin
            match fn cmper k_in r with
            | None -> None
            | Some r' -> Some (join l (k, v) r')
          end
      end
  in
  fn cmper k node

let remove k t =
  match remove_node t.cmper k t.root with
  | Some root -> {t with root}
  | None -> t

let remove_hlt k t =
  match remove_node t.cmper k t.root with
  | Some root -> {t with root}
  | None -> halt "Key not bound in map"

let amend k ~f t =
  let open Cmper in
  let open Cmp in
  let rec fn cmper k_in ~f node = begin
    match node with
    | Empty -> begin
        match f None with
        | None -> None
        | Some v -> Some (leaf_init (k_in, v))
      end
    | Leaf {k; v} -> begin
        match cmper.cmp k_in k with
        | Lt -> begin
            match f None with
            | None -> None
            | Some v' -> Some (node_init Empty (k_in, v') node)
          end
        | Eq -> begin
            match f (Some v) with
            | None -> Some Empty
            | Some v' -> Some (leaf_init (k_in, v'))
          end
        | Gt -> begin
            match f None with
            | None -> None
            | Some v' -> Some (node_init node (k_in, v') Empty)
          end
      end
    | Node {l; k; v; n=_; h=_; r} -> begin
        match cmper.cmp k_in k with
        | Lt -> begin
            match fn cmper k_in ~f l with
            | None -> None
            | Some l' -> Some (join l' (k, v) r)
          end
        | Eq -> begin
            match f (Some v) with
            | None -> Some (join2 l r)
            | Some v' -> Some (node_init l (k_in, v') r)
          end
        | Gt -> begin
            match fn cmper k_in ~f r with
            | None -> None
            | Some r' -> Some (join l (k, v) r')
          end
      end
  end in
  match fn t.cmper k ~f t.root with
  | Some root -> {t with root}
  | None -> t

let of_alist m kvs =
  match kvs with
  | [] -> empty m
  | (k, v) :: kvs' -> begin
      let rec fn kvs ordmap = begin
        match kvs with
        | [] -> ordmap
        | (k, v) :: kvs' -> fn kvs' (insert_hlt ~k ~v ordmap)
      end in
      fn kvs' (singleton m ~k ~v)
    end

let to_alist t =
  fold_right ~init:[] ~f:(fun kv accum -> kv :: accum) t

let to_alist_rev t =
  fold ~init:[] ~f:(fun accum kv -> kv :: accum) t

let split a t =
  let l, a_opt, r = split_node a t.cmper t.root in
  {t with root=l}, a_opt, {t with root=r}

let union ~f t0 t1 =
  let rec fn cmper node0 node1 = begin
    match node0, node1 with
    | _, Empty -> node0
    | Empty, _ -> node1
    | Leaf {k; v}, _ -> begin
        let l1, kv1_opt, r1 = split_node k cmper node1 in
        let v' = match kv1_opt with
          | None -> v
          | Some (_, v1) -> f k v v1
        in
        join l1 (k, v') r1
      end
    | Node {l; k; v; n=_; h=_; r}, _ -> begin
        let l1, kv1_opt, r1 = split_node k cmper node1 in
        let v' = match kv1_opt with
          | None -> v
          | Some (_, v1) -> f k v v1
        in
        let l' = fn cmper l l1 in
        let r' = fn cmper r r1 in
        join l' (k, v') r'
      end
  end in
  let root' = fn t0.cmper t0.root t1.root in
  {t0 with root=root'}

let of_array m arr =
  match arr with
  | [||] -> empty m
  | _ -> Array.reduce_hlt (Array.map arr ~f:(fun (k, v) -> singleton m ~k ~v))
    ~f:(fun ordmap0 ordmap1 -> union ~f:(fun _ _ ->
      halt "Duplicate key"
    ) ordmap0 ordmap1)

let inter ~f t0 t1 =
  let rec fn cmper node0 node1 = begin
    match node0, node1 with
    | _, Empty
    | Empty, _ -> Empty
    | Leaf {k; v}, _ -> begin
        let _, kv1_opt, _ = split_node k cmper node1 in
        match kv1_opt with
        | Some (_, v1) -> leaf_init (k, (f k v v1))
        | None -> Empty
      end
    | Node {l; k; v; n=_; h=_; r}, _ -> begin
        let l1, kv1_opt, r1 = split_node k cmper node1 in
        let l' = fn cmper l l1 in
        let r' = fn cmper r r1 in
        match kv1_opt with
        | Some (_, v1) -> begin
            let v' = f k v v1 in
            join l' (k, v') r'
          end
        | None -> join2 l' r'
      end
  end in
  let root' = fn t0.cmper t0.root t1.root in
  {t0 with root=root'}

let diff t0 t1 =
  let rec fn cmper node0 node1 = begin
    match node0, node1 with
    | _, Empty -> node0
    | Empty, _ -> Empty
    | _, Leaf {k; v=_} -> begin
        let l0, _, r0 = split_node k cmper node0 in
        join2 l0 r0
      end
    | _, Node {r; k; v=_; n=_; h=_; l} -> begin
        let l0, _, r0 = split_node k cmper node0 in
        let l' = fn cmper l0 l in
        let r' = fn cmper r0 r in
        join2 l' r'
      end
  end in
  let root' = fn t0.cmper t0.root t1.root in
  {t0 with root=root'}

let filter ~f t =
  let rec fn ~f node = begin
    match node with
    | Empty -> Empty
    | Leaf {k; v} -> begin
        match f (k, v) with
        | true -> node
        | false -> Empty
      end
    | Node {l; k; v; n=_; h=_; r} -> begin
        let l' = fn ~f l in
        let r' = fn ~f r in
        match f (k, v) with
        | true -> join l' (k, v) r'
        | false -> join2 l' r'
      end
  end in
  {t with root=fn ~f t.root}

let filter_map ~f t =
  let rec fn ~f = function
    | Empty -> Empty
    | Leaf {k; v} -> begin
        match f (k, v) with
        | Some v2 -> leaf_init (k, v2)
        | None -> Empty
      end
    | Node {l; k; v; n=_; h=_; r} -> begin
        let l' = fn ~f l in
        let r' = fn ~f r in
        match f (k, v) with
        | Some v2 -> join l' (k, v2) r'
        | None -> join2 l' r'
      end
  in
  {t with root=fn ~f t.root}

let filteri ~f t =
  let rec fn ~f base node = begin
    match node with
    | Empty -> Empty
    | Leaf {k; v} -> begin
        match f base (k, v) with
        | true -> node
        | false -> Empty
      end
    | Node {l; k; v; n=_; h=_; r} -> begin
        let index = base + (nnodes l) in
        let l' = fn ~f base l in
        let r' = fn ~f (succ index) r in
        match f index (k, v) with
        | true -> join l' (k, v) r'
        | false -> join2 l' r'
      end
  end in
  {t with root=fn ~f 0 t.root}

let filteri_map ~f t =
  let rec fn ~f base = function
    | Empty -> Empty
    | Leaf {k; v} -> begin
        match f base (k, v) with
        | Some v2 -> leaf_init (k, v2)
        | None -> Empty
      end
    | Node {l; k; v; n=_; h=_; r} -> begin
        let index = base + (nnodes l) in
        let l' = fn ~f base l in
        let r' = fn ~f (succ index) r in
        match f index (k, v) with
        | Some v2 -> join l' (k, v2) r'
        | None -> join2 l' r'
      end
  in
  {t with root=fn ~f 0 t.root}

let partition_tf ~f t =
  let rec fn ~f node = begin
    match node with
    | Empty -> Empty, Empty
    | Leaf {k; v} -> begin
        match f (k, v) with
        | true -> node, Empty
        | false -> Empty, node
      end
    | Node {l; k; v; n=_; h=_; r} -> begin
        let t_l', f_l' = fn ~f l in
        let t_r', f_r' = fn ~f r in
        match f (k, v) with
        | true -> (join t_l' (k, v) t_r'), (join2 f_l' f_r')
        | false -> (join2 t_l' t_r'), (join f_l' (k, v) f_r')
      end
  end in
  let t_root, f_root = fn ~f t.root in
  {t with root=t_root}, {t with root=f_root}

let partitioni_tf ~f t =
  let rec fn ~f base node = begin
    match node with
    | Empty -> Empty, Empty
    | Leaf {k; v} -> begin
        match f base (k, v) with
        | true -> node, Empty
        | false -> Empty, node
      end
    | Node {l; k; v; n=_; h=_; r} -> begin
        let index = base + (nnodes l) in
        let t_l', f_l' = fn ~f base l in
        let t_r', f_r' = fn ~f (succ index) r in
        match f index (k, v) with
        | true -> (join t_l' (k, v) t_r'), (join2 f_l' f_r')
        | false -> (join2 t_l' t_r'), (join f_l' (k, v) f_r')
      end
  end in
  let t_root, f_root = fn ~f 0 t.root in
  {t with root=t_root}, {t with root=f_root}

let partition_map ~f t =
  let open Either in
  let rec fn ~f = function
    | Empty -> Empty, Empty
    | Leaf {k; v} -> begin
        match f (k, v) with
        | First v2 -> (leaf_init (k, v2)), Empty
        | Second v3 -> Empty, (leaf_init (k, v3))
      end
    | Node {l; k; v; n=_; h=_; r} -> begin
        let a_l', b_l' = fn ~f l in
        let a_r', b_r' = fn ~f r in
        match f (k, v) with
        | First v2 -> (join a_l' (k, v2) a_r'), (join2 b_l' b_r')
        | Second v3 -> (join2 a_l' a_r'), (join b_l' (k, v3) b_r')
      end
  in
  let a_root, b_root = fn ~f t.root in
  {t with root=a_root}, {t with root=b_root}

let partitioni_map ~f t =
  let open Either in
  let rec fn ~f base = function
    | Empty -> Empty, Empty
    | Leaf {k; v} -> begin
        match f base (k, v) with
        | First v2 -> (leaf_init (k, v2)), Empty
        | Second v3 -> Empty, (leaf_init (k, v3))
      end
    | Node {l; k; v; n=_; h=_; r} -> begin
        let index = base + (nnodes l) in
        let a_l', b_l' = fn ~f base l in
        let a_r', b_r' = fn ~f (succ index) r in
        match f index (k, v) with
        | First v2 -> (join a_l' (k, v2) a_r'), (join2 b_l' b_r')
        | Second v3 -> (join2 a_l' a_r'), (join b_l' (k, v3) b_r')
      end
  in
  let a_root, b_root = fn ~f 0 t.root in
  {t with root=a_root}, {t with root=b_root}

let kreduce ~f t =
  let rec fn ~reduce2 = function
    | Leaf {k; v=_} -> k
    | Node {l; k; v=_; n=_; h=_; r=Empty} -> reduce2 (fn ~reduce2 l) k
    | Node {l=Empty; k; v=_; n=_; h=_; r} -> reduce2 (fn ~reduce2 r) k
    | Node {l; k; v=_; n=_; h=_; r} ->
      reduce2 k (reduce2 (fn ~reduce2 l) (fn ~reduce2 r))
    | Empty -> not_reached ()
  in
  match t.root with
  | Empty -> None
  | _ -> Some (fn ~reduce2:f t.root)

let kreduce_hlt ~f t =
  match kreduce ~f t with
  | None -> halt "Empty map"
  | Some k -> k

let reduce ~f t =
  let rec fn ~reduce2 = function
    | Leaf {k=_; v} -> v
    | Node {l; k=_; v; n=_; h=_; r=Empty} -> reduce2 (fn ~reduce2 l) v
    | Node {l=Empty; k=_; v; n=_; h=_; r} -> reduce2 (fn ~reduce2 r) v
    | Node {l; k=_; v; n=_; h=_; r} ->
      reduce2 v (reduce2 (fn ~reduce2 l) (fn ~reduce2 r))
    | Empty -> not_reached ()
  in
  match t.root with
  | Empty -> None
  | _ -> Some (fn ~reduce2:f t.root)

let reduce_hlt ~f t =
  match reduce ~f t with
  | None -> halt "Empty map"
  | Some v -> v

(******************************************************************************)
(* Begin tests. *)

let pp pp_v ppf t =
  let open Format in
  let rec pp_node ppf = function
    | Empty -> fprintf ppf "Empty"
    | Leaf {k; v} -> fprintf ppf "@[<h>Leaf {k=%a;@ v=%a}@]" t.cmper.pp k pp_v v
    | Node {l; k; v; n; h; r} -> fprintf ppf
        ("@;<0 2>@[<v>Node {@;<0 2>@[<v>l=%a;@,k=%a;@,v=%a;@,n=%a;@,h=%a;@," ^^
            "r=%a@]@,}@]")
        pp_node l
        t.cmper.pp k
        pp_v v
        Usize.pp n
        Usize.pp h
        pp_node r
  in
  fprintf ppf "@[<v>Ordmap {@;<0 2>@[<v>root=%a@]@,}@]"
    pp_node t.root

let pp_kv pp_v ppf (k, v) =
  Format.fprintf ppf "(%a, %a)" Usize.pp k pp_v v

let validate t =
  let rec fn = function
    | Empty -> ()
    | Leaf _ -> ()
    | Node {l; k; v=_; n; h; r} -> begin
        fn l;
        fn r;
        let () = match l with
          | Empty -> ()
          | _ -> begin
              let _, (l_k, _), _ = expose l in
              assert (Cmp.is_lt (t.cmper.cmp l_k k));
            end
        in
        let () = match r with
          | Empty -> ()
          | _ -> begin
              let _, (r_k, _), _ = expose r in
              assert (Cmp.is_lt (t.cmper.cmp k r_k));
            end
        in
        assert (n = (nnodes l) + 1 + (nnodes r));
        let lh = height l in
        let rh = height r in
        assert (h = succ (max lh rh));
        assert ((max lh rh) - (min lh rh) < 2);
      end
  in
  fn t.root

let of_klist ks =
  List.fold ks ~init:(empty (module Usize)) ~f:(fun ordmap k ->
    insert_hlt ~k ~v:(k * 100) ordmap
  )

let of_karray ks =
  Array.fold ks ~init:(empty (module Usize)) ~f:(fun ordmap k ->
    insert_hlt ~k ~v:(k * 100) ordmap
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
        let ordmap = of_klist l in
        printf "hash_fold (of_klist %a) -> %a@\n"
          (List.pp Usize.pp) l
          Hash.pp (Hash.t_of_state
            (hash_fold Usize.hash_fold ordmap Hash.State.empty));
        fn lists'
      end
  in
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
    hash_fold (of_klist [0]) -> 0xf931_3f2a_e691_89fb_c121_c10c_2321_2ab7u128
    hash_fold (of_klist [0; 1]) -> 0x6e2d_e492_9376_8d4d_6ed4_d4a1_826a_e0f3u128
    hash_fold (of_klist [0; 2]) -> 0xd095_5072_d648_e202_4c57_11bf_5e6f_9d1bu128
    hash_fold (of_klist [2; 3]) -> 0xe261_acf8_6d66_7835_4c5e_2250_ae24_ada9u128
    |}]

let%expect_test "hash_fold empty" =
  let hash_empty state = begin
    state
    |> hash_fold Unit.hash_fold (empty (module Usize))
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
  let e = empty (module Usize) in
  validate e;
  assert (length e = 0);
  printf "%a@\n" (pp Unit.pp) e;

  let s = singleton (cmper_m e) ~k:0 ~v:"zero" in
  validate s;
  assert (length s = 1);
  printf "%a@\n" (pp String.pp) s;
  printf "@]";

  [%expect{|
    Ordmap {
      root=Empty
    }
    Ordmap {
      root=Leaf {k=0; v="zero"}
    }
    |}]

let%expect_test "mem,get,insert,subset" =
  let rec test ks ordmap = begin
    match ks with
    | [] -> ()
    | k :: ks' -> begin
        assert (not (mem k ordmap));
        assert (Option.is_none (get k ordmap));
        let v = k * 100 in
        let ordmap' = insert ~k ~v ordmap in
        validate ordmap';
        assert (mem k ordmap');
        assert ((get_hlt k ordmap') = v);
        assert (subset veq ordmap' ordmap);
        assert (not (subset veq ordmap ordmap'));
        test ks' ordmap'
      end
  end in
  let ks = [1; 3; 2; 44; 45; 56; 60; 66; 75; 81; 91] in
  test ks (empty (module Usize));

  [%expect{|
    |}]

let%expect_test "mem,get,insert,insert_hlt" =
  let rec test ks ordmap = begin
    match ks with
    | [] -> ()
    | k :: ks' -> begin
        assert (not (mem k ordmap));
        assert (Option.is_none (get k ordmap));
        let v = k * 100 in
        let ordmap' = insert_hlt ~k ~v ordmap in
        assert (mem k ordmap');
        assert ((get_hlt k ordmap') = v);
        validate ordmap';
        let v' = k * 10000 in
        let ordmap'' = insert ~k ~v:v' ordmap' in
        assert (mem k ordmap'');
        assert ((get_hlt k ordmap'') = v);
        validate ordmap'';
        test ks' ordmap''
      end
  end in
  let ks = [1; 3; 2; 44; 45; 56; 60; 66; 75; 81; 91] in
  test ks (empty (module Usize));

  [%expect{|
    |}]

let%expect_test "mem,get,update,upsert,update_hlt,subset" =
  let rec test ks ordmap = begin
    match ks with
    | [] -> ()
    | k :: ks' -> begin
        assert (not (mem k ordmap));
        assert (Option.is_none (get k ordmap));
        (* update (silently fail) *)
        let v = k * 100 in
        let ordmap' = update ~k ~v ordmap in
        assert (not (mem k ordmap'));
        validate ordmap';
        (* upsert *)
        let ordmap'' = upsert ~k ~v ordmap' in
        assert (mem k ordmap'');
        assert ((get_hlt k ordmap'') = v);
        validate ordmap'';
        (* update_hlt *)
        let v' = k * 10000 in
        let ordmap''' = update_hlt ~k ~v:v' ordmap'' in
        assert (mem k ordmap''');
        assert ((get_hlt k ordmap''') = v');
        assert (not (subset veq ordmap'' ordmap'''));
        assert (not (subset veq ordmap''' ordmap''));
        validate ordmap''';
        (* update *)
        let v'' = k * 1000000 in
        let ordmap'''' = update ~k ~v:v'' ordmap''' in
        assert (mem k ordmap'''');
        assert ((get_hlt k ordmap'''') = v'');
        validate ordmap'''';
        test ks' ordmap''''
      end
  end in
  let ks = [1; 3; 2; 44; 45; 56; 60; 66; 75; 81; 91] in
  test ks (empty (module Usize));

  [%expect{|
    |}]

let%expect_test "mem,get,amend" =
  let rec test ks ordmap = begin
    match ks with
    | [] -> ()
    | k :: ks' -> begin
        assert (not (mem k ordmap));
        assert (Option.is_none (get k ordmap));
        let v = k * 100 in
        let ordmap' = amend k ~f:(function
          | None -> Some v
          | Some _ -> not_reached ()
        ) ordmap in
        assert (mem k ordmap');
        assert ((get_hlt k ordmap') = v);
        validate ordmap';
        let v' = k * 10000 in
        let ordmap'' = amend k ~f:(function
          | Some vx -> begin
              assert (vx = v);
              Some v'
            end
          | None -> not_reached ()
        ) ordmap' in
        assert (mem k ordmap'');
        assert ((get_hlt k ordmap'') = v');
        validate ordmap'';
        test ks' ordmap''
      end
  end in
  let ks = [1; 3; 2; 44; 45; 56; 60; 66; 75; 81; 91] in
  test ks (empty (module Usize));

  [%expect{|
    |}]

let%expect_test "of_alist,remove" =
  let open Format in
  printf "@[";
  let test k ordmap descr = begin
    validate ordmap;
    printf "--- %s ---@\n" descr;
    let ordmap' = remove k ordmap in
    validate ordmap';
    printf "@[<v>remove %a@;<0 2>@[<v>%a ->@,%a@]@]@\n"
      Usize.pp k (pp String.pp) ordmap (pp String.pp) ordmap'
  end in
  let test_tuples = [
    ([(0, "0"); (1, "1")], 2,           "Not member.");
    ([(0, "0")], 0,                     "Member, length 1 -> 0.");
    ([(0, "0"); (1, "1")], 1,           "Member, length 2 -> 1.");
    ([(0, "0"); (1, "1"); (2, "2")], 2, "Member, length 3 -> 2.");
  ] in
  List.iter test_tuples ~f:(fun (kvs, k, descr) ->
    let ordmap = of_alist (module Usize) kvs in
    test k ordmap descr
  );
  printf "@]";

  [%expect{|
    --- Not member. ---
    remove 2
      Ordmap {
        root=
          Node {
            l=Leaf {k=0; v="0"};
            k=1;
            v="1";
            n=2;
            h=2;
            r=Empty
          }
      } ->
      Ordmap {
        root=
          Node {
            l=Leaf {k=0; v="0"};
            k=1;
            v="1";
            n=2;
            h=2;
            r=Empty
          }
      }
    --- Member, length 1 -> 0. ---
    remove 0
      Ordmap {
        root=Leaf {k=0; v="0"}
      } ->
      Ordmap {
        root=Empty
      }
    --- Member, length 2 -> 1. ---
    remove 1
      Ordmap {
        root=
          Node {
            l=Leaf {k=0; v="0"};
            k=1;
            v="1";
            n=2;
            h=2;
            r=Empty
          }
      } ->
      Ordmap {
        root=Leaf {k=0; v="0"}
      }
    --- Member, length 3 -> 2. ---
    remove 2
      Ordmap {
        root=
          Node {
            l=Leaf {k=0; v="0"};
            k=1;
            v="1";
            n=3;
            h=2;
            r=Leaf {k=2; v="2"}
          }
      } ->
      Ordmap {
        root=
          Node {
            l=Leaf {k=0; v="0"};
            k=1;
            v="1";
            n=2;
            h=2;
            r=Empty
          }
      }
    |}]

let%expect_test "of_alist,remove_hlt" =
  let open Format in
  printf "@[";
  let test k ordmap descr = begin
    validate ordmap;
    printf "--- %s ---@\n" descr;
    let ordmap' = remove_hlt k ordmap in
    validate ordmap';
    printf "@[<v>remove_hlt %a@;<0 2>@[<v>%a ->@,%a@]@]@\n"
      Usize.pp k (pp String.pp) ordmap (pp String.pp) ordmap'
  end in
  let test_tuples = [
    ([(0, "0")], 0,                     "Member, length 1 -> 0.");
    ([(0, "0"); (1, "1")], 1,           "Member, length 2 -> 1.");
    ([(0, "0"); (1, "1"); (2, "2")], 2, "Member, length 3 -> 2.");
  ] in
  List.iter test_tuples ~f:(fun (kvs, k, descr) ->
    let ordmap = of_alist (module Usize) kvs in
    test k ordmap descr
  );
  printf "@]";

  [%expect{|
    --- Member, length 1 -> 0. ---
    remove_hlt 0
      Ordmap {
        root=Leaf {k=0; v="0"}
      } ->
      Ordmap {
        root=Empty
      }
    --- Member, length 2 -> 1. ---
    remove_hlt 1
      Ordmap {
        root=
          Node {
            l=Leaf {k=0; v="0"};
            k=1;
            v="1";
            n=2;
            h=2;
            r=Empty
          }
      } ->
      Ordmap {
        root=Leaf {k=0; v="0"}
      }
    --- Member, length 3 -> 2. ---
    remove_hlt 2
      Ordmap {
        root=
          Node {
            l=Leaf {k=0; v="0"};
            k=1;
            v="1";
            n=3;
            h=2;
            r=Leaf {k=2; v="2"}
          }
      } ->
      Ordmap {
        root=
          Node {
            l=Leaf {k=0; v="0"};
            k=1;
            v="1";
            n=2;
            h=2;
            r=Empty
          }
      }
    |}]

let%expect_test "of_array,cursor" =
  let open Format in
  printf "@[";
  let test_fwd ordmap = begin
    let rec fn cursor = begin
      match Cursor.(cursor = (tl ordmap)) with
      | true -> printf "@\n"
      | false -> begin
          let i = Cursor.index cursor in
          assert Cursor.((seek (Usize.to_isize i) (hd ordmap)) = cursor);
          printf "            %a=%a@\n"
            Cursor.pp cursor
            (pp_kv String.pp) (Cursor.rget cursor);
          fn (Cursor.succ cursor)
        end
    end in
    printf "cursor fwd:@\n";
    fn (Cursor.hd ordmap);
  end in
  let test_rev ordmap = begin
    let rec fn cursor = begin
      match Cursor.(cursor = (hd ordmap)) with
      | true -> printf "@\n"
      | false -> begin
          let i = Cursor.index cursor in
          assert Cursor.((seek (Usize.to_isize i) (hd ordmap)) = cursor);
          printf "            %a=%a@\n"
            Cursor.pp cursor
            (pp_kv String.pp) (Cursor.lget cursor);
          fn (Cursor.pred cursor)
        end
    end in
    printf "cursor rev:@\n";
    fn (Cursor.tl ordmap);
  end in
  let test kvs = begin
    let ordmap = of_array (module Usize) kvs in
    printf "of_array %a -> @,%a@\n"
      (Array.pp (pp_kv String.pp)) kvs
      (pp String.pp) ordmap;
    validate ordmap;
    test_fwd ordmap;
    test_rev ordmap
  end in
  let test_arrays = [
    [||];
    [|(0, "0"); (1, "1"); (4, "4"); (5, "5"); (3, "3"); (2, "2")|];
  ] in
  List.iter test_arrays ~f:(fun kvs ->
    test kvs
  );
  printf "@]";

  [%expect{|
    of_array [||] ->
    Ordmap {
      root=Empty
    }
    cursor fwd:

    cursor rev:

    of_array [|(0, "0"); (1, "1"); (4, "4"); (5, "5"); (3, "3"); (2, "2")|] ->
    Ordmap {
      root=
        Node {
          l=
            Node {
              l=Leaf {k=0; v="0"};
              k=1;
              v="1";
              n=3;
              h=2;
              r=Leaf {k=2; v="2"}
            };
          k=3;
          v="3";
          n=6;
          h=3;
          r=
            Node {
              l=Empty;
              k=4;
              v="4";
              n=2;
              h=2;
              r=Leaf {k=5; v="5"}
            }
        }
    }
    cursor fwd:
                {index=0; lpath_opt=None; rpath_opt=Some [0; 1; 3]}=(0, "0")
                {index=1; lpath_opt=Some [0; 1; 3]; rpath_opt=Some [1; 3]}=(1, "1")
                {index=2; lpath_opt=Some [1; 3]; rpath_opt=Some [2; 1; 3]}=(2, "2")
                {index=3; lpath_opt=Some [2; 1; 3]; rpath_opt=Some [3]}=(3, "3")
                {index=4; lpath_opt=Some [3]; rpath_opt=Some [4; 3]}=(4, "4")
                {index=5; lpath_opt=Some [4; 3]; rpath_opt=Some [5; 4; 3]}=(5, "5")

    cursor rev:
                {index=6; lpath_opt=Some [5; 4; 3]; rpath_opt=None}=(5, "5")
                {index=5; lpath_opt=Some [4; 3]; rpath_opt=Some [5; 4; 3]}=(4, "4")
                {index=4; lpath_opt=Some [3]; rpath_opt=Some [4; 3]}=(3, "3")
                {index=3; lpath_opt=Some [2; 1; 3]; rpath_opt=Some [3]}=(2, "2")
                {index=2; lpath_opt=Some [1; 3]; rpath_opt=Some [2; 1; 3]}=(1, "1")
                {index=1; lpath_opt=Some [0; 1; 3]; rpath_opt=Some [1; 3]}=(0, "0")
    |}]

let%expect_test "of_alist,to_alist,to_array" =
  let open Format in
  printf "@[<h>";
  let test kvs = begin
    let ordmap = of_alist (module Usize) kvs in
    printf "of_alist %a; to_alist -> %a; to_array -> %a\n"
      (List.pp (pp_kv String.pp)) kvs
      (List.pp (pp_kv String.pp)) (to_alist ordmap)
      (Array.pp (pp_kv String.pp)) (to_array ordmap)
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
    of_alist []; to_alist -> []; to_array -> [||]
    of_alist [(0, "0")]; to_alist -> [(0, "0")]; to_array -> [|(0, "0")|]
    of_alist [(0, "0"); (1, "1")]; to_alist -> [(0, "0"); (1, "1")]; to_array -> [|(0, "0"); (1, "1")|]
    of_alist [(0, "0"); (1, "1"); (2, "2")]; to_alist -> [(0, "0"); (1, "1"); (2, "2")]; to_array -> [|(0, "0"); (1, "1"); (2, "2")|]
    of_alist [(0, "0"); (1, "1"); (66, "66")]; to_alist -> [(0, "0"); (1, "1"); (66, "66")]; to_array -> [|(0, "0"); (1, "1"); (66, "66")|]
    of_alist [(0, "0"); (1, "1"); (66, "66"); (91, "91")]; to_alist -> [(0, "0"); (1, "1"); (66, "66"); (91, "91")]; to_array -> [|(0, "0"); (1, "1"); (66, "66"); (91, "91")|]
    |}]

let%expect_test "choose_hlt" =
  let open Format in
  printf "@[";
  (* test is n^2 time complexity, so keep n small. *)
  let rec test n i ordmap = begin
    match i < n with
    | false -> ordmap
    | true -> begin
        validate ordmap;
        let ordmap' = test n (succ i) (insert_hlt ~k:i ~v:(i * 100) ordmap) in
        let k, v = choose_hlt ordmap' in
        assert (k * 100 = v);
        let ordmap'' = remove_hlt k ordmap' in
        validate ordmap'';
        assert ((length ordmap') = (length ordmap'') + 1);
        ordmap''
      end
  end in
  let e = empty (module Usize) in
  let _ = test 100 0 e in
  printf "@]";

  [%expect{|
    |}]

let%expect_test "search,nth" =
  let open Format in
  let test_search ordmap key_max = begin
    printf "%a@\n" (pp Usize.pp) ordmap;
    for probe = 0 to key_max do
      printf "  %a -> %s, %s, %s@\n" Usize.pp probe
        (match psearch probe ordmap with
          | None -> "<"
          | Some (Cmp.Lt, i) -> asprintf "<[%a]=%a"
              Usize.pp i (pp_kv Usize.pp) (nth i ordmap)
          | Some (Cmp.Eq, i) -> asprintf "=[%a]=%a"
              Usize.pp i (pp_kv Usize.pp) (nth i ordmap)
          | Some (Cmp.Gt, i) -> asprintf ">[%a]=%a"
              Usize.pp i (pp_kv Usize.pp) (nth i ordmap)
        )
        (match search probe ordmap with
          | None -> "<>"
          | Some i -> asprintf "=%a" (pp_kv Usize.pp) (nth i ordmap)
        )
        (match nsearch probe ordmap with
          | Some (Cmp.Lt, i) -> asprintf "<[%a]=%a"
              Usize.pp i (pp_kv Usize.pp) (nth i ordmap)
          | Some (Cmp.Eq, i) -> asprintf "=[%a]=%a"
              Usize.pp i (pp_kv Usize.pp) (nth i ordmap)
          | Some (Cmp.Gt, i) -> asprintf ">[%a]=%a"
              Usize.pp i (pp_kv Usize.pp) (nth i ordmap)
          | None -> ">"
        );
    done
  end in
  printf "@[";
  for len = 0 to 3 do
    let ordmap = of_array (module Usize)
      (Array.init len ~f:(fun i -> let k = (i * 2 + 1) in k, k * 10)) in
    let key_max = len * 2 in
    test_search ordmap key_max
  done;
  printf "@]";

  [%expect{|
    Ordmap {
      root=Empty
    }
      0 -> <, <>, >
    Ordmap {
      root=Leaf {k=1; v=10}
    }
      0 -> <[0]=(1, 10), <>, <[0]=(1, 10)
      1 -> =[0]=(1, 10), =(1, 10), =[0]=(1, 10)
      2 -> >[0]=(1, 10), <>, >[0]=(1, 10)
    Ordmap {
      root=
        Node {
          l=Empty;
          k=1;
          v=10;
          n=2;
          h=2;
          r=Leaf {k=3; v=30}
        }
    }
      0 -> <[0]=(1, 10), <>, <[0]=(1, 10)
      1 -> =[0]=(1, 10), =(1, 10), =[0]=(1, 10)
      2 -> >[0]=(1, 10), <>, <[1]=(3, 30)
      3 -> =[1]=(3, 30), =(3, 30), =[1]=(3, 30)
      4 -> >[1]=(3, 30), <>, >[1]=(3, 30)
    Ordmap {
      root=
        Node {
          l=Leaf {k=1; v=10};
          k=3;
          v=30;
          n=3;
          h=2;
          r=Leaf {k=5; v=50}
        }
    }
      0 -> <[0]=(1, 10), <>, <[0]=(1, 10)
      1 -> =[0]=(1, 10), =(1, 10), =[0]=(1, 10)
      2 -> >[0]=(1, 10), <>, <[1]=(3, 30)
      3 -> =[1]=(3, 30), =(3, 30), =[1]=(3, 30)
      4 -> >[1]=(3, 30), <>, <[2]=(5, 50)
      5 -> =[2]=(5, 50), =(5, 50), =[2]=(5, 50)
      6 -> >[2]=(5, 50), <>, >[2]=(5, 50)
    |}]

let%expect_test "fold_until" =
  let test ks = begin
    let ordmap = of_klist ks in
    (* Compute the number of elements in the triangle defined by folding n
     * times, each time terminating upon encounter of a distinct key.  The size
     * of the triangle is insensitive to fold order. *)
    assert ((List.length ks) = (length ordmap));
    let n = length ordmap in
    let triangle_sum = List.fold ks ~init:0 ~f:(fun accum k ->
      accum + fold_until ordmap ~init:0 ~f:(fun accum (k1, _) ->
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
  ] in
  List.iter test_lists ~f:(fun ks ->
    test ks
  );

  [%expect{|
    |}]

let%expect_test "fold_right_until" =
  let test ks = begin
    let ordmap = of_klist ks in
    (* Compute the number of elements in the triangle defined by folding n
     * times, each time terminating upon encounter of a distinct key.  The size
     * of the triangle is insensitive to fold order. *)
    assert ((List.length ks) = (length ordmap));
    let n = length ordmap in
    let triangle_sum = List.fold ks ~init:0 ~f:(fun accum k ->
      accum + fold_right_until ordmap ~init:0 ~f:(fun (k1, _) accum ->
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
  ] in
  List.iter test_lists ~f:(fun ks ->
    test ks
  );

  [%expect{|
    |}]

let%expect_test "fold2_until" =
  let test ks0 ks1 = begin
    let ordmap0 = of_klist ks0 in
    let ordmap1 = of_klist ks1 in
    let ordmap = union ~f:merge ordmap0 ordmap1 in
    let kvs = to_alist ordmap in
    (* Compute the number of elements in the triangle defined by folding n
     * times, each time terminating upon encounter of a distinct key.  The size
     * of the triangle is insensitive to fold order. *)
    assert ((List.length kvs) = (length ordmap));
    let n = length ordmap in
    let triangle_sum = List.fold kvs ~init:0 ~f:(fun accum (k, _) ->
      accum + fold2_until ordmap0 ordmap1 ~init:0
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
    let ordmap0 = of_klist ks0 in
    let ordmap1 = of_klist ks1 in
    let pairs = fold2 ~init:[] ~f:(fun accum kv0_opt kv1_opt ->
      (kv0_opt, kv1_opt) :: accum
    ) ordmap0 ordmap1 in
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
    fold2 [] [0; 1] -> [(None, Some (1, 100)); (None, Some (0, 0))]
    fold2 [] [0; 1; 2] -> [(None, Some (2, 200)); (None, Some (1, 100)); (None, Some (0, 0))]
    fold2 [] [0; 1; 66] -> [(None, Some (66, 6600)); (None, Some (1, 100)); (None, Some (0, 0))]
    fold2 [] [0; 1; 66; 91] -> [(None, Some (91, 9100)); (None, Some (66, 6600)); (None, Some (1, 100)); (None, Some (0, 0))]
    fold2 [0] [0] -> [(Some (0, 0), Some (0, 0))]
    fold2 [0] [0; 1] -> [(None, Some (1, 100)); (Some (0, 0), Some (0, 0))]
    fold2 [0] [0; 1; 2] -> [(None, Some (2, 200)); (None, Some (1, 100)); (Some (0, 0), Some (0, 0))]
    fold2 [0] [0; 1; 66] -> [(None, Some (66, 6600)); (None, Some (1, 100)); (Some (0, 0), Some (0, 0))]
    fold2 [0] [0; 1; 66; 91] -> [(None, Some (91, 9100)); (None, Some (66, 6600)); (None, Some (1, 100)); (Some (0, 0), Some (0, 0))]
    fold2 [0; 1] [0; 1] -> [(Some (1, 100), Some (1, 100)); (Some (0, 0), Some (0, 0))]
    fold2 [0; 1] [0; 1; 2] -> [(None, Some (2, 200)); (Some (1, 100), Some (1, 100)); (Some (0, 0), Some (0, 0))]
    fold2 [0; 1] [0; 1; 66] -> [(None, Some (66, 6600)); (Some (1, 100), Some (1, 100)); (Some (0, 0), Some (0, 0))]
    fold2 [0; 1] [0; 1; 66; 91] -> [(None, Some (91, 9100)); (None, Some (66, 6600)); (Some (1, 100), Some (1, 100)); (Some (0, 0), Some (0, 0))]
    fold2 [0; 1; 2] [0; 1; 2] -> [(Some (2, 200), Some (2, 200)); (Some (1, 100), Some (1, 100)); (Some (0, 0), Some (0, 0))]
    fold2 [0; 1; 2] [0; 1; 66] -> [(None, Some (66, 6600)); (Some (2, 200), None); (Some (1, 100), Some (1, 100)); (Some (0, 0), Some (0, 0))]
    fold2 [0; 1; 2] [0; 1; 66; 91] -> [(None, Some (91, 9100)); (None, Some (66, 6600)); (Some (2, 200), None); (Some (1, 100), Some (1, 100)); (Some (0, 0), Some (0, 0))]
    fold2 [0; 1; 66] [0; 1; 66] -> [(Some (66, 6600), Some (66, 6600)); (Some (1, 100), Some (1, 100)); (Some (0, 0), Some (0, 0))]
    fold2 [0; 1; 66] [0; 1; 66; 91] -> [(None, Some (91, 9100)); (Some (66, 6600), Some (66, 6600)); (Some (1, 100), Some (1, 100)); (Some (0, 0), Some (0, 0))]
    fold2 [0; 1; 66; 91] [0; 1; 66; 91] -> [(Some (91, 9100), Some (91, 9100)); (Some (66, 6600), Some (66, 6600)); (Some (1, 100), Some (1, 100)); (Some (0, 0), Some (0, 0))]
    |}]

let%expect_test "iter2,equal,subset,disjoint" =
  let open Format in
  printf "@[";
  let test_equal ks0 ks1 = begin
    let ordmap0 = of_klist ks0 in
    let ordmap1 = of_klist ks1 in
    assert (equal veq ordmap0 ordmap1);
    assert (subset veq ordmap0 ordmap1);
    assert (subset veq ordmap1 ordmap0);
    assert ((length ordmap0 = 0) || (not (disjoint ordmap0 ordmap1)));
    iter2 ~f:(fun kv0_opt kv1_opt ->
      match kv0_opt, kv1_opt with
      | Some _, Some _ -> ()
      | None, Some _
      | Some _, None -> begin
          printf "Should be equal:@,%a@,%a@\n"
            (pp Usize.pp) ordmap0 (pp Usize.pp) ordmap1;
          assert false;
        end
      | None, None -> not_reached ()
    ) ordmap0 ordmap1
  end in
  let test_disjoint ks0 ks1 = begin
    let ordmap0 = of_klist ks0 in
    let ordmap1 = of_klist ks1 in
    assert (not (equal veq ordmap0 ordmap1));
    assert (not (subset veq ordmap0 ordmap1));
    assert ((length ordmap0 = 0) || (not (subset veq ordmap1 ordmap0)));
    assert (disjoint ordmap0 ordmap1);
    iter2 ~f:(fun kv0_opt kv1_opt ->
      match kv0_opt, kv1_opt with
      | Some _, Some _ -> begin
          printf "Should be disjoint:@,%a@,%a@\n"
            (pp Usize.pp) ordmap0 (pp Usize.pp) ordmap1;
          assert false;
        end
      | None, Some _
      | Some _, None -> ()
      | None, None -> not_reached ()
    ) ordmap0 ordmap1
  end in
  let test_lists = [
    [];
    [0];
    [0; 1];
    [0; 1; 2];
    [0; 1; 66];
    [0; 1; 66; 91];
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
    let ordmap0 = of_klist ks0 in
    let ordmap1 = of_klist ks1 in
    let ordmap = union ~f:merge ordmap0 ordmap1 in
    let kvs = to_alist ordmap in
    List.iter ks0 ~f:(fun k -> assert ((mem k ordmap) && (mem k ordmap0)));
    List.iter ks1 ~f:(fun k -> assert ((mem k ordmap) && (mem k ordmap1)));
    List.iter kvs ~f:(fun (k, _) ->
      assert ((mem k ordmap0) || (mem k ordmap1)));
  end in
  let test_disjoint ks0 ks1 = begin
    let ordmap0 = of_klist ks0 in
    let ordmap1 = of_klist ks1 in
    let ordmap = union ~f:merge ordmap0 ordmap1 in
    assert ((length ordmap) = (length ordmap0) + (length ordmap1));
  end in
  let test_lists = [
    [];
    [0];
    [0; 1];
    [0; 1; 2];
    [0; 1; 66];
    [0; 1; 66; 91];
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
    let ordmap0 = of_klist ks0 in
    let ordmap1 = of_klist ks1 in
    let ordmap = inter ~f:merge ordmap0 ordmap1 in
    let kvs = to_alist ordmap in
    List.iter ks0 ~f:(fun k ->
      assert ((mem k ordmap) || (not (mem k ordmap1))));
    List.iter ks1 ~f:(fun k ->
      assert ((mem k ordmap) || (not (mem k ordmap0))));
    List.iter kvs ~f:(fun (k, _) ->
      assert ((mem k ordmap0) && (mem k ordmap1)));
  end in
  let test_disjoint ks0 ks1 = begin
    let ordmap0 = of_klist ks0 in
    let ordmap1 = of_klist ks1 in
    let ordmap = inter ~f:merge ordmap0 ordmap1 in
    assert ((length ordmap) = 0);
  end in
  let test_lists = [
    [];
    [0];
    [0; 1];
    [0; 1; 2];
    [0; 1; 66];
    [0; 1; 66; 91];
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
    let ordmap0 = of_klist ks0 in
    let ordmap1 = of_klist ks1 in
    let ordmap = diff ordmap0 ordmap1 in
    let kvs = to_alist ordmap in
    List.iter ks0 ~f:(fun k -> assert ((mem k ordmap) || (mem k ordmap1)));
    List.iter ks1 ~f:(fun k -> assert (not (mem k ordmap)));
    List.iter kvs ~f:(fun (k, _) ->
      assert ((mem k ordmap0) && (not (mem k ordmap1))));
  end in
  let test_disjoint ks0 ks1 = begin
    let ordmap0 = of_klist ks0 in
    let ordmap1 = of_klist ks1 in
    let ordmap = diff ordmap0 ordmap1 in
    assert ((length ordmap) = (length ordmap0));
  end in
  let test_lists = [
    [];
    [0];
    [0; 1];
    [0; 1; 2];
    [0; 1; 66];
    [0; 1; 66; 91];
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
  let test arr = begin
    let ordmap = of_karray arr in
    let ordmap' = filter ordmap ~f:(fun (k, _) -> k % 2 = 0) in
    let arr' = to_array ordmap' in
    printf "%a -> %a@\n"
      (Array.pp Usize.pp) arr
      (Array.pp (pp_kv Usize.pp)) arr'
  end in
  for n = 0 to 6 do
    let arr = Array.init n ~f:(fun i -> i) in
    test arr
  done;
  printf "@]";

  [%expect{|
    [||] -> [||]
    [|0|] -> [|(0, 0)|]
    [|0; 1|] -> [|(0, 0)|]
    [|0; 1; 2|] -> [|(0, 0); (2, 200)|]
    [|0; 1; 2; 3|] -> [|(0, 0); (2, 200)|]
    [|0; 1; 2; 3; 4|] -> [|(0, 0); (2, 200); (4, 400)|]
    [|0; 1; 2; 3; 4; 5|] -> [|(0, 0); (2, 200); (4, 400)|]
    |}]

let%expect_test "filter_map" =
  let open Format in
  printf "@[<h>";
  let test arr = begin
    let ordmap = of_karray arr in
    let ordmap' = filter_map ordmap ~f:(fun (k, v) ->
      match k % 2 = 0 with
      | true -> Some (Usize.to_string v)
      | false -> None
    ) in
    let arr' = to_array ordmap' in
    printf "%a -> %a@\n"
      (Array.pp Usize.pp) arr
      (Array.pp (pp_kv String.pp)) arr'
  end in
  for n = 0 to 6 do
    let arr = Array.init n ~f:(fun i -> i) in
    test arr
  done;
  printf "@]";

  [%expect{|
    [||] -> [||]
    [|0|] -> [|(0, "0")|]
    [|0; 1|] -> [|(0, "0")|]
    [|0; 1; 2|] -> [|(0, "0"); (2, "200")|]
    [|0; 1; 2; 3|] -> [|(0, "0"); (2, "200")|]
    [|0; 1; 2; 3; 4|] -> [|(0, "0"); (2, "200"); (4, "400")|]
    [|0; 1; 2; 3; 4; 5|] -> [|(0, "0"); (2, "200"); (4, "400")|]
    |}]

let%expect_test "filteri" =
  let open Format in
  printf "@[<h>";
  let test arr = begin
    let ordmap = of_karray arr in
    let ordmap' = filteri ordmap ~f:(fun i _kv -> i % 2 = 0) in
    let arr' = to_array ordmap' in
    printf "%a -> %a@\n"
      (Array.pp Usize.pp) arr
      (Array.pp (pp_kv Usize.pp)) arr'
  end in
  for n = 0 to 6 do
    let arr = Array.init n ~f:(fun i -> i * 10) in
    test arr
  done;
  printf "@]";

  [%expect{|
    [||] -> [||]
    [|0|] -> [|(0, 0)|]
    [|0; 10|] -> [|(0, 0)|]
    [|0; 10; 20|] -> [|(0, 0); (20, 2000)|]
    [|0; 10; 20; 30|] -> [|(0, 0); (20, 2000)|]
    [|0; 10; 20; 30; 40|] -> [|(0, 0); (20, 2000); (40, 4000)|]
    [|0; 10; 20; 30; 40; 50|] -> [|(0, 0); (20, 2000); (40, 4000)|]
    |}]

let%expect_test "filteri_map" =
  let open Format in
  printf "@[<h>";
  let test arr = begin
    let ordmap = of_karray arr in
    let ordmap' = filteri_map ordmap ~f:(fun i (_, v) ->
      match i % 2 = 0 with
      | true -> Some (Usize.to_string v)
      | false -> None
    ) in
    let arr' = to_array ordmap' in
    printf "%a -> %a@\n"
      (Array.pp Usize.pp) arr
      (Array.pp (pp_kv String.pp)) arr'
  end in
  for n = 0 to 6 do
    let arr = Array.init n ~f:(fun i -> i * 10) in
    test arr
  done;
  printf "@]";

  [%expect{|
    [||] -> [||]
    [|0|] -> [|(0, "0")|]
    [|0; 10|] -> [|(0, "0")|]
    [|0; 10; 20|] -> [|(0, "0"); (20, "2000")|]
    [|0; 10; 20; 30|] -> [|(0, "0"); (20, "2000")|]
    [|0; 10; 20; 30; 40|] -> [|(0, "0"); (20, "2000"); (40, "4000")|]
    [|0; 10; 20; 30; 40; 50|] -> [|(0, "0"); (20, "2000"); (40, "4000")|]
    |}]

let%expect_test "partition_tf" =
  let open Format in
  printf "@[<h>";
  let test arr = begin
    let ordmap = of_karray arr in
    let t_ordmap, f_ordmap = partition_tf ordmap ~f:(fun (k, _) -> k % 2 = 0) in
    let t_arr = to_array t_ordmap in
    let f_arr = to_array f_ordmap in
    printf "%a -> %a / %a@\n"
      (Array.pp Usize.pp) arr
      (Array.pp (pp_kv Usize.pp)) t_arr
      (Array.pp (pp_kv Usize.pp)) f_arr
  end in
  for n = 0 to 6 do
    let arr = Array.init n ~f:(fun i -> i) in
    test arr
  done;
  printf "@]";

  [%expect{|
    [||] -> [||] / [||]
    [|0|] -> [|(0, 0)|] / [||]
    [|0; 1|] -> [|(0, 0)|] / [|(1, 100)|]
    [|0; 1; 2|] -> [|(0, 0); (2, 200)|] / [|(1, 100)|]
    [|0; 1; 2; 3|] -> [|(0, 0); (2, 200)|] / [|(1, 100); (3, 300)|]
    [|0; 1; 2; 3; 4|] -> [|(0, 0); (2, 200); (4, 400)|] / [|(1, 100); (3, 300)|]
    [|0; 1; 2; 3; 4; 5|] -> [|(0, 0); (2, 200); (4, 400)|] / [|(1, 100); (3, 300); (5, 500)|]
    |}]

let%expect_test "partitioni_tf" =
  let open Format in
  printf "@[<h>";
  let test arr = begin
    let ordmap = of_karray arr in
    let t_ordmap, f_ordmap = partitioni_tf ordmap ~f:(fun i _kv -> i % 2 = 0) in
    let t_arr = to_array t_ordmap in
    let f_arr = to_array f_ordmap in
    printf "%a -> %a / %a@\n"
      (Array.pp Usize.pp) arr
      (Array.pp (pp_kv Usize.pp)) t_arr
      (Array.pp (pp_kv Usize.pp)) f_arr
  end in
  for n = 0 to 6 do
    let arr = Array.init n ~f:(fun i -> i * 10) in
    test arr
  done;
  printf "@]";

  [%expect{|
    [||] -> [||] / [||]
    [|0|] -> [|(0, 0)|] / [||]
    [|0; 10|] -> [|(0, 0)|] / [|(10, 1000)|]
    [|0; 10; 20|] -> [|(0, 0); (20, 2000)|] / [|(10, 1000)|]
    [|0; 10; 20; 30|] -> [|(0, 0); (20, 2000)|] / [|(10, 1000); (30, 3000)|]
    [|0; 10; 20; 30; 40|] -> [|(0, 0); (20, 2000); (40, 4000)|] / [|(10, 1000); (30, 3000)|]
    [|0; 10; 20; 30; 40; 50|] -> [|(0, 0); (20, 2000); (40, 4000)|] / [|(10, 1000); (30, 3000); (50, 5000)|]
    |}]

let%expect_test "partition_map" =
  let open Format in
  printf "@[<h>";
  let test arr = begin
    let ordmap = of_karray arr in
    let a_ordmap, b_ordmap = partition_map ordmap ~f:(fun (k, v) ->
      match k % 2 = 0 with
      | true -> First (Usize.to_string v)
      | false -> Second (Usize.to_isize v)
    ) in
    let a_arr = to_array a_ordmap in
    let b_arr = to_array b_ordmap in
    printf "%a -> %a / %a@\n"
      (Array.pp Usize.pp) arr
      (Array.pp (pp_kv String.pp)) a_arr
      (Array.pp (pp_kv Isize.pp)) b_arr
  end in
  for n = 0 to 6 do
    let arr = Array.init n ~f:(fun i -> i) in
    test arr
  done;
  printf "@]";

  [%expect{|
    [||] -> [||] / [||]
    [|0|] -> [|(0, "0")|] / [||]
    [|0; 1|] -> [|(0, "0")|] / [|(1, 100i)|]
    [|0; 1; 2|] -> [|(0, "0"); (2, "200")|] / [|(1, 100i)|]
    [|0; 1; 2; 3|] -> [|(0, "0"); (2, "200")|] / [|(1, 100i); (3, 300i)|]
    [|0; 1; 2; 3; 4|] -> [|(0, "0"); (2, "200"); (4, "400")|] / [|(1, 100i); (3, 300i)|]
    [|0; 1; 2; 3; 4; 5|] -> [|(0, "0"); (2, "200"); (4, "400")|] / [|(1, 100i); (3, 300i); (5, 500i)|]
    |}]

let%expect_test "partitioni_map" =
  let open Format in
  printf "@[<h>";
  let test arr = begin
    let ordmap = of_karray arr in
    let a_ordmap, b_ordmap = partitioni_map ordmap ~f:(fun i (_, v) ->
      match i % 2 = 0 with
      | true -> First (Usize.to_string v)
      | false -> Second (Usize.to_isize v)
    ) in
    let a_arr = to_array a_ordmap in
    let b_arr = to_array b_ordmap in
    printf "%a -> %a / %a@\n"
      (Array.pp Usize.pp) arr
      (Array.pp (pp_kv String.pp)) a_arr
      (Array.pp (pp_kv Isize.pp)) b_arr
  end in
  for n = 0 to 6 do
    let arr = Array.init n ~f:(fun i -> i * 10) in
    test arr
  done;
  printf "@]";

  [%expect{|
    [||] -> [||] / [||]
    [|0|] -> [|(0, "0")|] / [||]
    [|0; 10|] -> [|(0, "0")|] / [|(10, 1000i)|]
    [|0; 10; 20|] -> [|(0, "0"); (20, "2000")|] / [|(10, 1000i)|]
    [|0; 10; 20; 30|] -> [|(0, "0"); (20, "2000")|] / [|(10, 1000i); (30, 3000i)|]
    [|0; 10; 20; 30; 40|] -> [|(0, "0"); (20, "2000"); (40, "4000")|] / [|(10, 1000i); (30, 3000i)|]
    [|0; 10; 20; 30; 40; 50|] -> [|(0, "0"); (20, "2000"); (40, "4000")|] / [|(10, 1000i); (30, 3000i); (50, 5000i)|]
    |}]

let%expect_test "kreduce" =
  let open Format in
  printf "@[<h>";
  let test ks = begin
    let ordmap = of_klist ks in
    let sum = kreduce ~f:( + ) ordmap in
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
    let ordmap = of_klist ks in
    let sum = reduce ~f:( + ) ordmap in
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
  let rec test n i e ordmap = begin
    match i < n with
    | false -> ordmap
    | true -> begin
        let ordmap' = remove_hlt i
            (test n (succ i) e (insert_hlt ~k:i ~v:(i * 100) ordmap)) in
        assert (equal veq ordmap ordmap');
        assert (equal veq ordmap (union ~f:merge ordmap ordmap'));
        assert (equal veq ordmap (inter ~f:merge ordmap ordmap'));
        assert (equal veq e (diff ordmap ordmap'));
        validate ordmap';
        ordmap'
      end
  end in
  let e = empty (module Usize) in
  let _ = test 100 0 e e in
  printf "@]";

  [%expect{|
    |}]

let%expect_test "stress2" =
  let open Format in
  printf "@[";
  (* test is n^2 time complexity, so keep n small. *)
  let veq_u128 v0 v1 = Cmp.is_eq (U128.cmp v0 v1) in
  let merge_u128 k v0 v1 = begin
    assert U128.(k = (bit_not v0));
    assert (veq_u128 v0 v1);
    v0
  end in
  let rec test n i e ordmap = begin
    match i < n with
    | false -> ordmap
    | true -> begin
        (* Hash i in order to test semi-random insertion order. *)
        let h = Hash.(t_of_state (Usize.hash_fold i State.empty)) in
        let ordmap' = remove_hlt h
            (test n (succ i) e (insert_hlt ~k:h ~v:(U128.bit_not h) ordmap)) in
        validate ordmap';
        assert (equal veq_u128 ordmap ordmap');
        assert (equal veq_u128 ordmap (union ~f:merge_u128 ordmap ordmap'));
        assert (equal veq_u128 ordmap (inter ~f:merge_u128 ordmap ordmap'));
        assert (equal veq_u128 e (diff ordmap ordmap'));
        ordmap'
      end
  end in
  let e = empty (module U128) in
  let _ = test 100 0 e e in
  printf "@]";

  [%expect{|
    |}]
