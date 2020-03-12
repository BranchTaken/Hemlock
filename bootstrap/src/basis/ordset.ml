(* AVL tree implementation of ordered sets, based on the join operation.
   The join-based approach is both work-optimal and highly parallelizeable.

     Just Join for Parallel Ordered Sets
     Guy E. Blelloch, Daniel Ferizovic, and Yihan Sun
     SPAA '16
     DOI: http://dx.doi.org/10.1145/2935764.2935768
     https://arxiv.org/pdf/1602.02120.pdf
     https://www.cs.ucr.edu/~yihans/papers/join.pdf

     https://en.wikipedia.org/wiki/AVL_tree
     https://en.wikipedia.org/wiki/Join-based_tree_algorithms

   It is possible to implement AVL trees with as little as one bit of balance
   metadata per node, but this implementation is less svelte:

   - Each node tracks the number of nodes in its subtree, in order to support
     O(lg n) indexed access.
   - Each node tracks height rather than balance, in order to simplify the
     join logic.  Join heavily relies on node height, and although it is
     possible to efficiently compute heights based on imbalance metadata, doing
     so in join is complex and brittle.  Join is O(lg n), and the naïve
     approach of computing node height from scratch as needed would introduce
     O(n lg n) complexity to join.  The efficient alternative would be to
     compute the root's height, then incrementally compute node heights.
     The complexity of this is that computed heights must be tightly coupled
     with their nodes, and the computations must take into account imbalance,
     recursive joins, rotations, etc. *)

open Rudiments

module T = struct
  type 'a node =
    | Empty
    | Leaf of {
        (* Set member. *)
        mem: 'a;
      }
    | Node of {
        (* Left subtree. *)
        l: 'a node;
        (* Set member. *)
        mem: 'a;
        (* Subtree node count, including this node. *)
        n: usize;
        (* Node height, inductively defined as (succ (max (height l)
           (height r))), where an empty subtree has height 0, and a leaf has
           height 1. *)
        h: usize;
        (* Right subtree. *)
        r: 'a node;
      }
  type ('a, 'cmp) t = {
    (* Comparator. *)
    cmper: ('a, 'cmp) Cmper.t;
    (* Tree root. *)
    root: 'a node;
  }
  type 'a elm = 'a

  type ('a, 'cmp) cmper =
    (module Cmper.S_mono with type t = 'a and type cmper_witness = 'cmp)

  let nnodes = function
    | Empty -> 0
    | Leaf _ -> 1
    | Node {l=_; mem=_; n; h=_; r=_} -> n

  let height = function
    | Empty -> 0
    | Leaf _ -> 1
    | Node {l=_; mem=_; n=_; h; r=_} -> h

  let leaf_init mem =
    Leaf {mem}

  let node_init l mem r =
    match l, r with
    | Empty, Empty -> leaf_init mem
    | _, _ -> begin
        let nnodes_height = function
          | Empty -> 0, 0
          | Leaf _ -> 1, 1
          | Node {l=_; mem=_; n; h; r=_} -> n, h
        in
        let ln, lh = nnodes_height l in
        let rn, rh = nnodes_height r in
        let n = ln + 1 + rn in
        let h = succ (max lh rh) in
        Node {l; mem; n; h; r}
      end

  let length t =
    nnodes t.root

  let is_empty t =
    (length t) = 0

  (* Path to node.  Incapable of expressing "no path". *)
  module Path = struct
    type 'a elm = {
      node: 'a node;
      index: usize;
    }
    type 'a t = 'a elm list

    (* Record node's index in path, where base is the index of the leftmost node
       in the subtree rooted at node.  Terminate when the path reaches the node
       at index. *)
    let rec descend ~index base node path =
      assert ((index - base) < (nnodes node));
      match node with
      | Leaf _ ->
        assert (index = base);
        {node; index} :: path
      | Node {l=Empty; mem=_; n=_; h=_; r} -> begin
          let node_index = base in
          match Usize.cmp index node_index with
          | Lt -> not_reached ()
          | Eq -> {node; index} :: path
          | Gt -> descend ~index (succ node_index) r
            ({node; index=node_index} :: path)
        end
      | Node {l; mem=_; n=_; h=_; r=Empty} -> begin
          let node_index = base + (nnodes l) in
          match Usize.cmp index node_index with
          | Lt -> descend ~index base l ({node; index=node_index} :: path)
          | Eq -> {node; index} :: path
          | Gt -> not_reached ()
        end
      | Node {l; mem=_; n=_; h=_; r} -> begin
          let node_index = base + (nnodes l) in
          match Usize.cmp index node_index with
          | Lt -> descend ~index base l ({node; index=node_index} :: path)
          | Eq -> {node; index} :: path
          | Gt -> descend ~index (succ node_index) r
            ({node; index=node_index} :: path)
        end
      | Empty -> not_reached ()

    let init index ordset =
      descend ~index 0 ordset.root []

    let index = function
      | [] -> not_reached ()
      | elm :: _ -> elm.index

    (* Ascend until the desired index is beneath node, then descend into the
       appropriate subtree. *)
    let rec seek_nth index path =
      match path with
      | [] -> not_reached ()
      | elm :: tl -> begin
          match Usize.cmp index elm.index with
          | Lt -> begin
              match elm.node with
              | Empty
              | Leaf _ -> seek_nth index tl
              | Node {l; mem=_; n=_; h=_; r=_} -> begin
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
              | Node {l=_; mem=_; n=_; h=_; r} -> begin
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

    let mem = function
      | [] -> not_reached ()
      | elm :: _ -> begin
          match elm.node with
          | Leaf {mem}
          | Node {l=_; mem; n=_; h=_; r=_} -> mem
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
     complete traversals would be ϴ(n lg n) rather than ϴ(n). *)
  module Cursor = struct
    module T = struct
      type ('a, 'cmp) container = ('a, 'cmp) t
      type ('a, 'cmp) t = {
        ordset: ('a, 'cmp) container;
        index: usize;
        (* Separate paths to the nodes to the left and right of the cursor
           efficiently handle edge conditions near the minimum/maximum nodes,
           and they ensure that lget/rget are O(1). *)
        lpath_opt: 'a Path.t option;
        rpath_opt: 'a Path.t option;
      }

      let cmp t0 t1 =
        assert ((length t0.ordset) = (length t1.ordset));
        Usize.cmp t0.index t1.index

      let hd ordset =
        let rpath_opt = match length ordset with
          | 0 -> None
          | _ -> Some (Path.init 0 ordset)
        in
        {ordset; index=0; lpath_opt=None; rpath_opt}

      let tl ordset =
        let index = length ordset in
        let lpath_opt = match index with
          | 0 -> None
          | _ -> Some (Path.init (pred index) ordset)
        in
        {ordset; index; lpath_opt; rpath_opt=None}

      let seek i t =
        match Isize.cmp i (Isize.kv 0) with
        | Lt -> begin
            let u = (Usize.of_isize Isize.(neg i)) in
            match Usize.cmp t.index u with
            | Lt -> halt "Cannot seek before beginning of ordered set"
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
            match Usize.cmp index' (length t.ordset) with
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
            | Gt -> halt "Cannot seek past end of ordered set"
          end

      let succ t =
        seek Isize.one t

      let pred t =
        seek Isize.neg_one t

      let lget t =
        match t.lpath_opt with
        | None -> halt "Out of bounds"
        | Some lpath -> Path.mem lpath

      let rget t =
        match t.rpath_opt with
        | None -> halt "Out of bounds"
        | Some rpath -> Path.mem rpath

      let container t =
        t.ordset

      let index t =
        t.index

      let pp ppf t =
        Format.fprintf ppf "@[<h>{index=%a;@ lpath_opt=%a;@ rpath_opt=%a}@]"
          Usize.pp t.index
          (Option.pp Path.pp) t.lpath_opt
          (Option.pp Path.pp) t.rpath_opt
    end
    include T
    include Cmpable.Make_poly2(T)
  end
end
include T
include Container_common.Make_poly2_fold(T)
include Container_array.Make_poly2_array(T)

let hash_fold t state =
  foldi t ~init:state ~f:(fun i state mem ->
    state
    |> Usize.hash_fold i
    |> t.cmper.hash_fold mem
  )
  |> Usize.hash_fold (length t)

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

let empty m =
  {cmper=m_cmper m; root=Empty}

let singleton m a =
  {cmper=m_cmper m; root=leaf_init a}

let mem a t =
  let open Cmper in
  let rec fn a cmper = function
    | Empty -> false
    | Leaf {mem} -> Cmp.is_eq (cmper.cmp a mem)
    | Node {l; mem; n=_; h=_; r} ->
      match cmper.cmp a mem with
      | Lt -> fn a cmper l
      | Eq -> true
      | Gt -> fn a cmper r
  in
  fn a t.cmper t.root

let expose = function
  | Leaf {mem} -> Empty, mem, Empty
  | Node {l; mem; n=_; h=_; r} -> l, mem, r
  | Empty -> not_reached ()

(* Rotate right, such that nz' becomes the new subtree root.

            nx                  nz'
           /  \                /  \
          /    \              /    \
        nz     (n2)  ===>  (n0)    nx'
       /  \                       /  \
      /    \                     /    \
   (n0)    (n1)               (n1)    (n2) *)
let rotate_right nx =
  let nz, nx_mem, n2 = expose nx in
  let n0, nz_mem, n1 = expose nz in
  let nx' = node_init n1 nx_mem n2 in
  node_init n0 nz_mem nx'

(* Rotate left, such that nz' becomes the new subtree root.

        nx                          nz'
       /  \                        /  \
      /    \                      /    \
   (n0)     nz       ===>       nx'    (n2)
           /  \                /  \
          /    \              /    \
       (n1)    (n2)        (n0)    (n1) *)
let rotate_left nx =
  let n0, nx_mem, nz = expose nx in
  let n1, nz_mem, n2 = expose nz in
  let nx' = node_init n0 nx_mem n1 in
  node_init nx' nz_mem n2

let nth_opt i t =
  let rec fn i = function
    | Empty -> None
    | Leaf {mem} -> Some mem
    | Node {l=Empty; mem; n=_; h=_; r} -> begin
        match Usize.cmp i 0 with
        | Lt -> not_reached ()
        | Eq -> Some mem
        | Gt -> fn (i - 1) r
      end
    | Node {l; mem; n=_; h=_; r=Empty} -> begin
        let l_n = nnodes l in
        match Usize.cmp i l_n with
        | Lt -> fn i l
        | Eq -> Some mem
        | Gt -> None
      end
    | Node {l; mem; n=_; h=_; r} -> begin
        let l_n = nnodes l in
        match Usize.cmp i l_n with
        | Lt -> fn i l
        | Eq -> Some mem
        | Gt -> fn (i - l_n - 1) r
      end
  in
  fn i t.root

let nth i t =
  match nth_opt i t with
  | None -> halt "Out of bounds"
  | Some mem -> mem

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
    | Leaf {mem} -> begin
        let index = base in
        match cmper.cmp a mem with
        | Lt -> lt_empty mode base index
        | Eq -> Some (Eq, index)
        | Gt -> gt_empty mode past index
      end
    | Node {l; mem; n=_; h=_; r} -> begin
        let index = match l with
          | Empty -> base
          | Leaf _ -> base + 1
          | Node {l=_; mem=_; n; h=_; r=_} -> base + n
        in
        match cmper.cmp a mem with
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
module Seq_poly2_fold2 = struct
  type ('a, 'cmp) container = ('a, 'cmp) t
  type 'a elm = 'a
  type ('a, 'cmp) t = {
    cursor_opt: ('a, 'cmp) Cursor.t option;
  }

  let init ordset =
    match length ordset with
    | 0 -> {cursor_opt=None}
    | _ -> {cursor_opt=Some (Cursor.hd ordset)}

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
    let a = Cursor.rget cursor in
    let cursor' = Cursor.succ cursor in
    a, {cursor_opt=Some cursor'}

  let next_opt t =
    match (index t) < (length t) with
    | false -> None
    | true -> Some (next t)

  let cmper = cmper

  let cmp cmper a0 a1 =
    let open Cmper in
    cmper.cmp a0 a1
end
include Seq.Make_poly2_fold2(Seq_poly2_fold2)

let cmp t0 t1 =
  let open Cmp in
  fold2_until ~init:Eq ~f:(fun _ a0_opt a1_opt ->
    match a0_opt, a1_opt with
    | Some _, Some _ -> Eq, false
    | Some _, None -> Lt, true
    | None, Some _ -> Gt, true
    | None, None -> not_reached ()
  ) t0 t1

let equal t0 t1 =
  (* Check lengths first, since it's cheap and easy to do so. *)
  match (length t0) = (length t1) with
  | false -> false
  | true -> Cmp.is_eq (cmp t0 t1)

let join l a r =
  let rec join_left_tall l a r = begin
    let ll, l_mem, lr = expose l in
    match (height lr) <= (height r) + 1 with
    | true -> begin
        let node = node_init lr a r in
        match (height node) <= (height ll) + 1 with
        | true -> node_init ll l_mem node
        | false -> rotate_left (node_init ll l_mem (rotate_right node))
      end
    | false -> begin
        let lr' = join_left_tall lr a r in
        let node = node_init ll l_mem lr' in
        match (height lr') <= (height ll) + 1 with
        | true -> node
        | false -> rotate_left node
      end
  end in
  let rec join_right_tall l a r = begin
    let rl, r_mem, rr = expose r in
    match (height rl) <= (height l) + 1 with
    | true -> begin
        let node = node_init l a rl in
        match (height node) <= (height rr) + 1 with
        | true -> node_init node r_mem rr
        | false -> rotate_right (node_init (rotate_left node) r_mem rr)
      end
    | false -> begin
        let rl' = join_right_tall l a rl in
        let node = node_init rl' r_mem rr in
        match (height rl') <= (height rr) + 1 with
        | true -> node
        | false -> rotate_right node
      end
  end in
  let lh = height l in
  let rh = height r in
  match (lh > rh + 1), (lh + 1 < rh) with
  | true, _ -> join_left_tall l a r
  | _, true -> join_right_tall l a r
  | false, false -> node_init l a r

let join2 l r =
  let rec split_rightmost = function
    | Leaf {mem} -> Empty, mem
    | Node {l; mem; n=_; h=_; r=Empty} -> l, mem
    | Node {l; mem; n=_; h=_; r} -> begin
        let r', a = split_rightmost r in
        (join l mem r'), a
      end
    | Empty -> not_reached ()
  in
  match l with
  | Empty -> r
  | _ -> begin
      let l', a = split_rightmost l in
      join l' a r
    end

let rec split_node a cmper node =
  let open Cmper in
  match node with
  | Empty -> Empty, None, Empty
  | Leaf {mem} -> begin
      match cmper.cmp a mem with
      | Lt -> Empty, None, node
      | Eq -> Empty, (Some mem), Empty
      | Gt -> node, None, Empty
    end
  | Node {l; mem; n=_; h=_; r} -> begin
      match cmper.cmp a mem with
      | Lt -> begin
          let ll, a_opt, lr = split_node a cmper l in
          ll, a_opt, (join lr mem r)
        end
      | Eq -> l, (Some a), r
      | Gt -> begin
          let rl, a_opt, rr = split_node a cmper r in
          (join l mem rl), a_opt, rr
        end
    end

let insert a t =
  let l, a_opt, r = split_node a t.cmper t.root in
  match a_opt with
  | Some _ -> t
  | None -> {t with root=join l a r}

let remove a t =
  let l, a_opt, r = split_node a t.cmper t.root in
  match a_opt with
  | None -> t
  | Some _ -> {t with root=join2 l r}

let of_list m alist =
  match alist with
  | [] -> empty m
  | a :: alist' -> begin
      let rec fn alist ordset = begin
        match alist with
        | [] -> ordset
        | a :: alist' -> fn alist' (insert a ordset)
      end in
      fn alist' (singleton m a)
    end

let split a t =
  let l, a_opt, r = split_node a t.cmper t.root in
  {t with root=l}, a_opt, {t with root=r}

let union t0 t1 =
  let rec fn cmper node0 node1 = begin
    match node0, node1 with
    | _, Empty -> node0
    | Empty, _ -> node1
    | Leaf {mem}, _ -> begin
        let l1, _, r1 = split_node mem cmper node1 in
        join l1 mem r1
      end
    | Node {l; mem; n=_; h=_; r}, _ -> begin
        let l1, _, r1 = split_node mem cmper node1 in
        let l' = fn cmper l l1 in
        let r' = fn cmper r r1 in
        join l' mem r'
      end
  end in
  let root' = fn t0.cmper t0.root t1.root in
  {t0 with root=root'}

let inter t0 t1 =
  let rec fn cmper node0 node1 = begin
    match node0, node1 with
    | _, Empty
    | Empty, _ -> Empty
    | Leaf {mem}, _ -> begin
        let _, mem_opt, _ = split_node mem cmper node1 in
        match mem_opt with
        | Some _ -> node0
        | None -> Empty
      end
    | Node {l; mem; n=_; h=_; r}, _ -> begin
        let l1, mem_opt, r1 = split_node mem cmper node1 in
        let l' = fn cmper l l1 in
        let r' = fn cmper r r1 in
        match mem_opt with
        | Some _ -> join l' mem r'
        | None -> join2 l' r'
      end
  end in
  let root' = fn t0.cmper t0.root t1.root in
  {t0 with root=root'}

let of_array m arr =
  match arr with
  | [||] -> empty m
  | _ -> Array.reduce_hlt (Array.map arr ~f:(fun a -> singleton m a))
    ~f:(fun ordset0 ordset1 -> union ordset0 ordset1)

let diff t0 t1 =
  let rec fn cmper node0 node1 = begin
    match node0, node1 with
    | _, Empty -> node0
    | Empty, _ -> Empty
    | _, Leaf {mem} -> begin
        let l0, _, r0 = split_node mem cmper node0 in
        join2 l0 r0
      end
    | _, Node {r; mem; n=_; h=_; l} -> begin
        let l0, _, r0 = split_node mem cmper node0 in
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
    | Leaf {mem} -> begin
        match f mem with
        | true -> node
        | false -> Empty
      end
    | Node {l; mem; n=_; h=_; r} -> begin
        let l' = fn ~f l in
        let r' = fn ~f r in
        match f mem with
        | true -> join l' mem r'
        | false -> join2 l' r'
      end
  end in
  {t with root=fn ~f t.root}

let filteri ~f t =
  let rec fn ~f base node = begin
    match node with
    | Empty -> Empty
    | Leaf {mem} -> begin
        match f base mem with
        | true -> node
        | false -> Empty
      end
    | Node {l; mem; n=_; h=_; r} -> begin
        let index = base + (nnodes l) in
        let l' = fn ~f base l in
        let r' = fn ~f (succ index) r in
        match f index mem with
        | true -> join l' mem r'
        | false -> join2 l' r'
      end
  end in
  {t with root=fn ~f 0 t.root}

let reduce ~f t =
  let rec fn ~reduce2 = function
    | Leaf {mem} -> mem
    | Node {l; mem; n=_; h=_; r=Empty} -> reduce2 (fn ~reduce2 l) mem
    | Node {l=Empty; mem; n=_; h=_; r} -> reduce2 (fn ~reduce2 r) mem
    | Node {l; mem; n=_; h=_; r} ->
      reduce2 mem (reduce2 (fn ~reduce2 l) (fn ~reduce2 r))
    | Empty -> not_reached ()
  in
  match t.root with
  | Empty -> None
  | _ -> Some (fn ~reduce2:f t.root)

let reduce_hlt ~f t =
  match reduce ~f t with
  | None -> halt "Empty set"
  | Some set -> set

let pp ppf t =
  let open Format in
  let rec pp_node ppf = function
    | Empty -> fprintf ppf "Empty"
    | Leaf {mem} -> fprintf ppf "Leaf {mem=%a}" t.cmper.pp mem
    | Node {l; mem; n; h; r} -> fprintf ppf
        ("@;<0 2>@[<v>Node {@;<0 2>@[<v>l=%a;@,mem=%a;@,n=%a;@,h=%a;@," ^^
            "r=%a@]@,}@]")
        pp_node l
        t.cmper.pp mem
        Usize.pp n
        Usize.pp h
        pp_node r
  in
  fprintf ppf "@[<v>Ordset {@;<0 2>@[<v>root=%a@]@,}@]"
    pp_node t.root

let validate t =
  let rec fn = function
    | Empty -> ()
    | Leaf _ -> ()
    | Node {l; mem; n; h; r} -> begin
        fn l;
        fn r;
        let () = match l with
          | Empty -> ()
          | _ -> begin
              let _, l_mem, _ = expose l in
              assert (Cmp.is_lt (t.cmper.cmp l_mem mem));
            end
        in
        let () = match r with
          | Empty -> ()
          | _ -> begin
              let _, r_mem, _ = expose r in
              assert (Cmp.is_lt (t.cmper.cmp mem r_mem));
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

(******************************************************************************)
(* Begin tests. *)

let%expect_test "hash_fold" =
  let open Format in
  printf "@[";
  let rec fn = function
    | [] -> ()
    | arr :: arrs' -> begin
        let ordset = of_array (module Usize) arr in
        printf "hash_fold (of_array (module Usize) %a) -> %a@\n"
          (Array.pp Usize.pp) arr
          Hash.pp (Hash.t_of_state (hash_fold ordset Hash.State.empty));
        fn arrs'
      end
  in
  let arrs = [
    [||];
    [|0|];
    [|0; 1|];
    [|0; 2|]
  ] in
  fn arrs;
  printf "@]";

  [%expect{|
    hash_fold (of_array (module Usize) [||]) -> 0xb465_a9ec_cd79_1cb6_4bbd_1bf2_7da9_18d6u128
    hash_fold (of_array (module Usize) [|0|]) -> 0x53a3_5e44_9415_8ff4_24a3_88ce_df7b_e5a4u128
    hash_fold (of_array (module Usize) [|0; 1|]) -> 0xa677_190c_1ad3_d08a_d7f7_106c_570d_6d2eu128
    hash_fold (of_array (module Usize) [|0; 2|]) -> 0xcab8_697f_19cd_8c2e_5911_10d8_d88d_5cc0u128
    |}]

let%expect_test "hash_fold empty" =
  let hash_empty state = begin
    state
    |> hash_fold (empty (module Usize))
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
  printf "%a@\n" pp e;

  let s = singleton (cmper_m e) 0 in
  validate s;
  assert (length s = 1);
  printf "%a@\n" pp s;
  printf "@]";

  [%expect{|
    Ordset {
      root=Empty
    }
    Ordset {
      root=Leaf {mem=0}
    }
    |}]

let%expect_test "mem,insert" =
  let open Format in
  printf "@[";
  let rec test ms ordset = begin
    match ms with
    | [] -> printf "%a@\n" pp ordset
    | m :: ms' -> begin
        assert (not (mem m ordset));
        let ordset' = insert m ordset in
        validate ordset';
        assert (mem m ordset');
        test ms' ordset'
      end
  end in
  let ms = [1; 3; 2; 44; 45; 56; 60; 66; 75; 81; 91] in
  test ms (empty (module Usize));
  printf "@]";

  [%expect{|
    Ordset {
      root=
        Node {
          l=
            Node {
              l=Leaf {mem=1};
              mem=2;
              n=3;
              h=2;
              r=Leaf {mem=3}
            };
          mem=44;
          n=11;
          h=4;
          r=
            Node {
              l=
                Node {
                  l=Leaf {mem=45};
                  mem=56;
                  n=3;
                  h=2;
                  r=Leaf {mem=60}
                };
              mem=66;
              n=7;
              h=3;
              r=
                Node {
                  l=Leaf {mem=75};
                  mem=81;
                  n=3;
                  h=2;
                  r=Leaf {mem=91}
                }
            }
        }
    }
    |}]

let%expect_test "of_list duplicate" =
  let open Format in
  printf "@[";
  printf "%a@\n" pp (of_list (module Usize) [0; 0]);
  printf "@]";

  [%expect{|
    Ordset {
      root=Leaf {mem=0}
    }
    |}]

let%expect_test "of_list,remove" =
  let open Format in
  printf "@[";
  let test m ordset descr = begin
    validate ordset;
    printf "--- %s ---@\n" descr;
    let ordset' = remove m ordset in
    validate ordset';
    printf "@[<v>remove %a@;<0 2>@[<v>%a ->@,%a@]@]@\n"
      Usize.pp m pp ordset pp ordset'
  end in
  let test_tuples = [
    ([0; 1], 2,            "Not member.");
    ([0], 0,               "Member, length 1 -> 0.");
    ([0; 1], 1,            "Member, length 2 -> 1.");
    ([0; 1; 2], 2,         "Member, length 3 -> 2.");
  ] in
  List.iter test_tuples ~f:(fun (ms, m, descr) ->
    let ordset = of_list (module Usize) ms in
    test m ordset descr
  );
  printf "@]";

  [%expect{|
    --- Not member. ---
    remove 2
      Ordset {
        root=
          Node {
            l=Leaf {mem=0};
            mem=1;
            n=2;
            h=2;
            r=Empty
          }
      } ->
      Ordset {
        root=
          Node {
            l=Leaf {mem=0};
            mem=1;
            n=2;
            h=2;
            r=Empty
          }
      }
    --- Member, length 1 -> 0. ---
    remove 0
      Ordset {
        root=Leaf {mem=0}
      } ->
      Ordset {
        root=Empty
      }
    --- Member, length 2 -> 1. ---
    remove 1
      Ordset {
        root=
          Node {
            l=Leaf {mem=0};
            mem=1;
            n=2;
            h=2;
            r=Empty
          }
      } ->
      Ordset {
        root=Leaf {mem=0}
      }
    --- Member, length 3 -> 2. ---
    remove 2
      Ordset {
        root=
          Node {
            l=Leaf {mem=0};
            mem=1;
            n=3;
            h=2;
            r=Leaf {mem=2}
          }
      } ->
      Ordset {
        root=
          Node {
            l=Leaf {mem=0};
            mem=1;
            n=2;
            h=2;
            r=Empty
          }
      }
    |}]

let%expect_test "of_array,cursor" =
  let open Format in
  printf "@[";
  let test_fwd ordset = begin
    let rec fn cursor = begin
      match Cursor.(cursor = (tl ordset)) with
      | true -> printf "@\n"
      | false -> begin
          let i = Cursor.index cursor in
          assert Cursor.((seek (Usize.to_isize i) (hd ordset)) = cursor);
          printf "            %a=%a@\n"
            Cursor.pp cursor
            Usize.pp (Cursor.rget cursor);
          fn (Cursor.succ cursor)
        end
    end in
    printf "cursor fwd:@\n";
    fn (Cursor.hd ordset);
  end in
  let test_rev ordset = begin
    let rec fn cursor = begin
      match Cursor.(cursor = (hd ordset)) with
      | true -> printf "@\n"
      | false -> begin
          let i = Cursor.index cursor in
          assert Cursor.((seek (Usize.to_isize i) (hd ordset)) = cursor);
          printf "            %a=%a@\n"
            Cursor.pp cursor
            Usize.pp (Cursor.lget cursor);
          fn (Cursor.pred cursor)
        end
    end in
    printf "cursor rev:@\n";
    fn (Cursor.tl ordset);
  end in
  let test ms = begin
    let ordset = of_array (module Usize) ms in
    printf "of_array %a -> @,%a@\n"
      (Array.pp Usize.pp) ms
      pp ordset;
    validate ordset;
    test_fwd ordset;
    test_rev ordset
  end in
  let test_arrays = [
    [||];
    [|0; 1; 4; 5; 3; 2|];
  ] in
  List.iter test_arrays ~f:(fun ms ->
    test ms
  );
  printf "@]";

  [%expect{|
    of_array [||] ->
    Ordset {
      root=Empty
    }
    cursor fwd:

    cursor rev:

    of_array [|0; 1; 4; 5; 3; 2|] ->
    Ordset {
      root=
        Node {
          l=
            Node {
              l=Leaf {mem=0};
              mem=1;
              n=3;
              h=2;
              r=Leaf {mem=2}
            };
          mem=3;
          n=6;
          h=3;
          r=
            Node {
              l=Empty;
              mem=4;
              n=2;
              h=2;
              r=Leaf {mem=5}
            }
        }
    }
    cursor fwd:
                {index=0; lpath_opt=None; rpath_opt=Some [0; 1; 3]}=0
                {index=1; lpath_opt=Some [0; 1; 3]; rpath_opt=Some [1; 3]}=1
                {index=2; lpath_opt=Some [1; 3]; rpath_opt=Some [2; 1; 3]}=2
                {index=3; lpath_opt=Some [2; 1; 3]; rpath_opt=Some [3]}=3
                {index=4; lpath_opt=Some [3]; rpath_opt=Some [4; 3]}=4
                {index=5; lpath_opt=Some [4; 3]; rpath_opt=Some [5; 4; 3]}=5

    cursor rev:
                {index=6; lpath_opt=Some [5; 4; 3]; rpath_opt=None}=5
                {index=5; lpath_opt=Some [4; 3]; rpath_opt=Some [5; 4; 3]}=4
                {index=4; lpath_opt=Some [3]; rpath_opt=Some [4; 3]}=3
                {index=3; lpath_opt=Some [2; 1; 3]; rpath_opt=Some [3]}=2
                {index=2; lpath_opt=Some [1; 3]; rpath_opt=Some [2; 1; 3]}=1
                {index=1; lpath_opt=Some [0; 1; 3]; rpath_opt=Some [1; 3]}=0
    |}]

let%expect_test "of_list,to_list,to_array" =
  let open Format in
  printf "@[<h>";
  let test ms = begin
    let ordset = of_list (module Usize) ms in
    printf "of_list %a; to_list -> %a; to_array -> %a\n"
      (List.pp Usize.pp) ms
      (List.pp Usize.pp) (to_list ordset)
      (Array.pp Usize.pp) (to_array ordset)
  end in
  let test_lists = [
    [];
    [0];
    [0; 1];
    [0; 1; 2];
    [0; 1; 66];
    [0; 1; 66; 91];
  ] in
  List.iter test_lists ~f:(fun ms ->
    test ms
  );
  printf "@]";

  [%expect{|
    of_list []; to_list -> []; to_array -> [||]
    of_list [0]; to_list -> [0]; to_array -> [|0|]
    of_list [0; 1]; to_list -> [0; 1]; to_array -> [|0; 1|]
    of_list [0; 1; 2]; to_list -> [0; 1; 2]; to_array -> [|0; 1; 2|]
    of_list [0; 1; 66]; to_list -> [0; 1; 66]; to_array -> [|0; 1; 66|]
    of_list [0; 1; 66; 91]; to_list -> [0; 1; 66; 91]; to_array -> [|0; 1; 66; 91|]
    |}]

let%expect_test "search,nth" =
  let open Format in
  let test_search ordset key_max = begin
    printf "%a@\n" pp ordset;
    for probe = 0 to key_max do
      printf "  %a -> %s, %s, %s@\n" Usize.pp probe
        (match psearch probe ordset with
          | None -> "<"
          | Some (Cmp.Lt, i) -> asprintf "<[%a]=%a"
              Usize.pp i Usize.pp (nth i ordset)
          | Some (Cmp.Eq, i) -> asprintf "=[%a]=%a"
              Usize.pp i Usize.pp (nth i ordset)
          | Some (Cmp.Gt, i) -> asprintf ">[%a]=%a"
              Usize.pp i Usize.pp (nth i ordset)
        )
        (match search probe ordset with
          | None -> "<>"
          | Some i -> asprintf "=%a" Usize.pp (nth i ordset)
        )
        (match nsearch probe ordset with
          | Some (Cmp.Lt, i) -> asprintf "<[%a]=%a"
              Usize.pp i Usize.pp (nth i ordset)
          | Some (Cmp.Eq, i) -> asprintf "=[%a]=%a"
              Usize.pp i Usize.pp (nth i ordset)
          | Some (Cmp.Gt, i) -> asprintf ">[%a]=%a"
              Usize.pp i Usize.pp (nth i ordset)
          | None -> ">"
        );
    done
  end in
  printf "@[";
  for len = 0 to 3 do
    let ordset = of_array (module Usize)
      (Array.init len ~f:(fun i -> i * 2 + 1)) in
    let key_max = len * 2 in
    test_search ordset key_max
  done;
  printf "@]";

  [%expect{|
    Ordset {
      root=Empty
    }
      0 -> <, <>, >
    Ordset {
      root=Leaf {mem=1}
    }
      0 -> <[0]=1, <>, <[0]=1
      1 -> =[0]=1, =1, =[0]=1
      2 -> >[0]=1, <>, >[0]=1
    Ordset {
      root=
        Node {
          l=Empty;
          mem=1;
          n=2;
          h=2;
          r=Leaf {mem=3}
        }
    }
      0 -> <[0]=1, <>, <[0]=1
      1 -> =[0]=1, =1, =[0]=1
      2 -> >[0]=1, <>, <[1]=3
      3 -> =[1]=3, =3, =[1]=3
      4 -> >[1]=3, <>, >[1]=3
    Ordset {
      root=
        Node {
          l=Leaf {mem=1};
          mem=3;
          n=3;
          h=2;
          r=Leaf {mem=5}
        }
    }
      0 -> <[0]=1, <>, <[0]=1
      1 -> =[0]=1, =1, =[0]=1
      2 -> >[0]=1, <>, <[1]=3
      3 -> =[1]=3, =3, =[1]=3
      4 -> >[1]=3, <>, <[2]=5
      5 -> =[2]=5, =5, =[2]=5
      6 -> >[2]=5, <>, >[2]=5
    |}]

let%expect_test "fold_until" =
  let test ms = begin
    let ordset = of_list (module Usize) ms in
    (* Compute the number of elements in the triangle defined by folding n
       times, each time terminating upon encounter of a distinct set member.
       The size of the triangle is insensitive to fold order. *)
    assert ((List.length ms) = (length ordset));
    let n = length ordset in
    let triangle_sum = List.fold ms ~init:0 ~f:(fun accum m ->
      accum + fold_until ordset ~init:0 ~f:(fun accum a ->
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
  ] in
  List.iter test_lists ~f:(fun ms ->
    test ms
  );

  [%expect{|
    |}]

let%expect_test "fold_right_until" =
  let test ms = begin
    let ordset = of_list (module Usize) ms in
    (* Compute the number of elements in the triangle defined by folding n
       times, each time terminating upon encounter of a distinct set member.
       The size of the triangle is insensitive to fold order. *)
    assert ((List.length ms) = (length ordset));
    let n = length ordset in
    let triangle_sum = List.fold ms ~init:0 ~f:(fun accum m ->
      accum + fold_right_until ordset ~init:0 ~f:(fun a accum ->
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
  ] in
  List.iter test_lists ~f:(fun ms ->
    test ms
  );

  [%expect{|
    |}]

let%expect_test "fold2_until" =
  let test ms0 ms1 = begin
    let ordset0 = of_list (module Usize) ms0 in
    let ordset1 = of_list (module Usize) ms1 in
    let ordset = union ordset0 ordset1 in
    let ms = to_list ordset in
    (* Compute the number of elements in the triangle defined by folding n
       times, each time terminating upon encounter of a distinct set member.
       The size of the triangle is insensitive to fold order. *)
    assert ((List.length ms) = (length ordset));
    let n = length ordset in
    let triangle_sum = List.fold ms ~init:0 ~f:(fun accum m ->
      accum + fold2_until ordset0 ordset1 ~init:0 ~f:(fun accum a0_opt a1_opt ->
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
    let ordset0 = of_list (module Usize) ms0 in
    let ordset1 = of_list (module Usize) ms1 in
    let pairs = fold2 ~init:[] ~f:(fun accum a0_opt a1_opt ->
      (a0_opt, a1_opt) :: accum
    ) ordset0 ordset1 in
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
    fold2 [] [0; 1] -> [(None, Some 1); (None, Some 0)]
    fold2 [] [0; 1; 2] -> [(None, Some 2); (None, Some 1); (None, Some 0)]
    fold2 [] [0; 1; 66] -> [(None, Some 66); (None, Some 1); (None, Some 0)]
    fold2 [] [0; 1; 66; 91] -> [(None, Some 91); (None, Some 66); (None, Some 1); (None, Some 0)]
    fold2 [0] [0] -> [(Some 0, Some 0)]
    fold2 [0] [0; 1] -> [(None, Some 1); (Some 0, Some 0)]
    fold2 [0] [0; 1; 2] -> [(None, Some 2); (None, Some 1); (Some 0, Some 0)]
    fold2 [0] [0; 1; 66] -> [(None, Some 66); (None, Some 1); (Some 0, Some 0)]
    fold2 [0] [0; 1; 66; 91] -> [(None, Some 91); (None, Some 66); (None, Some 1); (Some 0, Some 0)]
    fold2 [0; 1] [0; 1] -> [(Some 1, Some 1); (Some 0, Some 0)]
    fold2 [0; 1] [0; 1; 2] -> [(None, Some 2); (Some 1, Some 1); (Some 0, Some 0)]
    fold2 [0; 1] [0; 1; 66] -> [(None, Some 66); (Some 1, Some 1); (Some 0, Some 0)]
    fold2 [0; 1] [0; 1; 66; 91] -> [(None, Some 91); (None, Some 66); (Some 1, Some 1); (Some 0, Some 0)]
    fold2 [0; 1; 2] [0; 1; 2] -> [(Some 2, Some 2); (Some 1, Some 1); (Some 0, Some 0)]
    fold2 [0; 1; 2] [0; 1; 66] -> [(None, Some 66); (Some 2, None); (Some 1, Some 1); (Some 0, Some 0)]
    fold2 [0; 1; 2] [0; 1; 66; 91] -> [(None, Some 91); (None, Some 66); (Some 2, None); (Some 1, Some 1); (Some 0, Some 0)]
    fold2 [0; 1; 66] [0; 1; 66] -> [(Some 66, Some 66); (Some 1, Some 1); (Some 0, Some 0)]
    fold2 [0; 1; 66] [0; 1; 66; 91] -> [(None, Some 91); (Some 66, Some 66); (Some 1, Some 1); (Some 0, Some 0)]
    fold2 [0; 1; 66; 91] [0; 1; 66; 91] -> [(Some 91, Some 91); (Some 66, Some 66); (Some 1, Some 1); (Some 0, Some 0)]
    |}]

let%expect_test "iter2,equal" =
  let open Format in
  printf "@[";
  let test_equal ms0 ms1 = begin
    let ordset0 = of_list (module Usize) ms0 in
    let ordset1 = of_list (module Usize) ms1 in
    assert (equal ordset0 ordset1);
    iter2 ~f:(fun a0_opt a1_opt ->
      match a0_opt, a1_opt with
      | Some _, Some _ -> ()
      | None, Some _
      | Some _, None -> begin
          printf "Should be equal:@,%a@,%a@\n" pp ordset0 pp ordset1;
          assert false;
        end
      | None, None -> not_reached ()
    ) ordset0 ordset1
  end in
  let test_disjoint ms0 ms1 = begin
    let ordset0 = of_list (module Usize) ms0 in
    let ordset1 = of_list (module Usize) ms1 in
    assert (not (equal ordset0 ordset1));
    iter2 ~f:(fun a0_opt a1_opt ->
      match a0_opt, a1_opt with
      | Some _, Some _ -> begin
          printf "Should be disjoint:@,%a@,%a@\n" pp ordset0 pp ordset1;
          assert false;
        end
      | None, Some _
      | Some _, None -> ()
      | None, None -> not_reached ()
    ) ordset0 ordset1
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
    let ordset0 = of_list (module Usize) ms0 in
    let ordset1 = of_list (module Usize) ms1 in
    let ordset = union ordset0 ordset1 in
    let ms = to_list ordset in
    List.iter ms0 ~f:(fun m -> assert ((mem m ordset) && (mem m ordset0)));
    List.iter ms1 ~f:(fun m -> assert ((mem m ordset) && (mem m ordset1)));
    List.iter ms ~f:(fun m -> assert ((mem m ordset0) || (mem m ordset1)));
  end in
  let test_disjoint ms0 ms1 = begin
    let ordset0 = of_list (module Usize) ms0 in
    let ordset1 = of_list (module Usize) ms1 in
    let ordset = union ordset0 ordset1 in
    assert ((length ordset) = (length ordset0) + (length ordset1));
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
    let ordset0 = of_list (module Usize) ms0 in
    let ordset1 = of_list (module Usize) ms1 in
    let ordset = inter ordset0 ordset1 in
    let ms = to_list ordset in
    List.iter ms0 ~f:(fun m ->
      assert ((mem m ordset) || (not (mem m ordset1))));
    List.iter ms1 ~f:(fun m ->
      assert ((mem m ordset) || (not (mem m ordset0))));
    List.iter ms ~f:(fun m -> assert ((mem m ordset0) && (mem m ordset1)));
  end in
  let test_disjoint ms0 ms1 = begin
    let ordset0 = of_list (module Usize) ms0 in
    let ordset1 = of_list (module Usize) ms1 in
    let ordset = inter ordset0 ordset1 in
    assert ((length ordset) = 0);
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
    let ordset0 = of_list (module Usize) ms0 in
    let ordset1 = of_list (module Usize) ms1 in
    let ordset = diff ordset0 ordset1 in
    let ms = to_list ordset in
    List.iter ms0 ~f:(fun m -> assert ((mem m ordset) || (mem m ordset1)));
    List.iter ms1 ~f:(fun m -> assert (not (mem m ordset)));
    List.iter ms ~f:(fun m ->
      assert ((mem m ordset0) && (not (mem m ordset1))));
  end in
  let test_disjoint ms0 ms1 = begin
    let ordset0 = of_list (module Usize) ms0 in
    let ordset1 = of_list (module Usize) ms1 in
    let ordset = diff ordset0 ordset1 in
    assert ((length ordset) = (length ordset0));
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

let%expect_test "filter" =
  let open Format in
  printf "@[<h>";
  let test arr = begin
    let ordset = of_array (module Usize) arr in
    let ordset' = filter ordset ~f:(fun mem -> mem % 2 = 0) in
    let arr' = to_array ordset' in
    printf "%a -> %a@\n"
      (Array.pp Usize.pp) arr
      (Array.pp Usize.pp) arr'
  end in
  for n = 0 to 6 do
    let arr = Array.init n ~f:(fun i -> i) in
    test arr
  done;
  printf "@]";

  [%expect{|
    [||] -> [||]
    [|0|] -> [|0|]
    [|0; 1|] -> [|0|]
    [|0; 1; 2|] -> [|0; 2|]
    [|0; 1; 2; 3|] -> [|0; 2|]
    [|0; 1; 2; 3; 4|] -> [|0; 2; 4|]
    [|0; 1; 2; 3; 4; 5|] -> [|0; 2; 4|]
    |}]

let%expect_test "filteri" =
  let open Format in
  printf "@[<h>";
  let test arr = begin
    let ordset = of_array (module Usize) arr in
    let ordset' = filteri ordset ~f:(fun i _mem -> i % 2 = 0) in
    let arr' = to_array ordset' in
    printf "%a -> %a@\n"
      (Array.pp Usize.pp) arr
      (Array.pp Usize.pp) arr'
  end in
  for n = 0 to 6 do
    let arr = Array.init n ~f:(fun i -> i * 10) in
    test arr
  done;
  printf "@]";

  [%expect{|
    [||] -> [||]
    [|0|] -> [|0|]
    [|0; 10|] -> [|0|]
    [|0; 10; 20|] -> [|0; 20|]
    [|0; 10; 20; 30|] -> [|0; 20|]
    [|0; 10; 20; 30; 40|] -> [|0; 20; 40|]
    [|0; 10; 20; 30; 40; 50|] -> [|0; 20; 40|]
    |}]

let%expect_test "reduce" =
  let open Format in
  printf "@[<h>";
  let test ms = begin
    let ordset = of_list (module Usize) ms in
    let sum = reduce ~f:( + ) ordset in
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
    |}]

let%expect_test "stress" =
  let open Format in
  printf "@[";
  (* test is n^2 time complexity, so keep n small. *)
  let rec test n i e ordset = begin
    match i < n with
    | false -> ordset
    | true -> begin
        let ordset' = remove i (test n (succ i) e (insert i ordset)) in
        assert (equal ordset ordset');
        assert (equal ordset (union ordset ordset'));
        assert (equal ordset (inter ordset ordset'));
        assert (equal e (diff ordset ordset'));
        validate ordset';
        ordset'
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
  let rec test n i e ordset = begin
    match i < n with
    | false -> ordset
    | true -> begin
        (* Hash i in order to test semi-random insertion order. *)
        let h = Hash.(t_of_state (Usize.hash_fold i State.empty)) in
        let ordset' = remove h (test n (succ i) e (insert h ordset)) in
        validate ordset';
        assert (equal ordset ordset');
        assert (equal ordset (union ordset ordset'));
        assert (equal ordset (inter ordset ordset'));
        assert (equal e (diff ordset ordset'));
        ordset'
      end
  end in
  let e = empty (module U128) in
  let _ = test 100 0 e e in
  printf "@]";

  [%expect{|
    |}]
