open Basis
open Basis.Rudiments

module T = struct
  type t = {
    lr0item: Lr0Item.t;
    follow: Bitset.t;
  }

  let hash_fold {lr0item; follow} state =
    state
    |> Lr0Item.hash_fold lr0item
    |> Bitset.hash_fold follow

  let cmp {lr0item=l0; follow=f0} {lr0item=l1; follow=f1} =
    let open Cmp in
    match Lr0Item.cmp l0 l1 with
    | Lt -> Lt
    | Eq -> Bitset.cmp f0 f1
    | Gt -> Gt

  let pp {lr0item; follow} formatter =
    formatter
    |> Fmt.fmt "{lr0item=" |> Lr0Item.pp lr0item
    |> Fmt.fmt "; follow=" |> Bitset.pp follow

  let pp_hr symbols {lr0item=({prod={prec; _}; _} as lr0item); follow} formatter =
    formatter
    |> Fmt.fmt "["
    |> Lr0Item.pp_hr symbols lr0item
    |> Fmt.fmt ", {"
    |> (fun formatter ->
      Array.foldi ~init:formatter ~f:(fun i formatter symbol_index ->
        formatter
        |> Fmt.fmt (match i with
          | 0L -> ""
          | _ -> ", "
        )
        |> Symbol.pp_hr (Symbols.symbol_of_symbol_index symbol_index symbols)
      ) (Bitset.to_array follow)
    )
    |> Fmt.fmt "}]"
    |> (fun formatter ->
      match prec with
      | None -> formatter
      | Some prec -> formatter |> Fmt.fmt " " |> Prec.pp_hr prec
    )
end
include T
include Identifiable.Make(T)

let init ~lr0item ~follow =
  (* NB: The follow set is always non-empty for well-formed grammars, but nonterms with empty first
   * sets (one form of unreachable nonterms) can result in empty follow sets. Make no attempt here
   * to prohibit empty follow sets. *)
  {lr0item; follow}

(* The concatenation of the RHS symbols to the right of the dot and the follow set comprise an
 * ordered sequence of symbols to be expected. Merge-fold the symbols' first sets (excluding "ε"),
 * until a preceding symbol's first set does not contain "ε". Similarly, if all symbols contain "ε",
 * merge the follow set (excluding "ε"). Merge "ε" if all symbols' first sets and the follow set
 * contain "ε". *)
let first symbols {lr0item; follow} =
  let append_symbol_set first merge_epsilon symbol_set = begin
    let symbol_set_sans_epsilon = Bitset.remove Symbol.epsilon.index symbol_set in
    let first' = Bitset.union symbol_set_sans_epsilon first in
    let contains_epsilon = Bitset.mem Symbol.epsilon.index symbol_set in
    let merge_epsilon' = match contains_epsilon with
      | false -> false
      | true -> merge_epsilon
    in
    first', merge_epsilon'
  end in
  let rhs_indexes = lr0item.prod.rhs_indexes in
  let rhs_slice = Array.Slice.init ~range:(lr0item.dot =:< Array.length rhs_indexes) rhs_indexes in
  (* Merge-fold RHS symbols' first sets. *)
  let first, merge_epsilon = Array.Slice.fold_until
      ~init:(Bitset.empty, true)
      ~f:(fun (first, merge_epsilon) symbol_index ->
        let symbol = Symbols.symbol_of_symbol_index symbol_index symbols in
        let first', merge_epsilon' = append_symbol_set first merge_epsilon Symbol.(symbol.first) in
        (first', merge_epsilon'), not merge_epsilon'
      ) rhs_slice
  in
  (* Append the follow set only if all RHS symbols to the right of the dot contain "ε". *)
  match merge_epsilon with
  | false -> first
  | true -> begin
      let first', merge_epsilon' = append_symbol_set first merge_epsilon follow in
      match merge_epsilon' with
      | false -> first'
      | true -> Bitset.insert Symbol.epsilon.index first'
    end

let is_kernel_item {lr0item={prod; dot}; _} =
  Uns.(dot > 0L) || (Prod.is_synthetic prod)

let is_accept {lr0item={prod={rhs_indexes; _}; dot}; follow} =
  Uns.(=) dot (Array.length rhs_indexes) &&
  Uns.(=) (Bitset.length follow) 1L &&
  Uns.(=) (Bitset.choose_hlt follow) Symbol.(pseudo_end.index)

let follow_union symbol_indexes t =
  let follow = Bitset.union symbol_indexes t.follow in
  {t with follow}
