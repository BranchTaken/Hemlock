open Basis
open! Basis.Rudiments

type t = {
  (* The index of each element in ipreds/isuccs corresponds to a state index, and the array at each
   * index contains the corresponding ipreds'/isuccs' state indices. *)
  ipreds: State.Index.t array array;
  isuccs: State.Index.t array array;
}

let pp {ipreds; isuccs} formatter =
  let ipreds_states = Ordmap.of_alist (module StateIndex)
    (Array.to_list (Array.mapi ipreds ~f:(fun i elm -> i, elm))) in
  let isuccs_states = Ordmap.of_alist (module StateIndex)
    (Array.to_list (Array.mapi isuccs ~f:(fun i elm -> i, elm))) in
  formatter
  |> Fmt.fmt "{ipreds=" |> Ordmap.fmt ~alt:true (Array.pp StateIndex.pp) ipreds_states
  |> Fmt.fmt "; isuccs=" |> Ordmap.fmt ~alt:true (Array.pp StateIndex.pp) isuccs_states
  |> Fmt.fmt "}"

let length {ipreds; _} =
  Array.length ipreds

let ipreds_of_state_index_impl state_index ipreds =
  Array.get state_index ipreds

let init_ipreds states =
  let insert_ipred ~state_index ~ipred_state_index:ipred_state_index ipreds = begin
    Map.amend state_index ~f:(fun ipreds_opt ->
      let ipreds' = match ipreds_opt with
        | None -> Ordset.singleton (module State.Index) ipred_state_index
        | Some ipreds -> Ordset.insert ipred_state_index ipreds
      in
      Some ipreds'
    ) ipreds
  end in
  (* Incrementally initialize a map of (state index -> immediate predecessor index set). *)
  let ipreds_map = Array.fold ~init:(Map.empty (module State.Index))
    ~f:(fun ipreds
      State.{statenub={lr1itemsetclosure={index=ipred_state_index; _}; _}; actions; gotos; _} ->
      let ipreds = Ordmap.fold ~init:ipreds ~f:(fun ipreds (_, action_set) ->
        Ordset.fold ~init:ipreds ~f:(fun ipreds action ->
          let open State.Action in
          match action with
          | ShiftPrefix state_index
          | ShiftAccept state_index -> insert_ipred ~state_index ~ipred_state_index ipreds
          | Reduce _ -> ipreds
        ) action_set
      ) actions in
      let ipreds = Ordmap.fold ~init:ipreds ~f:(fun ipreds (_, goto) ->
        insert_ipred ~state_index:goto ~ipred_state_index ipreds
      ) gotos in
      ipreds
    ) states
  in
  (* Convert the map to an array, which is sufficient for all lookup needs. *)
  Array.init (0L =:< Array.length states) ~f:(fun state_index ->
    match Map.get state_index ipreds_map with
    | None -> [||]
    | Some ipreds_set -> Ordset.to_array ipreds_set
  )

let init_isuccs ipreds =
  let isuccs_map =
    Range.Uns.fold (0L =:< Array.length ipreds) ~init:(Map.empty (module State.Index))
      ~f:(fun isuccs_map state_index ->
        let ipred_indexes = ipreds_of_state_index_impl state_index ipreds in
        Array.fold ~init:isuccs_map ~f:(fun isuccs_map ipred_index ->
          Map.amend ipred_index ~f:(function
            | None -> Some (Ordset.singleton (module State.Index) state_index)
            | Some isuccs_set -> Some (Ordset.insert state_index isuccs_set)
          ) isuccs_map
        ) ipred_indexes
      ) in
  Array.init (0L =:< Array.length ipreds) ~f:(fun state_index ->
    match Map.get state_index isuccs_map with
    | None -> [||]
    | Some state_index_set -> Ordset.to_array state_index_set
  )

let init states =
  let ipreds = init_ipreds states in
  let isuccs = init_isuccs ipreds in
  assert Uns.(Array.(length ipreds) = (Array.length isuccs));
  {ipreds; isuccs}

let ipreds_of_state_index state_index {ipreds; _} =
  ipreds_of_state_index_impl state_index ipreds

let ipreds_of_state state t =
  ipreds_of_state_index (State.index state) t

let isuccs_of_state_index state_index {isuccs; _} =
  Array.get state_index isuccs

let isuccs_of_state state t =
  isuccs_of_state_index (State.index state) t
