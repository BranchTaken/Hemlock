open Seq_intf

module Make_def (T : I_mono_def) : S_mono_def with type t := T.t
                                               and type elm := T.elm =
struct
  let length = T.length

  let next = T.next

  let next_opt t =
    match length t with
    | len when Uint.(len = (kv 0)) -> None
    | _ -> Some (next t)
end

module Make_indef (T : I_mono_indef) : S_mono_indef with type t := T.t
                                                     and type elm := T.elm =
struct
  let next = T.next
end
