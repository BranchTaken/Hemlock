open Seq_intf

module Make_def (T : I_def) : S_def with type t := T.t and type elm := T.elm =
struct
  let length = T.length

  let next = T.next

  let next_opt t =
    match length t with
    | 0 -> None
    | _ -> Some (next t)
end

module Make_indef (T : I_indef) : S_indef with type t := T.t
                                           and type elm := T.elm = struct
  let next = T.next
end
