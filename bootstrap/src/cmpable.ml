open Cmpable_intf

module Make_eq (T : I) : S_eq with type t := T.t = struct
  let ( = ) t0 t1 =
    match T.cmp t0 t1 with
    | Lt -> false
    | Eq -> true
    | Gt -> false

  let ( <> ) t0 t1 =
    match T.cmp t0 t1 with
    | Lt -> true
    | Eq -> false
    | Gt -> true
end

module Make_rel (T : I) : S_rel with type t := T.t = struct
  let ( >= ) t0 t1 =
    match T.cmp t0 t1 with
    | Lt -> false
    | Eq
    | Gt -> true

  let ( <= ) t0 t1 =
    match T.cmp t0 t1 with
    | Lt
    | Eq -> true
    | Gt -> false

  let ( = ) t0 t1 =
    match T.cmp t0 t1 with
    | Lt -> false
    | Eq -> true
    | Gt -> false

  let ( > ) t0 t1 =
    match T.cmp t0 t1 with
    | Lt
    | Eq -> false
    | Gt -> true

  let ( < ) t0 t1 =
    match T.cmp t0 t1 with
    | Lt -> true
    | Eq
    | Gt -> false

  let ( <> ) t0 t1 =
    match T.cmp t0 t1 with
    | Lt -> true
    | Eq -> false
    | Gt -> true
end
