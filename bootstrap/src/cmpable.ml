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

  let ascending = T.cmp

  let descending t0 t1 =
    match T.cmp t0 t1 with
    | Lt -> Cmp.Gt
    | Eq -> Cmp.Eq
    | Gt -> Cmp.Lt

  let clamp t ~min ~max =
    assert (min <= max);
    match min <= t with
    | false -> min
    | true -> begin
        match t <= max with
        | false -> max
        | true -> t
      end

  let between t ~low ~high =
    t = (clamp t ~min:low ~max:high)
end
