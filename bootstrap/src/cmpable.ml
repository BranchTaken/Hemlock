open Cmpable_intf

module Make (T : I) : S with type t := T.t = struct
  let cmp = T.cmp

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
    assert (match T.cmp min max with
      | Lt
      | Eq -> true
      | Gt -> false
    );
    match T.cmp t min with
    | Lt -> min
    | Eq
    | Gt -> begin
        match T.cmp t max with
        | Lt
        | Eq -> t
        | Gt -> max
      end

  let between t ~low ~high =
    match T.cmp t (clamp t ~min:low ~max:high) with
    | Lt -> false
    | Eq -> true
    | Gt -> false
end

module Make_zero (T : I_zero) : S_zero with type t := T.t = struct
  let cmp = T.cmp
  let zero = T.zero

  let is_positive t =
    match T.cmp t T.zero with
    | Lt
    | Eq -> false
    | Gt -> true

  let is_non_negative t =
    match T.cmp t T.zero with
    | Lt -> false
    | Eq
    | Gt -> true

  let is_negative t =
    match T.cmp t T.zero with
    | Lt -> true
    | Eq
    | Gt -> false

  let is_non_positive t =
    match T.cmp t T.zero with
    | Lt
    | Eq -> true
    | Gt -> false

  let sign t =
    match T.cmp t T.zero with
    | Lt -> Sign.Neg
    | Eq -> Sign.Zero
    | Gt -> Sign.Pos
end

module Make_poly (T : I_poly) : S_poly with type 'a t := 'a T.t
                                        and type 'a elm := 'a T.elm = struct
  let cmp = T.cmp

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
    assert (match T.cmp min max with
      | Lt
      | Eq -> true
      | Gt -> false
    );
    match T.cmp t min with
    | Lt -> min
    | Eq
    | Gt -> begin
        match T.cmp t max with
        | Lt
        | Eq -> t
        | Gt -> max
      end

  let between t ~low ~high =
    match T.cmp t (clamp t ~min:low ~max:high) with
    | Lt -> false
    | Eq -> true
    | Gt -> false
end

