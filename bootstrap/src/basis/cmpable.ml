open CmpableIntf

module Make (T : IMono) : SMono with type t := T.t = struct
  include T

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

  let clamp ~min ~max t =
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

  let between ~low ~high t =
    match T.cmp t (clamp ~min:low ~max:high t) with
    | Lt -> false
    | Eq -> true
    | Gt -> false
end

module MakeZero (T : IMonoZero) : SMonoZero with type t := T.t = struct
  include Make(T)

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

module MakePoly (T : IPoly) : SPoly with type 'a t := 'a T.t = struct
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

  let clamp ~min ~max t =
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

  let between ~low ~high t =
    match T.cmp t (clamp ~min:low ~max:high t) with
    | Lt -> false
    | Eq -> true
    | Gt -> false
end

module MakePoly2 (T : IPoly2) : SPoly2
  with type ('a, 'cmp) t := ('a, 'cmp) T.t = struct
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

  let clamp ~min ~max t =
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

  let between ~low ~high t =
    match T.cmp t (clamp ~min:low ~max:high t) with
    | Lt -> false
    | Eq -> true
    | Gt -> false
end

module MakePoly3 (T : IPoly3) : SPoly3
  with type ('k, 'v, 'cmp) t := ('k, 'v, 'cmp) T.t = struct
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

  let clamp ~min ~max t =
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

  let between ~low ~high t =
    match T.cmp t (clamp ~min:low ~max:high t) with
    | Lt -> false
    | Eq -> true
    | Gt -> false
end
