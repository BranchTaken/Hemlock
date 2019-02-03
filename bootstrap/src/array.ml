open Rudiments_functions

type 'a t = 'a array

let empty = [||]

let init n ~f =
  Stdlib.Array.init n f

let of_list l =
  Stdlib.Array.of_list l

let to_list v =
  Stdlib.Array.to_list v

let length v =
  Stdlib.Array.length v

let is_empty v =
  (length v) = 0

let get v i =
  Stdlib.Array.get v i

let mutate v i x =
  Stdlib.Array.set v i x

let copy v =
  Stdlib.Array.copy v

let slice v start stop =
  Stdlib.Array.sub v start stop

let set v i x =
  let v' = copy v in
  mutate v' i x;
  v'

let concat vl =
  Stdlib.Array.concat vl

let append v0 v1 =
  concat [v0; v1]

let append_one v x =
  append v (of_list [x])

let insert v i x =
  let len = length v in
  if i = 0 then
    append (of_list [x]) v
  else if i < len then
    concat [(slice v 0 i); (of_list [x]); (slice v i len)]
  else
    append v (of_list [x])

let remove v i =
  let len = length v in
  match len with
  | 0 ->
    ignore (assert (i = 0));
    empty
  | _ -> begin
      if i = 0 then
        slice v 1 len
      else if i < len then
        concat [(slice v 0 i); (slice v (i + 1) len)]
      else
        slice v 0 (len - 1)
    end

let iter v ~f =
  Stdlib.Array.iter f v

let iteri v ~f =
  Stdlib.Array.iteri f v

let fold v ~init ~f =
  Stdlib.Array.fold_left f init v

let fold_right v ~init ~f =
  Stdlib.Array.fold_right f v init

let foldi v ~init ~f =
  let accum, _ = fold v ~init:(init, 0) ~f:(fun (accum, i) v ->
    let accum' = f i accum v in
    (accum', i + 1)
  ) in
  accum

let foldi_until v ~init ~f =
  let rec lambda accum i = begin
    let elm = get v i in
    let (accum', until) = f i accum elm in
    match until with
    | true -> accum'
    | false -> lambda accum' (i + 1)
  end in
  lambda init 0

let fold_until v ~init ~f =
  let rec lambda accum i = begin
    let elm = get v i in
    let (accum', until) = f accum elm in
    match until with
    | true -> accum'
    | false -> lambda accum' (i + 1)
  end in
  lambda init 0

let compare cmp v0 v1 =
  let rel = foldi_until v0 ~init:0 ~f:(fun i _ elm ->
    match (length v1) <= i with
    | true -> 1, true
    | false -> begin
        let rel = cmp elm (get v1 i) in
        match rel with
        | -1 | 1 -> rel, true
        | 0 -> 0, false
        | _ -> not_reached ()
      end
  ) in
  match rel with
  | -1 | 1 -> rel
  | 0 -> begin
      compare (length v0) (length v1)
    end
  | _ -> not_reached ()

let sexp_of_t t =
  Sexplib.Std.sexp_of_array t

let t_of_sexp sexp =
  Sexplib.Std.array_of_sexp sexp
