open Rudiments0

module T = struct
  type t = u64

  let hash_fold t state =
    Hash.State.Gen.init state
    |> Hash.State.Gen.fold_u64 1 ~f:(fun _ -> t)
    |> Hash.State.Gen.fini

  let cmp t0 t1 =
    Sint.cmp (Uns.to_sint Int64.(compare t0 t1)) (Sint.kv 0)

  let zero = Int64.zero

  let one = Int64.one

  let pp ppf t =
    Format.fprintf ppf "%Ldi64" t
end
include T
include Identifiable.Make(T)
include Cmpable.MakeZero(T)

let pp_b ppf t =
  let rec fn x shift = begin
    match shift with
    | 0 -> ()
    | _ -> begin
        if Uns.(shift % 8 = 0 && shift < 64) then Format.fprintf ppf "_";
        let shift' = Uns.pred shift in
        let bit = Int64.(logand (shift_right_logical x shift') (of_int 0x1)) in
        Format.fprintf ppf "%Ld" bit;
        fn x shift'
      end
  end in
  Format.fprintf ppf "0b";
  fn t 64;
  Format.fprintf ppf "i64"

let pp_o ppf t =
  Format.fprintf ppf "0o%Loi64" t

let pp_x ppf t =
  let rec fn x shift = begin
    match shift with
    | 0 -> ()
    | _ -> begin
        if Uns.(shift < 64) then Format.fprintf ppf "_";
        let shift' = shift - 16 in
        Format.fprintf ppf "%04Lx" Int64.(logand (shift_right_logical x shift') (of_int 0xffff));
        fn x shift'
      end
  end in
  Format.fprintf ppf "0x";
  fn t 64;
  Format.fprintf ppf "i64"

let of_string s =
  Int64.of_string s

let to_string t =
  Format.asprintf "%a" pp t

let of_real r =
  (* OCaml handles overflow poorly, but this deficiency has no anticipated impact on bootstrapping.
  *)
  Int64.of_float r

let to_real t =
  Int64.to_float t

let of_uns u =
  let i = Uns.to_sint u in
  match Sint.(i >= (kv 0)) with
  | true -> Int64.of_int u
  | false -> begin
      let sint_sign_bit = Uns.of_sint Sint.min_value in
      Int64.(add (of_int u) (add (of_int sint_sign_bit) (of_int sint_sign_bit)))
    end

let min_value = Int64.min_int

let max_value = Int64.max_int

let succ = U64.succ

let pred = U64.pred

let bit_and = U64.bit_and

let bit_or = U64.bit_or

let bit_xor = U64.bit_xor

let bit_not = U64.bit_not

let bit_sl = U64.bit_sl

let bit_usr = U64.bit_usr

let bit_ssr = U64.bit_ssr

let ( + ) = U64.( + )

let ( - ) = U64.( - )

let ( * ) = U64.( * )

let ( / ) = U64.( / )

let ( % ) = U64.( % )

let ( ** ) t0 t1 =
  (* Decompose the exponent to limit algorithmic complexity. *)
  let rec fn r p n = begin
    match n = zero with
    | true -> r
    | false -> begin
        let r' =
          match (bit_and n one) = zero with
          | true -> r
          | false -> r * p
        in
        let p' = p * p in
        let n' = bit_usr ~shift:1 n in
        fn r' p' n'
      end
  end in
  fn one t0 t1

let ( // ) t0 t1 =
  (to_real t0) /. (to_real t1)

let bit_pop = U64.bit_pop

let bit_clz = U64.bit_clz

let bit_ctz = U64.bit_ctz

module U = struct
  type nonrec t = t

  let bit_length = 64

  let cmp = cmp
  let zero = zero
  let one = one
  let of_uns = of_uns
  let ( + ) = ( + )
  let ( - ) = ( - )
  let bit_and = bit_and
  let bit_sl = bit_sl
  let bit_clz = bit_clz
end
include Intnb.MakeDerived(U)

let abs t =
  Int64.abs t

let neg t =
  Int64.neg t

let ( ~+ ) t =
  t

let ( ~- ) t =
  neg t

let neg_one = Int64.minus_one

let min_sint = neg (bit_sl ~shift:62 one)
let max_sint = pred (bit_sl ~shift:62 one)

let to_sint t =
  Sint.of_int (Int64.to_int t)

let to_sint_opt t =
  match t < min_sint || t > max_sint with
  | false -> Some (to_sint t)
  | true -> None

let to_sint_hlt t =
  match to_sint_opt t with
  | Some x -> x
  | None -> halt "Lossy conversion"

let of_sint x =
  Int64.of_int (Uns.of_sint x)
