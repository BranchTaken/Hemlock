open Rudiments_functions
open Intnw_intf
module Uns = U63

external intnw_icmp: int64 array -> int64 array -> Cmp.t = "hemlock_intnw_icmp"
external intnw_ucmp: int64 array -> int64 array -> Cmp.t = "hemlock_intnw_ucmp"

external intnw_bit_and: int64 array -> int64 array -> int64 array =
  "hemlock_intnw_bit_and"
external intnw_bit_or: int64 array -> int64 array -> int64 array =
  "hemlock_intnw_bit_or"
external intnw_bit_xor: int64 array -> int64 array -> int64 array =
  "hemlock_intnw_bit_xor"
external intnw_bit_not: int64 array -> int64 array = "hemlock_intnw_bit_not"
external intnw_bit_sl: uns -> int64 array -> int64 array =
  "hemlock_intnw_bit_sl"
external intnw_bit_usr: uns -> int64 array -> int64 array =
  "hemlock_intnw_bit_usr"
external intnw_bit_ssr: uns -> int64 array -> int64 array =
  "hemlock_intnw_bit_ssr"
external intnw_bit_pop: int64 array -> uns = "hemlock_intnw_bit_pop"
external intnw_bit_clz: int64 array -> uns = "hemlock_intnw_bit_clz"
external intnw_bit_ctz: int64 array -> uns = "hemlock_intnw_bit_ctz"

external intnw_add: int64 array -> int64 array -> int64 array =
  "hemlock_intnw_add"
external intnw_sub: int64 array -> int64 array -> int64 array =
  "hemlock_intnw_sub"
external intnw_mul: int64 array -> int64 array -> int64 array =
  "hemlock_intnw_mul"
external intnw_udiv: int64 array -> int64 array -> int64 array
  = "hemlock_intnw_udiv"
external intnw_idiv: int64 array -> int64 array -> int64 array
  = "hemlock_intnw_idiv"
external intnw_umod: int64 array -> int64 array -> int64 array
  = "hemlock_intnw_umod"
external intnw_imod: int64 array -> int64 array -> int64 array
  = "hemlock_intnw_imod"
external intnw_neg: int64 array -> int64 array = "hemlock_intnw_neg"
external intnw_abs: int64 array -> int64 array = "hemlock_intnw_abs"

external intnw_i_of_float: uns -> float -> int64 array =
  "hemlock_intnw_i_of_float"
external intnw_u_of_float: uns -> float -> int64 array =
  "hemlock_intnw_u_of_float"
external intnw_i_to_float: int64 array -> float = "hemlock_intnw_i_to_float"
external intnw_u_to_float: int64 array -> float = "hemlock_intnw_u_to_float"

module type I_common = sig
  include I
  val signed: bool
end

module type S_common = sig
  type t

  include S with type t := t
  include S_signed with type t := t
end

module Make_common (T : I_common) : S_common with type t := T.t = struct
  module U = struct
    type t = T.t

    let of_arr = T.of_arr

    let to_arr = T.to_arr

    let hash_fold t state =
      assert (T.num_bits mod 128 = 0);
      let t_arr = to_arr t in
      Hash.State.Gen.init state
      |> Hash.State.Gen.fold_u128 (T.num_bits / 128) ~f:(fun i ->
        u128_of_arr [|Array.get (i / 2) t_arr; Array.get (i / 2 + 1) t_arr|])
      |> Hash.State.Gen.fini

    let cmp t0 t1 =
      match T.signed with
      | true -> intnw_icmp (to_arr t0) (to_arr t1)
      | false -> intnw_ucmp (to_arr t0) (to_arr t1)

    let zero = of_arr (Array.init (T.num_bits / 64) ~f:(fun _ -> Int64.zero))

    let one =
      of_arr (Array.init (T.num_bits / 64) ~f:(fun i -> match i with
        | 0 -> Int64.one
        | _ -> Int64.zero
      ))

    let neg_one =
      of_arr (Array.init (T.num_bits / 64) ~f:(fun _ -> Int64.minus_one))

    let is_neg t =
      let t_arr = to_arr t in
      T.signed && (Int64.compare (Array.get (pred (Array.length t_arr)) t_arr)
        Int64.zero) < 0


    let to_u64 t =
      Array.get 0 (to_arr t)

    let to_u64_hlt t =
      let t_arr = to_arr t in
      for i = 1 to Uns.pred (Array.length t_arr) do
        if U64.((Array.get i t_arr) <> zero) then
          halt "Lossy conversion";
      done;
      to_u64 t

    let of_u64 u =
      of_arr (Array.init (T.num_bits / 64) ~f:(fun i -> match i with
        | 0 -> u
        | _ -> Int64.zero
      ))

    let to_uns t =
      U64.to_uns (to_u64 t)

    let to_uns_hlt t =
      U64.to_uns_hlt (to_u64_hlt t)

    let of_uns u =
      of_u64 (U64.of_uns u)

    let bit_and t0 t1 =
      of_arr (intnw_bit_and (to_arr t0) (to_arr t1))

    let bit_or t0 t1 =
      of_arr (intnw_bit_or (to_arr t0) (to_arr t1))

    let bit_xor t0 t1 =
      of_arr (intnw_bit_xor (to_arr t0) (to_arr t1))

    let bit_not t =
      of_arr (intnw_bit_not (to_arr t))

    let bit_sl ~shift t =
      of_arr (intnw_bit_sl shift (to_arr t))

    let bit_usr ~shift t =
      of_arr (intnw_bit_usr shift (to_arr t))

    let bit_ssr ~shift t =
      of_arr (intnw_bit_ssr shift (to_arr t))

    let bit_pop t =
      intnw_bit_pop (to_arr t)

    let bit_clz t =
      intnw_bit_clz (to_arr t)

    let bit_ctz t =
      intnw_bit_ctz (to_arr t)

    let ( + ) t0 t1 =
      of_arr (intnw_add (to_arr t0) (to_arr t1))

    let ( - ) t0 t1 =
      of_arr (intnw_sub (to_arr t0) (to_arr t1))

    let ( * ) t0 t1 =
      of_arr (intnw_mul (to_arr t0) (to_arr t1))

    let ( / ) t0 t1 =
      of_arr (
        match t1 = zero, T.signed with
        | true, _ -> halt "Division by 0"
        | false, true -> intnw_idiv (to_arr t0) (to_arr t1)
        | false, false -> intnw_udiv (to_arr t0) (to_arr t1)
      )

    let ( % ) t0 t1 =
      of_arr (
        match t1 = zero, T.signed with
        | true, _ -> halt "Division by 0"
        | false, true -> intnw_imod (to_arr t0) (to_arr t1)
        | false, false -> intnw_umod (to_arr t0) (to_arr t1)
      )

    let ( ** ) t0 t1 =
      (* Decompose the exponent to limit algorithmic complexity. *)
      let neg, n = if T.signed && t1 < zero then
          true, of_arr (intnw_neg (to_arr t1))
        else
          false, t1
      in
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
      let r = fn one t0 n in
      match neg with
      | false -> r
      | true -> one / r

    let max_value =
      match T.signed with
      | true -> bit_usr ~shift:1 (bit_not zero)
      | false -> bit_not zero

    let min_value =
      match T.signed with
      | true -> bit_sl ~shift:Uns.(T.num_bits - 1) one
      | false -> zero

    let succ t =
      t + one

    let pred t =
      t - one

    let ( ~- ) t =
      of_arr (intnw_neg (to_arr t))

    let ( ~+) t =
      t

    let neg t =
      of_arr (intnw_neg (to_arr t))

    let abs t =
      match T.signed with
      | true -> of_arr (intnw_abs (to_arr t))
      | false -> t

    let of_float f =
      match Float.is_nan f with
      | true -> halt "Not a number"
      | false -> begin
          of_arr (
            match T.signed with
            | true -> intnw_i_of_float Uns.(T.num_bits / 64) f
            | false -> intnw_u_of_float Uns.(T.num_bits / 64) f
          )
        end

    let to_float t =
      match T.signed with
      | true -> intnw_i_to_float (to_arr t)
      | false -> intnw_u_to_float (to_arr t)

    let ( // ) t0 t1 =
      (to_float t0) /. (to_float t1)

    let pp ppf t =
      let ten = of_uns 10 in
      let rec fn t i = begin
        match cmp t zero with
        | Cmp.Eq -> ()
        | Cmp.Lt | Cmp.Gt -> begin
            let t' = t / ten in
            let () = fn t' (Uns.succ i) in
            let digit = Array.get 0 (to_arr (match is_neg t with
              | true -> neg (t % ten)
              | false -> t % ten
            )) in
            Format.fprintf ppf "%Lu" digit;
            if Uns.(i % 3 = 0) && Uns.(i > 0) then
              Format.fprintf ppf "_";
            ()
          end
      end in
      match Cmp.is_eq (cmp t zero) with
      | true -> Format.fprintf ppf "0%s%u"
          (if T.signed then "i" else "u") T.num_bits
      | false -> begin
          let () = Format.fprintf ppf "%s" (if is_neg t then "-" else "") in
          let _ = fn t 0 in
          Format.fprintf ppf "%s%u"
            (if T.signed then "i" else "u")
            T.num_bits
        end

    let pp_x ppf t =
      let rec fn x shift = begin
        match shift with
        | 0 -> ()
        | _ -> begin
            if Uns.(shift < 64) then Format.fprintf ppf "_";
            let shift' = Uns.(shift - 16) in
            Format.fprintf ppf "%04Lx"
              Int64.(logand (shift_right_logical x shift') (of_int 0xffff));
            fn x shift'
          end
      end in
      Format.fprintf ppf "0x";
      let t_arr = to_arr t in
      for i = Uns.pred (Array.length t_arr) downto 0 do
        let elm = Array.get i t_arr in
        if Uns.(i < pred (Array.length t_arr)) then Format.fprintf ppf "_";
        fn elm 64
      done;
      Format.fprintf ppf "%s%u" (if T.signed then "i" else "u") T.num_bits

    let of_string s =
      let getc_opt s i len = begin
        match i < len with
        | false -> None
        | true -> Some ((Caml.String.get s i), Uns.(i + 1))
      end in
      let getc s i len = begin
        match getc_opt s i len with
        | None -> halt "Malformed string"
        | Some (c, i') -> c, i'
      end in
      let d_of_c c = begin
        match c with
        | '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9' ->
          of_uns Uns.((Caml.Char.code c) - (Caml.Char.code '0'))
        | 'a' | 'b' | 'c' | 'd' | 'e' | 'f' ->
          of_uns Uns.(10 + (Caml.Char.code c) - (Caml.Char.code 'a'))
        | _ -> not_reached ()
      end in
      let suffix s i len = begin
        let rec fn s i j len = begin
          let c, j' = getc s j len in
          match c with
          | '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9' -> begin
              match j' < len with
              | true -> fn s i j' len
              | false -> begin
                  let suf = Caml.String.sub s i Rudiments_int.(j' - i) in
                  let nbits = int_of_string suf in
                  match nbits = T.num_bits with
                  | false -> halt "Malformed string"
                  | true -> j'
                end
            end
          | _ -> halt "Malformed string"
        end in
        fn s i i len
      end in
      let rec hexadecimal s i ndigits len = begin
        match getc_opt s i len with
        | None -> begin
            match ndigits with
            | 0 -> halt "Malformed string" (* "0x" *)
            | _ -> zero, one
          end
        | Some (c, i') -> begin
            match c with
            | '_' -> hexadecimal s i' ndigits len
            | '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9'
            | 'a' | 'b' | 'c' | 'd' | 'e' | 'f' -> begin
                let ndigits' = Uns.succ ndigits in
                let accum, mult = hexadecimal s i' ndigits' len in
                let accum' = accum + mult * (d_of_c c) in
                let mult' = mult * (of_uns 16) in
                accum', mult'
              end
            | 'i' | 'u' -> begin
                match (T.signed && c = 'i') || ((not T.signed) && c = 'u') with
                | false -> halt "Malformed string"
                | true -> begin
                    let i'' = suffix s i' len in
                    hexadecimal s i'' ndigits len
                  end
              end
            | _ -> halt "Malformed string"
          end
      end in
      let rec decimal s i len = begin
        match getc_opt s i len with
        | None -> zero, one
        | Some (c, i') -> begin
            match c with
            | '_' -> decimal s i' len
            | '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9' -> begin
                let accum, mult = decimal s i' len in
                let accum' = accum + mult * (d_of_c c) in
                let mult' = mult * (of_uns 10) in
                accum', mult'
              end
            | 'i' | 'u' -> begin
                match (T.signed && c = 'i') || ((not T.signed) && c = 'u') with
                | false -> halt "Malformed string"
                | true -> begin
                    let i'' = suffix s i' len in
                    decimal s i'' len
                  end
              end
            | _ -> halt "Malformed string"
          end
      end in
      let rec binary s i ndigits len = begin
        match getc_opt s i len with
        | None -> begin
            match ndigits with
            | 0 -> halt "Malformed string" (* "0b" *)
            | _ -> zero, one
          end
        | Some (c, i') -> begin
            match c with
            | '_' -> binary s i' ndigits len
            | '0' | '1' -> begin
                let ndigits' = Uns.succ ndigits in
                let accum, mult = binary s i' ndigits' len in
                let accum' = accum + mult * (d_of_c c) in
                let mult' = mult * (of_uns 2) in
                accum', mult'
              end
            | 'i' | 'u' -> begin
                match (T.signed && c = 'i') || ((not T.signed) && c = 'u') with
                | false -> halt "Malformed string"
                | true -> begin
                    let i'' = suffix s i' len in
                    binary s i'' ndigits len
                  end
              end
            | _ -> halt "Malformed string"
          end
      end in
      let prefix1 s i len = begin
        match getc_opt s i len with
        | None -> zero
        | Some (c, i') -> begin
            match c with
            | 'b' -> begin
                let accum, _ = binary s i' 0 len in
                accum
              end
            | 'x' -> begin
                let accum, _ = hexadecimal s i' 0 len in
                accum
              end
            | '_' -> begin
                let accum, _ = decimal s i' len in
                accum
              end
            | '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9' ->
              let accum, mult = decimal s i' len in
              accum + mult * (d_of_c c)
            | 'i' | 'u' -> begin
                match (T.signed && c = 'i') || ((not T.signed) && c = 'u') with
                | false -> halt "Malformed string"
                | true -> begin
                    let _ = suffix s i' len in
                    zero
                  end
              end
            | _ -> halt "Malformed string"
          end
      end in
      let prefix0 s i len = begin
        let c, i' = getc s i len in
        match c with
        | '0' -> prefix1 s i' len
        | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9' -> begin
            let accum, mult = decimal s i' len in
            accum + mult * (d_of_c c)
          end
        | '+' -> begin
            match T.signed with
            | true -> let accum, _ = decimal s i' len in accum
            | false -> halt "Malformed string"
          end
        | '-' -> begin
            match T.signed with
            | true -> let accum, _ = decimal s i' len in neg accum
            | false -> halt "Malformed string"
          end
        | _ -> halt "Malformed string"
      end in
      prefix0 s 0 (Caml.String.length s)

    let to_string t =
      Format.asprintf "%a" pp t
  end
  include U
  include Identifiable.Make(U)
  include Cmpable.Make_zero(U)
  module V = struct
    include U

    let num_bits = T.num_bits
  end
  include Intnb.Make_derived(V)
end

module Make_i (T : I) : S_i with type t := T.t = struct
  module U = struct
    include T
    let signed = true
  end
  include U
  include Make_common(U)

end

module Make_u (T : I) : S_u with type t := T.t = struct
  module U = struct
    include T
    let signed = false
  end
  include U
  include Make_common(U)
end
