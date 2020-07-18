open Rudiments_functions
open Intnw_intf
module Uns = U63
module Sint = I63

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
external intnw_div_mod: int64 array -> int64 array -> int64 array * int64 array
  = "hemlock_intnw_div_mod"
external intnw_neg: int64 array -> int64 array = "hemlock_intnw_neg"
external intnw_abs: int64 array -> int64 array = "hemlock_intnw_abs"

module type I_common = sig
  include I
  val signed: bool
end

module type S_common = sig
  type t

  include S with type t := t
  include S_signed with type t := t
end

module Make_common (T : I_common) : S_common = struct
  module U = struct
    type t = int64 array

    let hash_fold t state =
      assert (T.num_bits mod 128 = 0);
      Hash.State.Gen.init state
      |> Hash.State.Gen.fold_u128 (T.num_bits / 128)
        ~f:(fun i -> {hi=Array.get (i / 2 + 1) t; lo=Array.get (i / 2) t})
      |> Hash.State.Gen.fini

    let cmp t0 t1 =
      match T.signed with
      | true -> intnw_icmp t0 t1
      | false -> intnw_ucmp t0 t1

    let zero = Array.init (T.num_bits / 64) ~f:(fun _ -> Int64.zero)

    let one =
      Array.init (T.num_bits / 64) ~f:(fun i -> match i with
        | 0 -> Int64.one
        | _ -> Int64.zero
      )

    let neg_one = Array.init (T.num_bits / 64) ~f:(fun _ -> Int64.minus_one)

(*XXX
    let to_uns t =
      U64.to_uns (Array.get 0 t)
*)

    let is_neg t =
      T.signed &&
        (Int64.compare (Array.get (pred (Array.length t)) t) Int64.zero) < 0

(*XXX
    let to_uns_hlt t =
      let rec fn i t = begin
        match i with
        | 0 -> U64.to_uns_hlt (Array.get 0 t)
        | _ -> begin
            match Int64.compare (Array.get i t) Int64.zero with
            | 0 -> fn (pred i) t
            | _ -> halt "Lossy conversion"
          end
      end in
      match is_neg t with
      | true -> halt "Lossy conversion"
      | false -> fn (pred (Array.length t)) t
*)

    let of_uns u =
      Array.init (T.num_bits / 64) ~f:(fun i -> match i with
        | 0 -> Int64.of_int u
        | _ -> Int64.zero
      )

    let bit_and t0 t1 =
      intnw_bit_and t0 t1

    let bit_or t0 t1 =
      intnw_bit_or t0 t1

    let bit_xor t0 t1 =
      intnw_bit_xor t0 t1

    let bit_not t =
      intnw_bit_not t

    let bit_sl ~shift t =
      intnw_bit_sl shift t

    let bit_usr ~shift t =
      intnw_bit_usr shift t

    let bit_ssr ~shift t =
      intnw_bit_ssr shift t

    let bit_pop t =
      intnw_bit_pop t

    let bit_clz t =
      intnw_bit_clz t

    let bit_ctz t =
      intnw_bit_ctz t

    let ( + ) t0 t1 =
      intnw_add t0 t1

    let ( - ) t0 t1 =
      intnw_sub t0 t1

    let ( * ) t0 t1 =
      intnw_mul t0 t1

    let ( / ) t0 t1 =
      let quotient, _ = intnw_div_mod t0 t1 in
      quotient

    let ( % ) t0 t1 =
      let _, remainder = intnw_div_mod t0 t1 in
      remainder

    let ( ** ) t0 t1 =
      (* Decompose the exponent to limit algorithmic complexity. *)
      let neg, n = if T.signed && t1 < zero then
          true, intnw_neg t1
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
      | true -> zero - max_value
      | false -> zero

    let succ t =
      t + one

    let pred t =
      t - one

    let ( ~- ) t =
      intnw_neg t

    let ( ~+) t =
      t

    let neg t =
      intnw_neg t

    let abs t =
      intnw_abs t

    let of_float f =
      let frac, exp = Caml.Float.frexp f in
      let sexp = Uns.to_sint exp in
      let x = Caml.Int64.of_float frac in
      let x' =
        match T.signed && Int64.(compare x zero) < 0 with
        | true -> begin
            Array.init Uns.(T.num_bits / 64) ~f:(fun i -> match i with
              | 0 -> x
              | _ -> Int64.zero
            )
          end
        | false -> begin
            Array.init Uns.(T.num_bits / 64) ~f:(fun i -> match i with
              | 0 -> x
              | _ -> Int64.minus_one
            )
          end
      in
      if Sint.(sexp <= -(Uns.to_sint T.num_bits)) then
        zero
      else if Sint.(sexp <= kv 0) then begin
        match T.signed with
        | true -> bit_ssr ~shift:T.num_bits x'
        | false -> bit_usr ~shift:T.num_bits x'
      end else if Sint.(sexp < (Uns.to_sint T.num_bits)) then
        bit_sl ~shift:T.num_bits x'
      else begin
        match T.signed && is_neg x' with
        | true -> min_value
        | false -> max_value
      end

    let to_float t =
      match T.signed with
      | true -> begin
          let sig_bits = Uns.( - ) T.num_bits (bit_clz t) in
          match sig_bits <= 63 with
          | true -> Int64.to_float (Array.get 0 t)
          | false -> begin
              (* Preserve 53 bits of accuracy. *)
              let exp = Uns.(sig_bits - 53) in
              let frac = Array.get 0 (bit_usr ~shift:exp t) in
              Caml.Float.ldexp (Int64.to_float frac) exp
            end
        end
      | false -> begin
          let sig_bits = match t >= zero with
            | true -> Uns.( - ) T.num_bits (bit_clz t)
            | false -> Uns.( - ) T.num_bits (bit_clz (bit_not t))
          in
          match sig_bits <= 64 with
          | true -> Int64.to_float (Array.get 0 t)
          | false -> begin
              (* Preserve 53 bits of accuracy. *)
              let exp = Uns.(sig_bits - 53) in
              let frac = Array.get 0 (bit_ssr ~shift:exp t) in
              Caml.Float.ldexp (Int64.to_float frac) exp
            end
        end

    let ( // ) t0 t1 =
      (to_float t0) /. (to_float t1)

    let pp ppf t =
      let ten = of_uns 10 in
      let rec fn t = begin
        match cmp t zero with
        | Cmp.Eq -> 0
        | Cmp.Lt | Cmp.Gt -> begin
            let t' = t / (of_uns 10) in
            let i = fn t' in
            let digit = Array.get 0 (match is_neg t with
              | true -> neg ((t % ten) - ten)
              | false -> t % ten
            ) in
            if Rudiments_int.(i % 3 = 0) && Rudiments_int.(i > 0) then
              Format.fprintf ppf "_";
            Format.fprintf ppf "%Lu" digit;
            Rudiments_int.(succ i)
          end
      end in
      match Cmp.is_eq (cmp t zero) with
      | true -> Format.fprintf ppf "0%s%u"
          (if T.signed then "u" else "i") T.num_bits
      | false -> begin
          let () = Format.fprintf ppf "%s" (if is_neg t then "-" else "") in
          let _ = fn t in
          Format.fprintf ppf "%s%u"
            (if T.signed then "u" else "i")
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
      Array.iteri t ~f:(fun i elm ->
        if Uns.(i < pred (Array.length t)) then Format.fprintf ppf "_";
        fn elm 64
      );
      Format.fprintf ppf "%s%u" (if T.signed then "u" else "i") T.num_bits

    (* XXX only handles u128 *)
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
      let rec suffix s i j len = begin
        let c, i' = getc s i len in
        match c, j with
        | '1', 1
        | '2', 2 -> suffix s i' (Uns.succ j) len
        | '8', 3 -> begin
            match i' < len with
            | true -> halt "Malformed string"
            | false -> i'
          end
        | _ -> halt "Malformed string"
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
            (* XXX 'i' *)
            | 'u' -> begin
                let i'' = suffix s i' 1 len in
                hexadecimal s i'' ndigits len
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
            (* XXX 'i' *)
            | 'u' -> begin
                let i'' = suffix s i' 1 len in
                decimal s i'' len
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
            (* XXX 'i' *)
            | 'u' -> begin
                let i'' = suffix s i' 1 len in
                binary s i'' ndigits len
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
            (* XXX 'i' *)
            | 'u' -> begin
                let _ = suffix s i' 1 len in
                zero
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
        | _ -> halt "Malformed string"
      end in
      (* XXX '+'/'-' sign. *)
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

module Make_i (T : I) : S_i = struct
  module U = struct
    let num_bits = T.num_bits
    let signed = true
  end
  include U
  include Make_common(U)

end

module Make_u (T : I) : S_u = struct
  module U = struct
    let num_bits = T.num_bits
    let signed = false
  end
  include U
  include Make_common(U)
end
