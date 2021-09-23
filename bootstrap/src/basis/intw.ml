open RudimentsFunctions
open IntwIntf
type real = float

module type IVCommon = sig
  include IV
  val signed: bool
end

module type SVCommon = sig
  type t

  include SVCommon with type t := t
  include SSigned with type t := t
end

module MakeVCommon (T : IVCommon) : SVCommon with type t := T.t = struct
  module U = struct
    type t = T.t
    type word_length_cb = t -> uns
    type get_cb = uns -> t -> int64

    let min_word_length = T.min_word_length

    let max_word_length = T.max_word_length

    let word_length = T.word_length

    let init = T.init

    let get = T.get

    let bit_length t =
      (T.word_length t) * 64

    let of_arr a =
      init (Array.length a) ~f:(fun i -> Array.get i a)

    let hash_fold t state =
      Hash.State.Gen.init state
      |> Hash.State.Gen.fold_u64 (word_length t) ~f:(fun i -> get i t)
      |> Hash.State.Gen.fini

    external intw_icmp: t -> t -> word_length_cb -> get_cb -> Cmp.t = "hm_basis_intw_i_cmp"
    external intw_ucmp: t -> t -> word_length_cb -> get_cb -> Cmp.t = "hm_basis_intw_u_cmp"

    let cmp t0 t1 =
      match T.signed with
      | true -> intw_icmp t0 t1 word_length get
      | false -> intw_ucmp t0 t1 word_length get

    let zero =
      init T.min_word_length ~f:(fun _ -> Int64.zero)

    let one =
      init (Uns.max T.min_word_length 1) ~f:(fun i -> match i with
        | 0 -> Int64.one
        | _ -> Int64.zero
      )

    let neg_one =
      init (Uns.max T.min_word_length 1) ~f:(fun _ -> Int64.minus_one)

    let is_neg t =
      T.signed && (Int64.compare (get (pred (word_length t)) t) Int64.zero) < 0

    let to_u64 t =
      match word_length t with
      | 0 -> U64.zero
      | _ -> get 0 t

    let to_u64_opt t =
      let rec fn i t = begin
        match Uns.(i < (word_length t)) with
        | false -> false
        | true -> begin
            match U64.((get i t) <> zero) with
            | true -> true
            | false -> fn (Uns.succ i) t
          end
      end in
      match fn 1 t with
      | true -> None
      | false -> Some (to_u64 t)

    let to_u64_hlt t =
      match to_u64_opt t with
      | None -> halt "Lossy conversion"
      | Some u -> u

    let of_u64 u =
      match U64.(u = zero), T.min_word_length with
      | true, 0 -> init 0 ~f:(fun _ -> Int64.zero)
      | _ -> begin
          let n = match T.signed && U64.(u >= 0x8000_0000_0000_0000L) with
            | true -> 2
            | false -> 1
          in
          init (Uns.max T.min_word_length n) ~f:(fun i -> match i with
            | 0 -> u
            | _ -> Int64.zero
          )
        end

    let to_uns t =
      U64.to_uns (to_u64 t)

    let to_uns_opt t =
      match to_u64_opt t with
      | None -> None
      | Some u64 -> U64.to_uns_opt u64

    let to_uns_hlt t =
      U64.to_uns_hlt (to_u64_hlt t)

    let of_uns u =
      of_u64 (U64.of_uns u)

    external intw_ubit_and: t -> t -> word_length_cb -> get_cb -> uns -> uns -> int64 array =
      "hm_basis_intw_u_bit_and_bytecode" "hm_basis_intw_u_bit_and_native"
    external intw_ibit_and: t -> t -> word_length_cb -> get_cb -> uns -> uns -> int64 array =
      "hm_basis_intw_i_bit_and_bytecode" "hm_basis_intw_i_bit_and_native"

    let bit_and t0 t1 =
      of_arr (
        match T.signed with
        | true -> intw_ibit_and t0 t1 word_length get T.min_word_length T.max_word_length
        | false -> intw_ubit_and t0 t1 word_length get T.min_word_length T.max_word_length
      )

    external intw_ubit_or: t -> t -> word_length_cb -> get_cb -> uns -> uns -> int64 array =
      "hm_basis_intw_u_bit_or_bytecode" "hm_basis_intw_u_bit_or_native"
    external intw_ibit_or: t -> t -> word_length_cb -> get_cb -> uns -> uns -> int64 array =
      "hm_basis_intw_i_bit_or_bytecode" "hm_basis_intw_i_bit_or_native"
    let bit_or t0 t1 =
      of_arr (
        match T.signed with
        | true -> intw_ibit_or t0 t1 word_length get T.min_word_length T.max_word_length
        | false-> intw_ubit_or t0 t1 word_length get T.min_word_length T.max_word_length
      )

    external intw_ubit_xor: t -> t -> word_length_cb -> get_cb -> uns -> uns -> int64 array =
      "hm_basis_intw_u_bit_xor_bytecode" "hm_basis_intw_u_bit_xor_native"
    external intw_ibit_xor: t -> t -> word_length_cb -> get_cb -> uns -> uns -> int64 array =
      "hm_basis_intw_i_bit_xor_bytecode" "hm_basis_intw_i_bit_xor_native"

    let bit_xor t0 t1 =
      of_arr (
        match T.signed with
        | true -> intw_ibit_xor t0 t1 word_length get T.min_word_length T.max_word_length
        | false -> intw_ubit_xor t0 t1 word_length get T.min_word_length T.max_word_length
      )

    external intw_bit_unot: t -> word_length_cb -> get_cb -> uns -> uns -> int64 array =
      "hm_basis_intw_u_bit_not"
    external intw_bit_inot: t -> word_length_cb -> get_cb -> uns -> uns -> int64 array =
      "hm_basis_intw_i_bit_not"

    let bit_not t =
      of_arr (
        match T.signed with
        | true -> intw_bit_inot t word_length get T.min_word_length T.max_word_length
        | false -> intw_bit_unot t word_length get T.min_word_length T.max_word_length
      )

    external intw_bit_usl: uns -> t -> word_length_cb -> get_cb -> uns -> uns -> int64 array =
      "hm_basis_intw_u_bit_sl_bytecode" "hm_basis_intw_u_bit_sl_native"
    external intw_bit_isl: uns -> t -> word_length_cb -> get_cb -> uns -> uns -> int64 array =
      "hm_basis_intw_i_bit_sl_bytecode" "hm_basis_intw_i_bit_sl_native"

    let bit_sl ~shift t =
      of_arr (
        match T.signed with
        | true -> intw_bit_isl shift t word_length get T.min_word_length T.max_word_length
        | false -> intw_bit_usl shift t word_length get T.min_word_length T.max_word_length
      )

    external intw_bit_usr: uns -> t -> word_length_cb -> get_cb -> uns -> uns -> int64 array =
      "hm_basis_intw_bit_usr_bytecode" "hm_basis_intw_bit_usr_native"

    let bit_usr ~shift t =
      of_arr (intw_bit_usr shift t word_length get T.min_word_length T.max_word_length)

    external intw_bit_ssr: uns -> t -> word_length_cb -> get_cb -> uns -> uns -> int64 array =
      "hm_basis_intw_bit_ssr_bytecode" "hm_basis_intw_bit_ssr_native"

    let bit_ssr ~shift t =
      of_arr (intw_bit_ssr shift t word_length get T.min_word_length T.max_word_length)

    external intw_bit_pop: t -> word_length_cb -> get_cb -> uns = "hm_basis_intw_bit_pop"

    let bit_pop t =
      intw_bit_pop t word_length get

    external intw_bit_clz: t -> word_length_cb -> get_cb -> uns = "hm_basis_intw_bit_clz"

    let bit_clz t =
      intw_bit_clz t word_length get

    external intw_bit_ctz: t -> word_length_cb -> get_cb -> uns = "hm_basis_intw_bit_ctz"

    let bit_ctz t =
      intw_bit_ctz t word_length get

    external intw_uadd: t -> t -> word_length_cb -> get_cb -> uns -> uns -> int64 array =
      "hm_basis_intw_u_add_bytecode" "hm_basis_intw_u_add_native"
    external intw_iadd: t -> t -> word_length_cb -> get_cb -> uns -> uns -> int64 array =
      "hm_basis_intw_i_add_bytecode" "hm_basis_intw_i_add_native"

    let ( + ) t0 t1 =
      of_arr (
        match T.signed with
        | true -> intw_iadd t0 t1 word_length get T.min_word_length T.max_word_length
        | false -> intw_uadd t0 t1 word_length get T.min_word_length T.max_word_length
      )

    external intw_usub: t -> t -> word_length_cb -> get_cb -> uns -> uns -> int64 array =
      "hm_basis_intw_u_sub_bytecode" "hm_basis_intw_u_sub_native"
    external intw_isub: t -> t -> word_length_cb -> get_cb -> uns -> uns -> int64 array =
      "hm_basis_intw_i_sub_bytecode" "hm_basis_intw_i_sub_native"

    let ( - ) t0 t1 =
      of_arr (
        match T.signed with
        | true -> intw_isub t0 t1 word_length get T.min_word_length T.max_word_length
        | false -> intw_usub t0 t1 word_length get T.min_word_length T.max_word_length
      )

    external intw_umul: t -> t -> word_length_cb -> get_cb -> uns -> uns -> int64 array =
      "hm_basis_intw_u_mul_bytecode" "hm_basis_intw_u_mul_native"
    external intw_imul: t -> t -> word_length_cb -> get_cb -> uns -> uns -> int64 array =
      "hm_basis_intw_i_mul_bytecode" "hm_basis_intw_i_mul_native"

    let ( * ) t0 t1 =
      of_arr (
        match T.signed with
        | true -> intw_imul t0 t1 word_length get T.min_word_length T.max_word_length
        | false -> intw_umul t0 t1 word_length get T.min_word_length T.max_word_length
      )

    external intw_udiv: t -> t -> word_length_cb -> get_cb -> uns -> uns -> int64 array =
      "hm_basis_intw_u_div_bytecode" "hm_basis_intw_u_div_native"
    external intw_idiv: t -> t -> word_length_cb -> get_cb -> uns -> uns -> int64 array =
      "hm_basis_intw_i_div_bytecode" "hm_basis_intw_i_div_native"

    let ( / ) t0 t1 =
      of_arr (
        match t1 = zero, T.signed with
        | true, _ -> halt "Division by 0"
        | false, true -> intw_idiv t0 t1 word_length get T.min_word_length T.max_word_length
        | false, false -> intw_udiv t0 t1 word_length get T.min_word_length T.max_word_length
      )

    external intw_umod: t -> t -> word_length_cb -> get_cb -> uns -> uns -> int64 array =
      "hm_basis_intw_u_mod_bytecode" "hm_basis_intw_u_mod_native"
    external intw_imod: t -> t -> word_length_cb -> get_cb -> uns -> uns -> int64 array =
      "hm_basis_intw_i_mod_bytecode" "hm_basis_intw_i_mod_native"

    let ( % ) t0 t1 =
      of_arr (
        match t1 = zero, T.signed with
        | true, _ -> halt "Division by 0"
        | false, true -> intw_imod t0 t1 word_length get T.min_word_length T.max_word_length
        | false, false -> intw_umod t0 t1 word_length get T.min_word_length T.max_word_length
      )

    external intw_neg: t -> word_length_cb -> get_cb -> uns -> uns -> int64 array =
      "hm_basis_intw_i_neg"

    let ( ~- ) t =
      of_arr (intw_neg t word_length get T.min_word_length T.max_word_length)

    let ( ~+ ) t =
      t

    let neg = ( ~- )

    external intw_abs: t -> word_length_cb -> get_cb -> uns -> uns -> int64 array =
      "hm_basis_intw_i_abs"

    let abs t =
      match T.signed with
      | true -> of_arr (intw_abs t word_length get T.min_word_length T.max_word_length)
      | false -> t

    let ( ** ) t0 t1 =
      (* Decompose the exponent to limit algorithmic complexity. *)
      let neg, n = if T.signed && t1 < zero then
          true, ~-t1
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

    let succ t =
      t + one

    let pred t =
      t - one

    let to_i64 t =
      assert T.signed;
      match word_length t with
      | 0 -> I64.zero
      | _ -> get 0 t

    let min_i64 = bit_sl ~shift:63 one
    let max_i64 = pred (bit_sl ~shift:63 one)

    let to_i64_opt t =
      assert T.signed;
      match t < min_i64 || t > max_i64 with
      | false -> Some (to_i64 t)
      | true -> None

    let to_i64_hlt t =
      assert T.signed;
      match to_i64_opt t with
      | None -> halt "Lossy conversion"
      | Some u -> u

    let of_i64 u =
      assert T.signed;
      match I64.(u = zero), T.min_word_length with
      | true, 0 -> init 0 ~f:(fun _ -> Int64.zero)
      | _ -> begin
          init (Uns.max T.min_word_length 1) ~f:(fun i -> match i with
            | 0 -> u
            | _ -> Int64.zero
          )
        end

    let to_sint t =
      assert T.signed;
      I64.to_sint (to_i64 t)

    let to_sint_opt t =
      assert T.signed;
      match to_i64_opt t with
      | None -> None
      | Some i64 -> I64.to_sint_opt i64

    let to_sint_hlt t =
      assert T.signed;
      I64.to_sint_hlt (to_i64_hlt t)

    let of_sint u =
      assert T.signed;
      of_i64 (I64.of_sint u)

    external intw_i_of_real: real -> uns -> uns -> int64 array = "hm_basis_intw_i_of_real"
    external intw_u_of_real: real -> uns -> uns -> int64 array = "hm_basis_intw_u_of_real"

    let of_real r =
      match Real.is_nan r with
      | true -> halt "Not a number"
      | false -> begin
          of_arr (
            match T.signed with
            | true -> intw_i_of_real r T.min_word_length T.max_word_length
            | false -> intw_u_of_real r T.min_word_length T.max_word_length
          )
        end

    external intw_i_to_real: t -> word_length_cb -> get_cb -> real = "hm_basis_intw_i_to_real"
    external intw_u_to_real: t -> word_length_cb -> get_cb -> real = "hm_basis_intw_u_to_real"

    let to_real t =
      match T.signed with
      | true -> intw_i_to_real t word_length get
      | false -> intw_u_to_real t word_length get

    let ( // ) t0 t1 =
      (to_real t0) /. (to_real t1)

    let pp_suffix ppf t =
      match Uns.(T.min_word_length = T.max_word_length) with
      | false -> ()
      | true -> begin
          Format.fprintf ppf "%s%u"
            (if T.signed then "i" else "u")
            Uns.(T.word_length t * 64)
        end

    let pp ppf t =
      let ten = of_uns 10 in
      let rec fn t i = begin
        match cmp t zero with
        | Cmp.Eq -> ()
        | Cmp.Lt | Cmp.Gt -> begin
            let t' = t / ten in
            let () = fn t' (Uns.succ i) in

            let digit = match is_neg t with
              | true -> neg (t % ten)
              | false -> t % ten
            in
            let digit_w0 = match Cmp.is_eq (cmp digit zero) with
              | true -> U64.zero
              | false -> get 0 digit
            in
            Format.fprintf ppf "%Lu" digit_w0;
            if Uns.(i % 3 = 0) && Uns.(i > 0) then
              Format.fprintf ppf "_";
            ()
          end
      end in
      match Cmp.is_eq (cmp t zero) with
      | true -> Format.fprintf ppf "0%a" pp_suffix t
      | false -> begin
          let () = Format.fprintf ppf "%s" (if is_neg t then "-" else "") in
          let _ = fn t 0 in
          Format.fprintf ppf "%a" pp_suffix t
        end

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
      let () = match word_length t with
        | 0 -> fn Int64.zero 64
        | _ -> begin
            for i = Uns.pred (word_length t) downto 0 do
              let elm = get i t in
              if Uns.(i < pred (word_length t)) then Format.fprintf ppf "_";
              fn elm 64
            done;
          end
      in
      Format.fprintf ppf "%a" pp_suffix t

    let pp_o ppf t =
      let rec fn x shift = begin
        assert Uns.(shift % 3 = 0);
        match shift with
        | 0 -> ()
        | _ -> begin
            let shift' = Uns.(shift - 3) in
            let digit = bit_and (bit_usr ~shift:shift' x) (of_uns 0x7) in
            Format.fprintf ppf "%a" pp digit;
            fn x shift'
          end
      end in
      Format.fprintf ppf "0o";
      let () = match word_length t with
        | 0 -> Format.fprintf ppf "0"
        | _ -> begin
            let padded_shift = Uns.((bit_length t) + 2) in
            let shift = Uns.(padded_shift - (padded_shift % 3)) in
            fn t shift
          end
      in
      Format.fprintf ppf "%a" pp_suffix t

    let pp_x ppf t =
      let rec fn x shift = begin
        match shift with
        | 0 -> ()
        | _ -> begin
            if Uns.(shift < 64) then Format.fprintf ppf "_";
            let shift' = Uns.(shift - 16) in
            Format.fprintf ppf "%04Lx" Int64.(logand (shift_right_logical x shift') (of_int
                0xffff));
            fn x shift'
          end
      end in
      Format.fprintf ppf "0x";
      let () = match word_length t with
        | 0 -> Format.fprintf ppf "0000_0000_0000_0000"
        | _ -> begin
            for i = Uns.pred (word_length t) downto 0 do
              let elm = get i t in
              if Uns.(i < pred (word_length t)) then Format.fprintf ppf "_";
              fn elm 64
            done;
          end
      in
      Format.fprintf ppf "%a" pp_suffix t

    let of_string s =
      let getc_opt s i len = begin
        match i < len with
        | false -> None
        | true -> Some ((Stdlib.String.get s i), Uns.(i + 1))
      end in
      let getc s i len = begin
        match getc_opt s i len with
        | None -> halt "Malformed string"
        | Some (c, i') -> c, i'
      end in
      let d_of_c c = begin
        match c with
        | '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9' ->
          of_uns Uns.((Stdlib.Char.code c) - (Stdlib.Char.code '0'))
        | 'a' | 'b' | 'c' | 'd' | 'e' | 'f' ->
          of_uns Uns.(10 + (Stdlib.Char.code c) - (Stdlib.Char.code 'a'))
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
                  let suf = Stdlib.String.sub s i RudimentsInt.(j' - i) in
                  let nbits = int_of_string suf in
                  match T.min_word_length = T.max_word_length && nbits = Uns.(T.min_word_length *
                      64) with
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
      prefix0 s 0 (Stdlib.String.length s)

    let to_string t =
      Format.asprintf "%a" pp t
  end
  include U
  include Identifiable.Make(U)
  include Cmpable.MakeZero(U)

  (* Intnb.MakeDerived() cannot be used here because `bit_length` isn't constant. It would be
   * possible to adapt the `bit_length` provided via the input module interface such that all other
   * users ignore the value parameter, but the API ripple effects would be rather difficult to
   * recognize as valid/intentional. *)

  let is_pow2 t =
    match t > zero with
    | false -> false
    | true -> Uns.( = ) (bit_pop t) 1

  let floor_pow2 t =
    match cmp t one with
    | Lt -> halt "Invalid input"
    | Eq -> t
    | Gt -> begin
        let nb = bit_length t in let lz = bit_clz t in
        bit_sl ~shift:Uns.(nb - 1 - lz) one
      end

  let ceil_pow2 t =
    match cmp t one with
    | Lt -> halt "Invalid input"
    | Eq -> t
    | Gt -> begin
        let pred_t = t - one in
        match Uns.(T.min_word_length = T.max_word_length), bit_clz pred_t with
        | true, 0 -> zero
        | _, lz -> bit_sl ~shift:Uns.((bit_length pred_t) - lz) one
      end

  let floor_lg_opt t =
    match cmp t zero with
    | Lt | Eq -> None
    | Gt -> begin
        let nb = bit_length t in
        let lz = bit_clz t in
        Some (of_uns Uns.(nb - 1 - lz))
      end

  let floor_lg t =
    match floor_lg_opt t with
    | None -> halt "Invalid input"
    | Some x -> x

  let ceil_lg t =
    match floor_lg_opt t with
    | None -> halt "Invalid input"
    | Some x -> (x + (if is_pow2 t then zero else one))

  let min t0 t1 =
    if t0 <= t1 then t0 else t1

  let max t0 t1 =
    if t0 < t1 then t1 else t0
end

module MakeVI (T : IV) : SVI with type t := T.t = struct
  module U = struct
    include T
    let signed = true
  end
  include U
  include MakeVCommon(U)
end

module MakeVU (T : IV) : SVU with type t := T.t = struct
  module U = struct
    include T
    let signed = false
  end
  include U
  include MakeVCommon(U)
end

module type IFCommon = sig
  include IF
  val signed: bool
end

module MakeFCommon (T : IFCommon) : SFI with type t := T.t = struct
  module U = struct
    module V = struct
      include T
      let min_word_length = T.word_length
      let max_word_length = T.word_length
      let init n ~f =
        assert (n = T.word_length);
        T.init ~f
      let word_length _t =
        T.word_length
      let get = T.get
    end
    include V
    include MakeVCommon(V)
  end
  include U

  let init = T.init
  let word_length = T.word_length
  let bit_length = Uns.(word_length * 64)

  let max_value =
    match T.signed with
    | true -> bit_usr ~shift:1 (bit_not zero)
    | false -> bit_not zero

  let min_value =
    match T.signed with
    | true -> bit_sl ~shift:Uns.((word_length * 64) - 1) one
    | false -> zero
end

module MakeFI (T : IF) : SFI with type t := T.t = struct
  module U = struct
    include T
    let signed = true
  end
  include U
  include MakeFCommon(U)
end

module MakeFU (T : IF) : SFU with type t := T.t = struct
  module U = struct
    include T
    let signed = false
  end
  include U
  include MakeFCommon(U)
end
