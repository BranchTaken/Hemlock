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

    external intw_icmp: t -> t -> word_length_cb -> get_cb -> Cmp.t =
      "hm_basis_intw_i_cmp"
    external intw_ucmp: t -> t -> word_length_cb -> get_cb -> Cmp.t =
      "hm_basis_intw_u_cmp"

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

    external intw_ubit_and: t -> t -> word_length_cb -> get_cb -> uns -> uns
      -> int64 array = "hm_basis_intw_u_bit_and_bytecode"
        "hm_basis_intw_u_bit_and_native"
    external intw_ibit_and: t -> t -> word_length_cb -> get_cb -> uns -> uns
      -> int64 array = "hm_basis_intw_i_bit_and_bytecode"
        "hm_basis_intw_i_bit_and_native"

    let bit_and t0 t1 =
      of_arr (
        match T.signed with
        | true -> intw_ibit_and t0 t1 word_length get T.min_word_length
            T.max_word_length
        | false -> intw_ubit_and t0 t1 word_length get T.min_word_length
            T.max_word_length
      )

    external intw_ubit_or: t -> t -> word_length_cb -> get_cb -> uns -> uns
      -> int64 array = "hm_basis_intw_u_bit_or_bytecode"
        "hm_basis_intw_u_bit_or_native"
    external intw_ibit_or: t -> t -> word_length_cb -> get_cb -> uns -> uns
      -> int64 array = "hm_basis_intw_i_bit_or_bytecode"
        "hm_basis_intw_i_bit_or_native"
    let bit_or t0 t1 =
      of_arr (
        match T.signed with
        | true -> intw_ibit_or t0 t1 word_length get T.min_word_length
            T.max_word_length
        | false-> intw_ubit_or t0 t1 word_length get T.min_word_length
            T.max_word_length
      )

    external intw_ubit_xor: t -> t -> word_length_cb -> get_cb -> uns -> uns
      -> int64 array = "hm_basis_intw_u_bit_xor_bytecode"
        "hm_basis_intw_u_bit_xor_native"
    external intw_ibit_xor: t -> t -> word_length_cb -> get_cb -> uns -> uns
      -> int64 array = "hm_basis_intw_i_bit_xor_bytecode"
        "hm_basis_intw_i_bit_xor_native"

    let bit_xor t0 t1 =
      of_arr (
        match T.signed with
        | true -> intw_ibit_xor t0 t1 word_length get T.min_word_length
            T.max_word_length
        | false -> intw_ubit_xor t0 t1 word_length get T.min_word_length
            T.max_word_length
      )

    external intw_bit_unot: t -> word_length_cb -> get_cb -> uns -> uns
      -> int64 array = "hm_basis_intw_u_bit_not"
    external intw_bit_inot: t -> word_length_cb -> get_cb -> uns -> uns
      -> int64 array = "hm_basis_intw_i_bit_not"

    let bit_not t =
      of_arr (
        match T.signed with
        | true -> intw_bit_inot t word_length get T.min_word_length
            T.max_word_length
        | false -> intw_bit_unot t word_length get T.min_word_length
            T.max_word_length
      )

    external intw_bit_usl: uns -> t -> word_length_cb -> get_cb -> uns -> uns
      -> int64 array = "hm_basis_intw_u_bit_sl_bytecode"
        "hm_basis_intw_u_bit_sl_native"
    external intw_bit_isl: uns -> t -> word_length_cb -> get_cb -> uns -> uns
      -> int64 array = "hm_basis_intw_i_bit_sl_bytecode"
        "hm_basis_intw_i_bit_sl_native"

    let bit_sl ~shift t =
      of_arr (
        match T.signed with
        | true -> intw_bit_isl shift t word_length get T.min_word_length
            T.max_word_length
        | false -> intw_bit_usl shift t word_length get T.min_word_length
            T.max_word_length
      )

    external intw_bit_usr: uns -> t -> word_length_cb -> get_cb -> uns -> uns
      -> int64 array = "hm_basis_intw_bit_usr_bytecode"
        "hm_basis_intw_bit_usr_native"

    let bit_usr ~shift t =
      of_arr (intw_bit_usr shift t word_length get T.min_word_length
          T.max_word_length)

    external intw_bit_ssr: uns -> t -> word_length_cb -> get_cb -> uns -> uns
      -> int64 array = "hm_basis_intw_bit_ssr_bytecode"
        "hm_basis_intw_bit_ssr_native"

    let bit_ssr ~shift t =
      of_arr (intw_bit_ssr shift t word_length get T.min_word_length
          T.max_word_length)

    external intw_bit_pop: t -> word_length_cb -> get_cb -> uns =
      "hm_basis_intw_bit_pop"

    let bit_pop t =
      intw_bit_pop t word_length get

    external intw_bit_clz: t -> word_length_cb -> get_cb -> uns =
      "hm_basis_intw_bit_clz"

    let bit_clz t =
      intw_bit_clz t word_length get

    external intw_bit_ctz: t -> word_length_cb -> get_cb -> uns =
      "hm_basis_intw_bit_ctz"

    let bit_ctz t =
      intw_bit_ctz t word_length get

    external intw_uadd: t -> t -> word_length_cb -> get_cb -> uns -> uns
      -> int64 array = "hm_basis_intw_u_add_bytecode"
        "hm_basis_intw_u_add_native"
    external intw_iadd: t -> t -> word_length_cb -> get_cb -> uns -> uns
      -> int64 array = "hm_basis_intw_i_add_bytecode"
        "hm_basis_intw_i_add_native"

    let ( + ) t0 t1 =
      of_arr (
        match T.signed with
        | true -> intw_iadd t0 t1 word_length get T.min_word_length
            T.max_word_length
        | false -> intw_uadd t0 t1 word_length get T.min_word_length
            T.max_word_length
      )

    external intw_usub: t -> t -> word_length_cb -> get_cb -> uns -> uns
      -> int64 array = "hm_basis_intw_u_sub_bytecode"
        "hm_basis_intw_u_sub_native"
    external intw_isub: t -> t -> word_length_cb -> get_cb -> uns -> uns
      -> int64 array = "hm_basis_intw_i_sub_bytecode"
        "hm_basis_intw_i_sub_native"

    let ( - ) t0 t1 =
      of_arr (
        match T.signed with
        | true -> intw_isub t0 t1 word_length get T.min_word_length
            T.max_word_length
        | false -> intw_usub t0 t1 word_length get T.min_word_length
            T.max_word_length
      )

    external intw_umul: t -> t -> word_length_cb -> get_cb -> uns -> uns
      -> int64 array = "hm_basis_intw_u_mul_bytecode"
        "hm_basis_intw_u_mul_native"
    external intw_imul: t -> t -> word_length_cb -> get_cb -> uns -> uns
      -> int64 array = "hm_basis_intw_i_mul_bytecode"
        "hm_basis_intw_i_mul_native"

    let ( * ) t0 t1 =
      of_arr (
        match T.signed with
        | true -> intw_imul t0 t1 word_length get T.min_word_length
            T.max_word_length
        | false -> intw_umul t0 t1 word_length get T.min_word_length
            T.max_word_length
      )

    external intw_udiv: t -> t -> word_length_cb -> get_cb -> uns -> uns
      -> int64 array = "hm_basis_intw_u_div_bytecode"
        "hm_basis_intw_u_div_native"
    external intw_idiv: t -> t -> word_length_cb -> get_cb -> uns -> uns
      -> int64 array = "hm_basis_intw_i_div_bytecode"
        "hm_basis_intw_i_div_native"

    let ( / ) t0 t1 =
      of_arr (
        match t1 = zero, T.signed with
        | true, _ -> halt "Division by 0"
        | false, true -> intw_idiv t0 t1 word_length get T.min_word_length
            T.max_word_length
        | false, false ->
          intw_udiv t0 t1 word_length get T.min_word_length T.max_word_length
      )

    external intw_umod: t -> t -> word_length_cb -> get_cb -> uns -> uns
      -> int64 array = "hm_basis_intw_u_mod_bytecode"
        "hm_basis_intw_u_mod_native"
    external intw_imod: t -> t -> word_length_cb -> get_cb -> uns -> uns
      -> int64 array = "hm_basis_intw_i_mod_bytecode"
        "hm_basis_intw_i_mod_native"

    let ( % ) t0 t1 =
      of_arr (
        match t1 = zero, T.signed with
        | true, _ -> halt "Division by 0"
        | false, true -> intw_imod t0 t1 word_length get T.min_word_length
            T.max_word_length
        | false, false ->
          intw_umod t0 t1 word_length get T.min_word_length T.max_word_length
      )

    external intw_neg: t -> word_length_cb -> get_cb -> uns -> uns
      -> int64 array = "hm_basis_intw_i_neg"

    let ( ~- ) t =
      of_arr (intw_neg t word_length get T.min_word_length T.max_word_length)

    let ( ~+ ) t =
      t

    let neg = ( ~- )

    external intw_abs: t -> word_length_cb -> get_cb -> uns -> uns
      -> int64 array = "hm_basis_intw_i_abs"

    let abs t =
      match T.signed with
      | true -> of_arr (intw_abs t word_length get T.min_word_length
          T.max_word_length)
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

    external intw_i_of_real: real -> uns -> uns -> int64 array =
      "hm_basis_intw_i_of_real"
    external intw_u_of_real: real -> uns -> uns -> int64 array =
      "hm_basis_intw_u_of_real"

    let of_real r =
      match Real.is_nan r with
      | true -> halt "Not a number"
      | false -> begin
          of_arr (
            match T.signed with
            | true ->
              intw_i_of_real r T.min_word_length T.max_word_length
            | false ->
              intw_u_of_real r T.min_word_length T.max_word_length
          )
        end

    external intw_i_to_real: t -> word_length_cb -> get_cb -> real =
      "hm_basis_intw_i_to_real"
    external intw_u_to_real: t -> word_length_cb -> get_cb -> real =
      "hm_basis_intw_u_to_real"

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
            let bit =
              Int64.(logand (shift_right_logical x shift') (of_int 0x1)) in
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
            Format.fprintf ppf "%04Lx"
              Int64.(logand (shift_right_logical x shift') (of_int 0xffff));
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
                  let suf = Caml.String.sub s i RudimentsInt.(j' - i) in
                  let nbits = int_of_string suf in
                  match T.min_word_length = T.max_word_length &&
                        nbits = Uns.(T.min_word_length * 64) with
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
  include Cmpable.MakeZero(U)

  (* Intnb.MakeDerived() cannot be used here because `bit_length` isn't
   * constant. It would be possible to adapt the `bit_length` provided via the
   * input module interface such that all other users ignore the value
   * parameter, but the API ripple effects would be rather difficult to
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
        match Uns.(T.min_word_length = T.max_word_length),
          bit_clz pred_t with
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

(******************************************************************************)
(* Begin tests. *)

module N = struct
  module T = struct
    type t = u64 array
    let min_word_length = 0
    let max_word_length = Uns.(2 ** 26) (* 2**32 bits. *)
    let init = Array.init
    let word_length = Array.length
    let get = Array.get
  end
  include T
  include MakeVU(T)
end

let%expect_test "N pp" =
  let open N in
  let open Format in
  let rec fn = function
    | [] -> ()
    | x :: xs' -> begin
        printf "%a %a %a %a\n" pp_b x pp_o x pp x pp_x x;
        fn xs'
      end
  in
  printf "@[<h>";
  fn [
    zero;
    one;
    of_string "42";
  ];
  printf "@]";

  [%expect{|
    0b00000000_00000000_00000000_00000000_00000000_00000000_00000000_00000000 0o0 0 0x0000_0000_0000_0000
    0b00000000_00000000_00000000_00000000_00000000_00000000_00000000_00000001 0o0000000000000000000001 1 0x0000_0000_0000_0001
    0b00000000_00000000_00000000_00000000_00000000_00000000_00000000_00101010 0o0000000000000000000052 42 0x0000_0000_0000_002a
    |}]

let%expect_test "N hash_fold" =
  let open N in
  let open Format in
  printf "@[<h>";
  let rec test_hash_fold us = begin
    match us with
    | [] -> ()
    | u :: us' -> begin
        printf "hash_fold %a -> %a\n"
          pp_x u Hash.pp (Hash.t_of_state (hash_fold u Hash.State.empty));
        test_hash_fold us'
      end
  end in
  let us = [zero; one] in
  test_hash_fold us;
  printf "@]";

  [%expect{|
    hash_fold 0x0000_0000_0000_0000 -> 0x0000_0000_0000_0000_0000_0000_0000_0000u128
    hash_fold 0x0000_0000_0000_0001 -> 0x3d8a_cdb4_d36d_9c06_0044_03b7_fb05_c44au128
    |}]

let%expect_test "N constants" =
  let open N in
  let open Format in

  printf "zero=%a\n" pp_x zero;
  printf "one=%a\n" pp_x one;

  [%expect{|
    zero=0x0000_0000_0000_0000
    one=0x0000_0000_0000_0001
    |}]

let%expect_test "N of_string" =
  let open N in
  let open Format in
  printf "@[<h>";
  let rec test_strs = function
    | [] -> ()
    | s :: strs' -> begin
        printf "of_string %S -> %a\n" s pp_x (of_string s);
        test_strs strs'
      end
  in
  let strs = [
    "0";
    "1";
    "9876543210";
    "9876543210_";
    "9";

    "57896044618658097711785492504343953926634992332820282019728792003956564819967";
    "115792089237316195423570985008687907853269984665640564039457584007913129639935";
    "231584178474632390847141970017375815706539969331281128078915168015826259279871";

    "0b0";
    "0b1";
    "0b10";
    "0b10_";
    "0b_1";
    ("0b_11111111_11111111_11111111_11111111" ^
        "_11111111_11111111_11111111_11111111" ^
        "_11111111_11111111_11111111_11111111" ^
        "_11111111_11111111_11111111_11111111" ^
        "_11111111_11111111_11111111_11111111" ^
        "_11111111_11111111_11111111_11111111" ^
        "_11111111_11111111_11111111_11111111" ^
        "_11111111_11111111_11111111_11111111");

    "0x0";
    "0x1";
    "0xfedcba9876543210";
    "0xfedcba9876543210_";
    "0xf";
    "0x_f";
    "0x_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff";
  ] in
  test_strs strs;
  printf "@]";

  [%expect{|
    of_string "0" -> 0x0000_0000_0000_0000
    of_string "1" -> 0x0000_0000_0000_0001
    of_string "9876543210" -> 0x0000_0002_4cb0_16ea
    of_string "9876543210_" -> 0x0000_0002_4cb0_16ea
    of_string "9" -> 0x0000_0000_0000_0009
    of_string "57896044618658097711785492504343953926634992332820282019728792003956564819967" -> 0x7fff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff
    of_string "115792089237316195423570985008687907853269984665640564039457584007913129639935" -> 0xffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff
    of_string "231584178474632390847141970017375815706539969331281128078915168015826259279871" -> 0x0000_0000_0000_0001_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff
    of_string "0b0" -> 0x0000_0000_0000_0000
    of_string "0b1" -> 0x0000_0000_0000_0001
    of_string "0b10" -> 0x0000_0000_0000_0002
    of_string "0b10_" -> 0x0000_0000_0000_0002
    of_string "0b_1" -> 0x0000_0000_0000_0001
    of_string "0b_11111111_11111111_11111111_11111111_11111111_11111111_11111111_11111111_11111111_11111111_11111111_11111111_11111111_11111111_11111111_11111111_11111111_11111111_11111111_11111111_11111111_11111111_11111111_11111111_11111111_11111111_11111111_11111111_11111111_11111111_11111111_11111111" -> 0xffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff
    of_string "0x0" -> 0x0000_0000_0000_0000
    of_string "0x1" -> 0x0000_0000_0000_0001
    of_string "0xfedcba9876543210" -> 0xfedc_ba98_7654_3210
    of_string "0xfedcba9876543210_" -> 0xfedc_ba98_7654_3210
    of_string "0xf" -> 0x0000_0000_0000_000f
    of_string "0x_f" -> 0x0000_0000_0000_000f
    of_string "0x_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff" -> 0xffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff
    |}]

let%expect_test "N rel" =
  let open N in
  let open Format in
  let fn x y = begin
    printf "cmp %a %a -> %a\n" pp_x x pp_x y Cmp.pp (cmp x y);
    printf "%a >= %a -> %b\n" pp_x x pp_x y (x >= y);
    printf "%a <= %a -> %b\n" pp_x x pp_x y (x <= y);
    printf "%a = %a -> %b\n" pp_x x pp_x y (x = y);
    printf "%a > %a -> %b\n" pp_x x pp_x y (x > y);
    printf "%a < %a -> %b\n" pp_x x pp_x y (x < y);
    printf "%a <> %a -> %b\n" pp_x x pp_x y (x <> y);
    printf "ascending %a %a -> %a\n" pp_x x pp_x y Cmp.pp (ascending x y);
    printf "descending %a %a -> %a\n" pp_x x pp_x y Cmp.pp (descending x y);
  end in
  fn zero (of_string
      "0x8000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000");
  printf "\n";
  fn zero (of_string
      "0xffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff");
  printf "\n";
  fn (of_string
      "0x8000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000")
    (of_string
        "0x7fff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff");
  let fn2 t min max = begin
    printf "\n";
    printf "clamp ~min:%a ~max:%a %a -> %a\n"
      pp_x min pp_x max pp_x t pp_x (clamp ~min ~max t);
    printf "between ~low:%a ~high:%a %a -> %b\n"
      pp_x min pp_x max pp_x t (between ~low:min ~high:max t);
  end in
  fn2 (of_string
      "0x7fff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_fffe")
    (of_string
        "0x7fff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff")
    (of_string
        "0x8000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0001");
  fn2 (of_string
      "0x7fff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff")
    (of_string
        "0x7fff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff")
    (of_string
        "0x8000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0001");
  fn2 (of_string
      "0x8000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000")
    (of_string
        "0x7fff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff")
    (of_string
        "0x8000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0001");
  fn2 (of_string
      "0x8000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0001")
    (of_string
        "0x7fff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff")
    (of_string
        "0x8000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0001");
  fn2 (of_string
      "0x8000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0002")
    (of_string
        "0x7fff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff")
    (of_string
        "0x8000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0001");

  [%expect{|
    cmp 0x0000_0000_0000_0000 0x8000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000 -> Lt
    0x0000_0000_0000_0000 >= 0x8000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000 -> false
    0x0000_0000_0000_0000 <= 0x8000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000 -> true
    0x0000_0000_0000_0000 = 0x8000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000 -> false
    0x0000_0000_0000_0000 > 0x8000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000 -> false
    0x0000_0000_0000_0000 < 0x8000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000 -> true
    0x0000_0000_0000_0000 <> 0x8000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000 -> true
    ascending 0x0000_0000_0000_0000 0x8000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000 -> Lt
    descending 0x0000_0000_0000_0000 0x8000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000 -> Gt

    cmp 0x0000_0000_0000_0000 0xffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff -> Lt
    0x0000_0000_0000_0000 >= 0xffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff -> false
    0x0000_0000_0000_0000 <= 0xffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff -> true
    0x0000_0000_0000_0000 = 0xffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff -> false
    0x0000_0000_0000_0000 > 0xffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff -> false
    0x0000_0000_0000_0000 < 0xffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff -> true
    0x0000_0000_0000_0000 <> 0xffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff -> true
    ascending 0x0000_0000_0000_0000 0xffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff -> Lt
    descending 0x0000_0000_0000_0000 0xffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff -> Gt

    cmp 0x8000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000 0x7fff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff -> Gt
    0x8000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000 >= 0x7fff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff -> true
    0x8000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000 <= 0x7fff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff -> false
    0x8000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000 = 0x7fff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff -> false
    0x8000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000 > 0x7fff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff -> true
    0x8000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000 < 0x7fff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff -> false
    0x8000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000 <> 0x7fff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff -> true
    ascending 0x8000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000 0x7fff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff -> Gt
    descending 0x8000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000 0x7fff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff -> Lt

    clamp ~min:0x7fff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff ~max:0x8000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0001 0x7fff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_fffe -> 0x7fff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff
    between ~low:0x7fff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff ~high:0x8000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0001 0x7fff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_fffe -> false

    clamp ~min:0x7fff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff ~max:0x8000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0001 0x7fff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff -> 0x7fff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff
    between ~low:0x7fff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff ~high:0x8000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0001 0x7fff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff -> true

    clamp ~min:0x7fff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff ~max:0x8000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0001 0x8000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000 -> 0x8000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000
    between ~low:0x7fff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff ~high:0x8000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0001 0x8000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000 -> true

    clamp ~min:0x7fff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff ~max:0x8000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0001 0x8000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0001 -> 0x8000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0001
    between ~low:0x7fff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff ~high:0x8000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0001 0x8000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0001 -> true

    clamp ~min:0x7fff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff ~max:0x8000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0001 0x8000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0002 -> 0x8000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0001
    between ~low:0x7fff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff ~high:0x8000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0001 0x8000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0002 -> false
    |}]

let%expect_test "N widening" =
  let open N in
  let open Format in
  let u64_max = of_u64 U64.max_value in
  let fifteen = of_string "15" in
  printf "u64_max -> %a\n" pp_x u64_max;

  let r = (u64_max + one) in
  printf "u64_max + %a -> %a %a\n" pp one pp_x r pp r;

  (* This underflow unintuitively results in two words rather than one, because
   * it doesn't make sense to trim leading "sign" bits for an
   * arbitrary-precision unsigned result. *)
  let r = (zero - one) in
  printf "%a - %a -> %a %a\n" pp zero pp one pp_x r pp r;

  let r = (u64_max * fifteen) in
  printf "u64_max * %a -> %a %a\n" pp fifteen pp_x r pp r;

  [%expect{|
    u64_max -> 0xffff_ffff_ffff_ffff
    u64_max + 1 -> 0x0000_0000_0000_0001_0000_0000_0000_0000 18_446_744_073_709_551_616
    0 - 1 -> 0xffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff 340_282_366_920_938_463_463_374_607_431_768_211_455
    u64_max * 15 -> 0x0000_0000_0000_000e_ffff_ffff_ffff_fff1 276_701_161_105_643_274_225
    |}]

let%expect_test "N +,-" =
  let open N in
  let open Format in
  printf "@[<h>";
  let rec test_pairs = function
    | [] -> ()
    | (x, y) :: pairs' -> begin
        printf "%a +,- %a -> %a, %a\n" pp_x x pp_x y pp_x (x + y) pp_x
          (x - y);
        test_pairs pairs'
      end
  in
  let pairs = [
    (of_string "0", of_string "0");

    (of_string "0", of_string "1");
    (of_string "1", of_string "0");

    (of_string "1", of_string "0xffff_ffff_ffff_ffff");
    (of_string "0xffff_ffff_ffff_ffff", of_string "1");

    (of_string "1", of_string "0xffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff");
    (of_string "0xffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff", of_string "1");

    (of_string "1", of_string
        "0xffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff");
    (of_string
        "0xffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff",
      of_string "1");
  ] in
  test_pairs pairs;
  printf "@]";

  [%expect{|
    0x0000_0000_0000_0000 +,- 0x0000_0000_0000_0000 -> 0x0000_0000_0000_0000, 0x0000_0000_0000_0000
    0x0000_0000_0000_0000 +,- 0x0000_0000_0000_0001 -> 0x0000_0000_0000_0001, 0xffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff
    0x0000_0000_0000_0001 +,- 0x0000_0000_0000_0000 -> 0x0000_0000_0000_0001, 0x0000_0000_0000_0001
    0x0000_0000_0000_0001 +,- 0xffff_ffff_ffff_ffff -> 0x0000_0000_0000_0001_0000_0000_0000_0000, 0xffff_ffff_ffff_ffff_0000_0000_0000_0002
    0xffff_ffff_ffff_ffff +,- 0x0000_0000_0000_0001 -> 0x0000_0000_0000_0001_0000_0000_0000_0000, 0xffff_ffff_ffff_fffe
    0x0000_0000_0000_0001 +,- 0xffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff -> 0x0000_0000_0000_0001_0000_0000_0000_0000_0000_0000_0000_0000, 0xffff_ffff_ffff_ffff_0000_0000_0000_0000_0000_0000_0000_0002
    0xffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff +,- 0x0000_0000_0000_0001 -> 0x0000_0000_0000_0001_0000_0000_0000_0000_0000_0000_0000_0000, 0xffff_ffff_ffff_ffff_ffff_ffff_ffff_fffe
    0x0000_0000_0000_0001 +,- 0xffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff -> 0x0000_0000_0000_0001_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000, 0xffff_ffff_ffff_ffff_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0002
    0xffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff +,- 0x0000_0000_0000_0001 -> 0x0000_0000_0000_0001_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000, 0xffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_fffe
    |}]

let%expect_test "N *" =
  let open N in
  let open Format in
  printf "@[<h>";
  let rec test_pairs = function
    | [] -> ()
    | (x, y) :: pairs' -> begin
        let z = (x * y) in
        printf "%a * %a -> %a\n" pp_x x pp_x y pp_x z;
        test_pairs pairs'
      end
  in
  let pairs = [
    (of_string "0", of_string "0");
    (of_string "0", of_string "1");
    (of_string "1", of_string "0");
    (of_string "1", of_string "0xffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff");
    (of_string "0xffff_ffff", of_string "0xffff_ffff");
    (of_string "0xffff_ffff_ffff_ffff", of_string "0xffff_ffff_ffff_ffff");
    (of_string "0xffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff",
      of_string "0xffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff");
    (of_string "0xffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff",
      of_string "0xffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff")
  ] in
  test_pairs pairs;
  printf "@]";

  [%expect{|
    0x0000_0000_0000_0000 * 0x0000_0000_0000_0000 -> 0x0000_0000_0000_0000
    0x0000_0000_0000_0000 * 0x0000_0000_0000_0001 -> 0x0000_0000_0000_0000
    0x0000_0000_0000_0001 * 0x0000_0000_0000_0000 -> 0x0000_0000_0000_0000
    0x0000_0000_0000_0001 * 0xffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff -> 0xffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff
    0x0000_0000_ffff_ffff * 0x0000_0000_ffff_ffff -> 0xffff_fffe_0000_0001
    0xffff_ffff_ffff_ffff * 0xffff_ffff_ffff_ffff -> 0xffff_ffff_ffff_fffe_0000_0000_0000_0001
    0xffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff * 0xffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff -> 0xffff_ffff_ffff_ffff_ffff_ffff_ffff_fffe_0000_0000_0000_0000_0000_0000_0000_0001
    0xffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff * 0xffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff -> 0xffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_fffe_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0001
    |}]

let%expect_test "N of_real,to_real" =
  let open N in
  let open Format in
  printf "@[<h>";
  let rec test_rs rs = begin
    match rs with
    | [] -> ()
    | r :: rs' -> begin
        let x = of_real r in
        printf "of_real %h -> %a; to_real -> %h\n"
          r pp_x x (to_real x);
        test_rs rs'
      end
  end in
  let rs = [
    -1.;
    0.;
    0x1.1p-1;
    1.;

    0x1.f_ffff_ffff_ffffp48;
    0x1.f_ffff_ffff_ffffp52;
    0x1.f_ffff_ffff_ffffp56;

    0x1.f_ffff_ffff_ffffp127;
    0x1.f_ffff_ffff_ffffp128;
    0x1.f_ffff_ffff_ffffp132;

    0x1.f_ffff_ffff_ffffp255;
    0x1.f_ffff_ffff_ffffp256;
    0x1.f_ffff_ffff_ffffp260;
    0x1.f_ffff_ffff_ffffp1023;

    0x1p254;
    0x1p255;
    0x1p256;
  ] in
  test_rs rs;
  printf "\n";
  let rec test_xs xs = begin
    match xs with
    | [] -> ()
    | x :: xs' -> begin
        let r = to_real x in
        printf "to_real %a -> %h; of_real -> %a\n"
          pp_x x r pp_x (of_real r);
        test_xs xs'
      end
  end in
  let xs = [
    zero;
    one;
  ] in
  test_xs xs;
  printf "@]";

  [%expect{|
    of_real -0x1p+0 -> 0x0000_0000_0000_0000; to_real -> 0x0p+0
    of_real 0x0p+0 -> 0x0000_0000_0000_0000; to_real -> 0x0p+0
    of_real 0x1.1p-1 -> 0x0000_0000_0000_0000; to_real -> 0x0p+0
    of_real 0x1p+0 -> 0x0000_0000_0000_0001; to_real -> 0x1p+0
    of_real 0x1.fffffffffffffp+48 -> 0x0001_ffff_ffff_ffff; to_real -> 0x1.ffffffffffffp+48
    of_real 0x1.fffffffffffffp+52 -> 0x001f_ffff_ffff_ffff; to_real -> 0x1.fffffffffffffp+52
    of_real 0x1.fffffffffffffp+56 -> 0x01ff_ffff_ffff_fff0; to_real -> 0x1.fffffffffffffp+56
    of_real 0x1.fffffffffffffp+127 -> 0xffff_ffff_ffff_f800_0000_0000_0000_0000; to_real -> 0x1.fffffffffffffp+127
    of_real 0x1.fffffffffffffp+128 -> 0x0000_0000_0000_0001_ffff_ffff_ffff_f000_0000_0000_0000_0000; to_real -> 0x1.fffffffffffffp+128
    of_real 0x1.fffffffffffffp+132 -> 0x0000_0000_0000_001f_ffff_ffff_ffff_0000_0000_0000_0000_0000; to_real -> 0x1.fffffffffffffp+132
    of_real 0x1.fffffffffffffp+255 -> 0xffff_ffff_ffff_f800_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000; to_real -> 0x1.fffffffffffffp+255
    of_real 0x1.fffffffffffffp+256 -> 0x0000_0000_0000_0001_ffff_ffff_ffff_f000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000; to_real -> 0x1.fffffffffffffp+256
    of_real 0x1.fffffffffffffp+260 -> 0x0000_0000_0000_001f_ffff_ffff_ffff_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000; to_real -> 0x1.fffffffffffffp+260
    of_real 0x1.fffffffffffffp+1023 -> 0xffff_ffff_ffff_f800_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000; to_real -> 0x1.fffffffffffffp+1023
    of_real 0x1p+254 -> 0x4000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000; to_real -> 0x1p+254
    of_real 0x1p+255 -> 0x8000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000; to_real -> 0x1p+255
    of_real 0x1p+256 -> 0x0000_0000_0000_0001_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000; to_real -> 0x1p+256

    to_real 0x0000_0000_0000_0000 -> 0x0p+0; of_real -> 0x0000_0000_0000_0000
    to_real 0x0000_0000_0000_0001 -> 0x1p+0; of_real -> 0x0000_0000_0000_0001
    |}]

let%expect_test "N /,%" =
  let open N in
  let open Format in
  printf "@[<h>";
  let rec test_pairs = function
    | [] -> ()
    | (x, y) :: pairs' -> begin
        let quotient = x / y in
        let remainder = x % y in
        printf "%a /,%% %a -> %a, %a\n"
          pp_x x pp_x y pp_x quotient pp_x remainder;
        assert (x = (y * quotient + remainder));
        assert (x >= y || remainder = x);
        test_pairs pairs'
      end
  in
  let pairs = [
    (* < 1 *)
    (of_string "0", of_string "1");
    (of_string "1", of_string
        "0xffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff");
    (of_string "0xfffe", of_string "0xffff");
    (of_string "0xffff_fffe", of_string "0xffff_ffff");
    (of_string "0xffff_ffff_ffff_fffe", of_string "0xffff_ffff_ffff_ffff");
    (of_string "0xf_ffff_ffff_ffff_fffe", of_string "0xf_ffff_ffff_ffff_ffff");
    (of_string "0xffff_ffff_ffff_ffff_fffe",
      of_string "0xffff_ffff_ffff_ffff_ffff");
    (of_string "0xffff_ffff_ffff_ffff_ffff_ffff_ffff_fffe",
      of_string "0xffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff");
    (of_string
        "0xffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_fffe",
      of_string
        "0xffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff");

    (* Single-digit (base 2^32) divisor. *)
    (of_string "1", of_string "1");
    (of_string
        "0xffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff",
      of_string "1");
    (of_string
        "0xffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff",
      of_string "2");
    (of_string
        "0xffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff",
      of_string "3");
    (of_string
        "0xffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff",
      of_string "7");
    (of_string
        "0xffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff",
      of_string "0xffff");
    (of_string
        "0xffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff",
      of_string "0xffff_ffff");

    (* Multi-digit (base 2^32) divisor. *)
    (of_string "0x1_0000_0000", of_string "0x1_0000_0000");
    (of_string "0x1_ffff_ffff", of_string "0x1_0000_0000");
    (of_string "0x2_ffff_ffff", of_string "0x1_0000_0000");
    (of_string "0xffff_ffff_ffff_ffff", of_string "0xffff_ffff_ffff_ffff");
    (of_string "0xffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff",
      of_string "0xffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff");
    (of_string
        "0xffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff",
      of_string
        "0xffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff");
    (of_string "0xffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff",
      of_string "0xffff_ffff_ffff_ffff");
    (of_string
        "0xffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff",
      of_string "0xffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff");
  ] in
  test_pairs pairs;
  printf "@]";

  [%expect{|
    0x0000_0000_0000_0000 /,% 0x0000_0000_0000_0001 -> 0x0000_0000_0000_0000, 0x0000_0000_0000_0000
    0x0000_0000_0000_0001 /,% 0xffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff -> 0x0000_0000_0000_0000, 0x0000_0000_0000_0001
    0x0000_0000_0000_fffe /,% 0x0000_0000_0000_ffff -> 0x0000_0000_0000_0000, 0x0000_0000_0000_fffe
    0x0000_0000_ffff_fffe /,% 0x0000_0000_ffff_ffff -> 0x0000_0000_0000_0000, 0x0000_0000_ffff_fffe
    0xffff_ffff_ffff_fffe /,% 0xffff_ffff_ffff_ffff -> 0x0000_0000_0000_0000, 0xffff_ffff_ffff_fffe
    0x0000_0000_0000_000f_ffff_ffff_ffff_fffe /,% 0x0000_0000_0000_000f_ffff_ffff_ffff_ffff -> 0x0000_0000_0000_0000, 0x0000_0000_0000_000f_ffff_ffff_ffff_fffe
    0x0000_0000_0000_ffff_ffff_ffff_ffff_fffe /,% 0x0000_0000_0000_ffff_ffff_ffff_ffff_ffff -> 0x0000_0000_0000_0000, 0x0000_0000_0000_ffff_ffff_ffff_ffff_fffe
    0xffff_ffff_ffff_ffff_ffff_ffff_ffff_fffe /,% 0xffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff -> 0x0000_0000_0000_0000, 0xffff_ffff_ffff_ffff_ffff_ffff_ffff_fffe
    0xffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_fffe /,% 0xffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff -> 0x0000_0000_0000_0000, 0xffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_fffe
    0x0000_0000_0000_0001 /,% 0x0000_0000_0000_0001 -> 0x0000_0000_0000_0001, 0x0000_0000_0000_0000
    0xffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff /,% 0x0000_0000_0000_0001 -> 0xffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff, 0x0000_0000_0000_0000
    0xffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff /,% 0x0000_0000_0000_0002 -> 0x7fff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff, 0x0000_0000_0000_0001
    0xffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff /,% 0x0000_0000_0000_0003 -> 0x5555_5555_5555_5555_5555_5555_5555_5555_5555_5555_5555_5555_5555_5555_5555_5555, 0x0000_0000_0000_0000
    0xffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff /,% 0x0000_0000_0000_0007 -> 0x2492_4924_9249_2492_4924_9249_2492_4924_9249_2492_4924_9249_2492_4924_9249_2492, 0x0000_0000_0000_0001
    0xffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff /,% 0x0000_0000_0000_ffff -> 0x0001_0001_0001_0001_0001_0001_0001_0001_0001_0001_0001_0001_0001_0001_0001_0001, 0x0000_0000_0000_0000
    0xffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff /,% 0x0000_0000_ffff_ffff -> 0x0000_0001_0000_0001_0000_0001_0000_0001_0000_0001_0000_0001_0000_0001_0000_0001, 0x0000_0000_0000_0000
    0x0000_0001_0000_0000 /,% 0x0000_0001_0000_0000 -> 0x0000_0000_0000_0001, 0x0000_0000_0000_0000
    0x0000_0001_ffff_ffff /,% 0x0000_0001_0000_0000 -> 0x0000_0000_0000_0001, 0x0000_0000_ffff_ffff
    0x0000_0002_ffff_ffff /,% 0x0000_0001_0000_0000 -> 0x0000_0000_0000_0002, 0x0000_0000_ffff_ffff
    0xffff_ffff_ffff_ffff /,% 0xffff_ffff_ffff_ffff -> 0x0000_0000_0000_0001, 0x0000_0000_0000_0000
    0xffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff /,% 0xffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff -> 0x0000_0000_0000_0001, 0x0000_0000_0000_0000
    0xffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff /,% 0xffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff -> 0x0000_0000_0000_0001, 0x0000_0000_0000_0000
    0xffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff /,% 0xffff_ffff_ffff_ffff -> 0x0000_0000_0000_0001_0000_0000_0000_0001, 0x0000_0000_0000_0000
    0xffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff /,% 0xffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff -> 0x0000_0000_0000_0001_0000_0000_0000_0000_0000_0000_0000_0001, 0x0000_0000_0000_0000
    |}]

let%expect_test "N bit_and,bit_or,bit_xor" =
  let open N in
  let open Format in
  printf "@[<h>";
  let rec test_pairs = function
    | [] -> ()
    | (x, y) :: pairs' -> begin
        printf "bit_{and,or,xor} %a %a -> %a, %a, %a\n"
          pp_x x pp_x y
          pp_x (bit_and x y)
          pp_x (bit_or x y)
          pp_x (bit_xor x y);
        test_pairs pairs'
      end
  in
  let u64_max = of_u64 U64.max_value in
  let pairs = [
    (zero, zero);
    (u64_max, zero);
    (zero, u64_max);
    (u64_max, u64_max);
  ] in
  test_pairs pairs;
  printf "@]";

  [%expect{|
    bit_{and,or,xor} 0x0000_0000_0000_0000 0x0000_0000_0000_0000 -> 0x0000_0000_0000_0000, 0x0000_0000_0000_0000, 0x0000_0000_0000_0000
    bit_{and,or,xor} 0xffff_ffff_ffff_ffff 0x0000_0000_0000_0000 -> 0x0000_0000_0000_0000, 0xffff_ffff_ffff_ffff, 0xffff_ffff_ffff_ffff
    bit_{and,or,xor} 0x0000_0000_0000_0000 0xffff_ffff_ffff_ffff -> 0x0000_0000_0000_0000, 0xffff_ffff_ffff_ffff, 0xffff_ffff_ffff_ffff
    bit_{and,or,xor} 0xffff_ffff_ffff_ffff 0xffff_ffff_ffff_ffff -> 0xffff_ffff_ffff_ffff, 0xffff_ffff_ffff_ffff, 0x0000_0000_0000_0000
    |}]

let%expect_test "N bit_not" =
  let open N in
  let open Format in
  printf "@[<h>";
  let rec test = function
    | [] -> ()
    | x :: xs' -> begin
        printf "bit_not %a -> %a\n"
          pp_x x pp_x (bit_not x);
        test xs'
      end
  in
  let xs = [
    zero; (* No bits, therefore the bitwise not is also no bits. *)
    one;
    of_u64 U64.max_value
  ] in
  test xs;
  printf "@]";

  [%expect{|
    bit_not 0x0000_0000_0000_0000 -> 0x0000_0000_0000_0000
    bit_not 0x0000_0000_0000_0001 -> 0xffff_ffff_ffff_fffe
    bit_not 0xffff_ffff_ffff_ffff -> 0x0000_0000_0000_0000
    |}]

let%expect_test "N bit_pop,bit_clz,bit_ctz" =
  let open N in
  let open Format in
  printf "@[<h>";
  let rec test = function
    | [] -> ()
    | x :: xs' -> begin
        printf "bit_length, bit_{pop,clz,ctz} %a -> %u, %u, %u, %u\n"
          pp_x x (bit_length x) (bit_pop x) (bit_clz x) (bit_ctz x);
        test xs'
      end
  in
  let xs = [
    of_string "0";
    of_string "1";
    of_string "0x8000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000";
    of_string "0xffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff"
  ] in
  test xs;
  printf "@]";

  [%expect{|
    bit_length, bit_{pop,clz,ctz} 0x0000_0000_0000_0000 -> 0, 0, 0, 0
    bit_length, bit_{pop,clz,ctz} 0x0000_0000_0000_0001 -> 64, 1, 63, 0
    bit_length, bit_{pop,clz,ctz} 0x8000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000 -> 256, 1, 0, 255
    bit_length, bit_{pop,clz,ctz} 0xffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff -> 256, 256, 0, 0
    |}]

let%expect_test "N **" =
  let open N in
  let open Format in
  printf "@[<h>";
  let rec test_pairs = function
    | [] -> ()
    | (x, y) :: pairs' -> begin
        printf "%a ** %a -> %a\n" pp_x x pp_x y pp_x (x ** y);
        test_pairs pairs'
      end
  in
  let pairs = [
    (of_string "0", of_string "0");
    (of_string "0", of_string "1");

    (of_string
        "0xffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff",
      of_string "0");
    (of_string
        "0xffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff",
      of_string "1");

    (of_string "2", of_string "31");
    (of_string "2", of_string "32");
    (of_string "2", of_string "63");
    (of_string "2", of_string "64");
    (of_string "2", of_string "127");
    (of_string "2", of_string "128");
    (of_string "2", of_string "255");
    (of_string "2", of_string "256");

    (of_string "0xf", of_string "0xf");
    (of_string "0xff", of_string "0xff");

    (of_string "1", of_string
        "0xffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff");
    (of_string "0x1ff", of_string "0x1ff");
  ] in
  test_pairs pairs;
  printf "@]";

  [%expect{|
    0x0000_0000_0000_0000 ** 0x0000_0000_0000_0000 -> 0x0000_0000_0000_0001
    0x0000_0000_0000_0000 ** 0x0000_0000_0000_0001 -> 0x0000_0000_0000_0000
    0xffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff ** 0x0000_0000_0000_0000 -> 0x0000_0000_0000_0001
    0xffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff ** 0x0000_0000_0000_0001 -> 0xffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff
    0x0000_0000_0000_0002 ** 0x0000_0000_0000_001f -> 0x0000_0000_8000_0000
    0x0000_0000_0000_0002 ** 0x0000_0000_0000_0020 -> 0x0000_0001_0000_0000
    0x0000_0000_0000_0002 ** 0x0000_0000_0000_003f -> 0x8000_0000_0000_0000
    0x0000_0000_0000_0002 ** 0x0000_0000_0000_0040 -> 0x0000_0000_0000_0001_0000_0000_0000_0000
    0x0000_0000_0000_0002 ** 0x0000_0000_0000_007f -> 0x8000_0000_0000_0000_0000_0000_0000_0000
    0x0000_0000_0000_0002 ** 0x0000_0000_0000_0080 -> 0x0000_0000_0000_0001_0000_0000_0000_0000_0000_0000_0000_0000
    0x0000_0000_0000_0002 ** 0x0000_0000_0000_00ff -> 0x8000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000
    0x0000_0000_0000_0002 ** 0x0000_0000_0000_0100 -> 0x0000_0000_0000_0001_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000
    0x0000_0000_0000_000f ** 0x0000_0000_0000_000f -> 0x0613_b62c_5977_07ef
    0x0000_0000_0000_00ff ** 0x0000_0000_0000_00ff -> 0x005e_5c8b_0eb9_5ab0_8f9d_37ef_127f_c01b_d0e3_3de5_2647_5283_96d7_8d5f_8da3_1989_e678_14f6_bba1_fb0f_0207_010f_f5f2_347b_19d5_f659_8fc9_1bf5_a88f_77da_a3d7_b382_fec4_84f3_d205_c06a_3444_5384_c0e7_ab0d_8837_88c6_8c01_2cb4_3305_5edd_a746_a484_0944_4ea9_1147_273b_79fc_3eab_b70e_ca55_2af6_50c2_34bb_01ed_4044_27f1_7cdd_dd71_d08e_39ef_9c39_82e3_ce44_e670_456a_a815_4c1f_dbd9_c359_47f4_9463_6a42_5c69_bf89_e9c7_5ad3_b7a0_a559_af0f_5da9_947c_8deb_a644_1731_0713_b23e_7ef4_de50_bb2a_3e90_bc2a_c3da_5201_cca8_d6e5_dfea_887c_4f7a_4e92_175d_9f88_bd27_79b5_7f9e_b35b_e752_8f96_5a06_da0a_c41d_cb3a_34f1_d8ab_7d8f_ee62_0a94_faa4_2c39_5997_756b_007f_feff
    0x0000_0000_0000_0001 ** 0xffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff -> 0x0000_0000_0000_0001
    0x0000_0000_0000_01ff ** 0x0000_0000_0000_01ff -> 0x002f_2275_878c_70d5_a1b1_bd32_e9be_69ee_6031_0850_8abd_3586_c249_5d97_655c_39cd_c7f1_40f1_9cec_df9e_e0e3_be45_8a7c_0dc0_1c8d_dd04_c2de_effd_7c1a_b167_fbf1_6b25_77cb_6903_363f_4655_f97a_98ed_fc03_be77_953f_2575_d2df_3f79_532a_4751_69ec_77ca_9fc9_ee90_804f_4901_1bd9_13f2_a6f3_556c_d9fb_12ca_faa0_aef3_01eb_4243_3745_f677_fd48_03e6_8898_f3ed_b912_10af_068f_11c1_2919_2cfc_091c_6735_3d37_31b6_3c1b_1fe7_140f_f6b2_dcd5_26c6_9417_ee7d_d0de_62d3_f880_07ce_bd5b_a430_5f1e_96fb_916e_4b70_8643_a1d8_e5e9_b51c_d1d4_f54e_afe6_c65e_3db3_b671_d474_879e_5b70_3c51_81bc_31b0_6a0c_eb7d_c5b1_905a_a965_e795_1c7a_9dae_29cc_e8e0_e45f_1c22_d5bf_064b_7b51_e368_aa7a_f272_92ef_6580_7d37_1e7d_23dd_3a15_d100_ebbc_159a_aabd_315b_26bf_880d_aa1e_ab90_20c3_c0e2_d0eb_dedc_4b7f_3475_a98d_27be_0705_5a8e_c158_aa7f_07ec_a308_c70f_0dbe_8266_bd11_9bfe_644c_9468_8674_b1aa_6d74_d0fd_665d_b7b4_db73_3247_7fc8_c1ac_c162_13d7_f291_419b_0b46_3c4f_59eb_2bf5_c6bf_1270_06a8_65ca_a5e8_a46f_6b0f_33c0_21f6_7dd8_8fb7_f31b_8417_2e31_d1ac_e7ed_f662_d734_848b_4e33_d4f4_9836_8a4f_3160_cb65_ff93_9efc_a84c_3042_dc1f_6e97_6bf0_87bb_28d8_3b14_f834_03c0_cfb4_07ca_e0ed_6cb6_e2ea_ac2b_dfa8_cfca_39b0_010a_b6ed_7497_9fb5_a43d_e2c3_70ab_b67a_2c72_9a9c_bf22_a838_e4dd_0ce4_36dd_99c9_1a9d_6222_dceb_964c_0b7b_71ec_be59_f303_f1e5_f808_dae1_3bea_1dae_d8aa_c976_476d_5e0d_1376_b2ac_4b3d_9be1_8d37_eebd_9742_6b55_f5c9_d27e_af9b_92e2_7bd2_b430_fa73_4fdf_d326_44a2_b762_b132_e959_9888_02b0_03ff_fdff
    |}]

let%expect_test "N is_pow2" =
  let open N in
  let open Format in
  printf "@[<h>";
  let rec test = function
    | [] -> ()
    | u :: us' -> begin
        printf "is_pow2 %a -> %b\n"
          pp_x u
          (is_pow2 u);
        test us'
      end
  in
  let us = [
    of_string "0";
    of_string "1";
    of_string "2";
    of_string "3";
    of_string
      "0x8000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000";
    of_string
      "0xffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff"
  ] in
  test us;
  printf "@]";

  [%expect{|
    is_pow2 0x0000_0000_0000_0000 -> false
    is_pow2 0x0000_0000_0000_0001 -> true
    is_pow2 0x0000_0000_0000_0002 -> true
    is_pow2 0x0000_0000_0000_0003 -> false
    is_pow2 0x8000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000 -> true
    is_pow2 0xffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff -> false
    |}]

let%expect_test "N floor_pow2,ceil_pow2" =
  let open N in
  let open Format in
  printf "@[<h>";
  let rec test = function
    | [] -> ()
    | u :: us' -> begin
        printf "floor_pow2,ceil_pow2 %a -> %a, %a\n"
          pp_x u
          pp_x (floor_pow2 u)
          pp_x (ceil_pow2 u);
        test us'
      end
  in
  let us = [
    of_string "1";
    of_string "2";
    of_string "3";

    of_string "0x8000_0000_0000_0000";
    of_string "0xffff_ffff_ffff_ffff";

    of_string "0x8000_0000_0000_0000_0000_0000_0000_0000";
    of_string "0xffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff";

    of_string
      "0x8000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000";
    of_string
      "0xffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff"
  ] in
  test us;
  printf "@]";

  [%expect{|
    floor_pow2,ceil_pow2 0x0000_0000_0000_0001 -> 0x0000_0000_0000_0001, 0x0000_0000_0000_0001
    floor_pow2,ceil_pow2 0x0000_0000_0000_0002 -> 0x0000_0000_0000_0002, 0x0000_0000_0000_0002
    floor_pow2,ceil_pow2 0x0000_0000_0000_0003 -> 0x0000_0000_0000_0002, 0x0000_0000_0000_0004
    floor_pow2,ceil_pow2 0x8000_0000_0000_0000 -> 0x8000_0000_0000_0000, 0x8000_0000_0000_0000
    floor_pow2,ceil_pow2 0xffff_ffff_ffff_ffff -> 0x8000_0000_0000_0000, 0x0000_0000_0000_0001_0000_0000_0000_0000
    floor_pow2,ceil_pow2 0x8000_0000_0000_0000_0000_0000_0000_0000 -> 0x8000_0000_0000_0000_0000_0000_0000_0000, 0x8000_0000_0000_0000_0000_0000_0000_0000
    floor_pow2,ceil_pow2 0xffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff -> 0x8000_0000_0000_0000_0000_0000_0000_0000, 0x0000_0000_0000_0001_0000_0000_0000_0000_0000_0000_0000_0000
    floor_pow2,ceil_pow2 0x8000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000 -> 0x8000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000, 0x8000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000
    floor_pow2,ceil_pow2 0xffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff -> 0x8000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000, 0x0000_0000_0000_0001_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000
    |}]

let%expect_test "N floor_lg,ceil_lg" =
  let open N in
  let open Format in
  printf "@[<h>";
  let rec test = function
    | [] -> ()
    | u :: us' -> begin
        printf "floor_lg,ceil_lg %a -> %a, %a\n"
          pp_x u
          pp (floor_lg u)
          pp (ceil_lg u);
        test us'
      end
  in
  let us = [
    of_string "1";
    of_string "2";
    of_string "3";
    of_string
      "0x8000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000";
    of_string
      "0xffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff"
  ] in
  test us;
  printf "@]";

  [%expect{|
        floor_lg,ceil_lg 0x0000_0000_0000_0001 -> 0, 0
        floor_lg,ceil_lg 0x0000_0000_0000_0002 -> 1, 1
        floor_lg,ceil_lg 0x0000_0000_0000_0003 -> 1, 2
        floor_lg,ceil_lg 0x8000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000 -> 255, 255
        floor_lg,ceil_lg 0xffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff -> 255, 256
    |}]

let%expect_test "N min,max" =
  let open N in
  let open Format in
  printf "@[<h>";
  let rec test_pairs = function
    | [] -> ()
    | (x, y) :: pairs' -> begin
        printf "min,max %a %a -> %a, %a\n"
          pp_x x pp_x y pp_x (min x y) pp_x (max x y);
        test_pairs pairs'
      end
  in
  let pairs = [
    (of_string "0", of_string "0");
    (of_string "0", of_string "1");
    (of_string "1", of_string "0");
    (of_string "1", of_string "1");
    (of_string "0", of_string
        "0xffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff");
  ] in
  test_pairs pairs;
  printf "@]";

  [%expect{|
    min,max 0x0000_0000_0000_0000 0x0000_0000_0000_0000 -> 0x0000_0000_0000_0000, 0x0000_0000_0000_0000
    min,max 0x0000_0000_0000_0000 0x0000_0000_0000_0001 -> 0x0000_0000_0000_0000, 0x0000_0000_0000_0001
    min,max 0x0000_0000_0000_0001 0x0000_0000_0000_0000 -> 0x0000_0000_0000_0000, 0x0000_0000_0000_0001
    min,max 0x0000_0000_0000_0001 0x0000_0000_0000_0001 -> 0x0000_0000_0000_0001, 0x0000_0000_0000_0001
    min,max 0x0000_0000_0000_0000 0xffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff -> 0x0000_0000_0000_0000, 0xffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff
   |}]

module Z = struct
  module T = struct
    type t = u64 array
    let min_word_length = 0
    let max_word_length = Uns.(2 ** 26) (* 2**32 bits. *)
    let init = Array.init
    let word_length = Array.length
    let get = Array.get
  end
  include T
  include MakeVI(T)
end

let%expect_test "Z pp" =
  let open Z in
  let open Format in
  let rec fn = function
    | [] -> ()
    | x :: xs' -> begin
        printf "%a %a %a %a\n" pp_b x pp_o x pp x pp_x x;
        fn xs'
      end
  in
  printf "@[<h>";
  fn [
    zero;
    one;
    neg_one;
    of_string "42";
  ];
  printf "@]";

  [%expect{|
    0b00000000_00000000_00000000_00000000_00000000_00000000_00000000_00000000 0o0 0 0x0000_0000_0000_0000
    0b00000000_00000000_00000000_00000000_00000000_00000000_00000000_00000001 0o0000000000000000000001 1 0x0000_0000_0000_0001
    0b11111111_11111111_11111111_11111111_11111111_11111111_11111111_11111111 0o1777777777777777777777 -1 0xffff_ffff_ffff_ffff
    0b00000000_00000000_00000000_00000000_00000000_00000000_00000000_00101010 0o0000000000000000000052 42 0x0000_0000_0000_002a
    |}]

let%expect_test "Z hash_fold" =
  let open Z in
  let open Format in
  printf "@[<h>";
  let rec test_hash_fold us = begin
    match us with
    | [] -> ()
    | u :: us' -> begin
        printf "hash_fold %a -> %a\n"
          pp_x u Hash.pp (Hash.t_of_state (hash_fold u Hash.State.empty));
        test_hash_fold us'
      end
  end in
  let us = [zero; one] in
  test_hash_fold us;
  printf "@]";

  [%expect{|
    hash_fold 0x0000_0000_0000_0000 -> 0x0000_0000_0000_0000_0000_0000_0000_0000u128
    hash_fold 0x0000_0000_0000_0001 -> 0x3d8a_cdb4_d36d_9c06_0044_03b7_fb05_c44au128
    |}]

let%expect_test "Z constants" =
  let open Z in
  let open Format in

  printf "zero=%a\n" pp_x zero;
  printf "one=%a\n" pp_x one;
  printf "neg_one=%a\n" pp_x neg_one;

  [%expect{|
    zero=0x0000_0000_0000_0000
    one=0x0000_0000_0000_0001
    neg_one=0xffff_ffff_ffff_ffff
    |}]

let%expect_test "Z of_string" =
  let open Z in
  let open Format in
  printf "@[<h>";
  let rec test_strs = function
    | [] -> ()
    | s :: strs' -> begin
        printf "of_string %S -> %a\n" s pp_x (of_string s);
        test_strs strs'
      end
  in
  let strs = [
    "0";
    "1";
    "9876543210";
    "9876543210_";
    "9";

    "57896044618658097711785492504343953926634992332820282019728792003956564819967";
    "115792089237316195423570985008687907853269984665640564039457584007913129639935";
    "231584178474632390847141970017375815706539969331281128078915168015826259279871";

    "0b0";
    "0b1";
    "0b10";
    "0b10_";
    "0b_1";
    ("0b_11111111_11111111_11111111_11111111" ^
        "_11111111_11111111_11111111_11111111" ^
        "_11111111_11111111_11111111_11111111" ^
        "_11111111_11111111_11111111_11111111" ^
        "_11111111_11111111_11111111_11111111" ^
        "_11111111_11111111_11111111_11111111" ^
        "_11111111_11111111_11111111_11111111" ^
        "_11111111_11111111_11111111_11111111");

    "0x0";
    "0x1";
    "0xfedcba9876543210";
    "0xfedcba9876543210_";
    "0xf";
    "0x_f";
    "0x_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff";
  ] in
  test_strs strs;
  printf "@]";

  [%expect{|
    of_string "0" -> 0x0000_0000_0000_0000
    of_string "1" -> 0x0000_0000_0000_0001
    of_string "9876543210" -> 0x0000_0002_4cb0_16ea
    of_string "9876543210_" -> 0x0000_0002_4cb0_16ea
    of_string "9" -> 0x0000_0000_0000_0009
    of_string "57896044618658097711785492504343953926634992332820282019728792003956564819967" -> 0x7fff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff
    of_string "115792089237316195423570985008687907853269984665640564039457584007913129639935" -> 0x0000_0000_0000_0000_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff
    of_string "231584178474632390847141970017375815706539969331281128078915168015826259279871" -> 0x0000_0000_0000_0001_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff
    of_string "0b0" -> 0x0000_0000_0000_0000
    of_string "0b1" -> 0x0000_0000_0000_0001
    of_string "0b10" -> 0x0000_0000_0000_0002
    of_string "0b10_" -> 0x0000_0000_0000_0002
    of_string "0b_1" -> 0x0000_0000_0000_0001
    of_string "0b_11111111_11111111_11111111_11111111_11111111_11111111_11111111_11111111_11111111_11111111_11111111_11111111_11111111_11111111_11111111_11111111_11111111_11111111_11111111_11111111_11111111_11111111_11111111_11111111_11111111_11111111_11111111_11111111_11111111_11111111_11111111_11111111" -> 0x0000_0000_0000_0000_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff
    of_string "0x0" -> 0x0000_0000_0000_0000
    of_string "0x1" -> 0x0000_0000_0000_0001
    of_string "0xfedcba9876543210" -> 0x0000_0000_0000_0000_fedc_ba98_7654_3210
    of_string "0xfedcba9876543210_" -> 0x0000_0000_0000_0000_fedc_ba98_7654_3210
    of_string "0xf" -> 0x0000_0000_0000_000f
    of_string "0x_f" -> 0x0000_0000_0000_000f
    of_string "0x_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff" -> 0x0000_0000_0000_0000_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff
    |}]

let%expect_test "Z rel" =
  let open Z in
  let open Format in
  let fn x y = begin
    printf "cmp %a %a -> %a\n" pp_x x pp_x y Cmp.pp (cmp x y);
    printf "%a >= %a -> %b\n" pp_x x pp_x y (x >= y);
    printf "%a <= %a -> %b\n" pp_x x pp_x y (x <= y);
    printf "%a = %a -> %b\n" pp_x x pp_x y (x = y);
    printf "%a > %a -> %b\n" pp_x x pp_x y (x > y);
    printf "%a < %a -> %b\n" pp_x x pp_x y (x < y);
    printf "%a <> %a -> %b\n" pp_x x pp_x y (x <> y);
    printf "ascending %a %a -> %a\n" pp_x x pp_x y Cmp.pp (ascending x y);
    printf "descending %a %a -> %a\n" pp_x x pp_x y Cmp.pp (descending x y);
  end in
  fn zero one;
  fn zero neg_one;
  fn neg_one one;
  fn zero (of_string
      "0x8000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000");
  printf "\n";
  fn zero (of_string
      "0xffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff");
  printf "\n";
  fn (of_string
      "0x8000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000")
    (of_string
        "0x7fff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff");
  let fn2 t min max = begin
    printf "\n";
    printf "clamp ~min:%a ~max:%a %a -> %a\n"
      pp_x min pp_x max pp_x t pp_x (clamp ~min ~max t);
    printf "between ~low:%a ~high:%a %a -> %b\n"
      pp_x min pp_x max pp_x t (between ~low:min ~high:max t);
  end in
  fn2 (of_string
      "0x7fff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_fffe")
    (of_string
        "0x7fff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff")
    (of_string
        "0x8000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0001");
  fn2 (of_string
      "0x7fff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff")
    (of_string
        "0x7fff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff")
    (of_string
        "0x8000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0001");
  fn2 (of_string
      "0x8000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000")
    (of_string
        "0x7fff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff")
    (of_string
        "0x8000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0001");
  fn2 (of_string
      "0x8000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0001")
    (of_string
        "0x7fff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff")
    (of_string
        "0x8000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0001");
  fn2 (of_string
      "0x8000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0002")
    (of_string
        "0x7fff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff")
    (of_string
        "0x8000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0001");

  [%expect{|
    cmp 0x0000_0000_0000_0000 0x0000_0000_0000_0001 -> Lt
    0x0000_0000_0000_0000 >= 0x0000_0000_0000_0001 -> false
    0x0000_0000_0000_0000 <= 0x0000_0000_0000_0001 -> true
    0x0000_0000_0000_0000 = 0x0000_0000_0000_0001 -> false
    0x0000_0000_0000_0000 > 0x0000_0000_0000_0001 -> false
    0x0000_0000_0000_0000 < 0x0000_0000_0000_0001 -> true
    0x0000_0000_0000_0000 <> 0x0000_0000_0000_0001 -> true
    ascending 0x0000_0000_0000_0000 0x0000_0000_0000_0001 -> Lt
    descending 0x0000_0000_0000_0000 0x0000_0000_0000_0001 -> Gt
    cmp 0x0000_0000_0000_0000 0xffff_ffff_ffff_ffff -> Gt
    0x0000_0000_0000_0000 >= 0xffff_ffff_ffff_ffff -> true
    0x0000_0000_0000_0000 <= 0xffff_ffff_ffff_ffff -> false
    0x0000_0000_0000_0000 = 0xffff_ffff_ffff_ffff -> false
    0x0000_0000_0000_0000 > 0xffff_ffff_ffff_ffff -> true
    0x0000_0000_0000_0000 < 0xffff_ffff_ffff_ffff -> false
    0x0000_0000_0000_0000 <> 0xffff_ffff_ffff_ffff -> true
    ascending 0x0000_0000_0000_0000 0xffff_ffff_ffff_ffff -> Gt
    descending 0x0000_0000_0000_0000 0xffff_ffff_ffff_ffff -> Lt
    cmp 0xffff_ffff_ffff_ffff 0x0000_0000_0000_0001 -> Lt
    0xffff_ffff_ffff_ffff >= 0x0000_0000_0000_0001 -> false
    0xffff_ffff_ffff_ffff <= 0x0000_0000_0000_0001 -> true
    0xffff_ffff_ffff_ffff = 0x0000_0000_0000_0001 -> false
    0xffff_ffff_ffff_ffff > 0x0000_0000_0000_0001 -> false
    0xffff_ffff_ffff_ffff < 0x0000_0000_0000_0001 -> true
    0xffff_ffff_ffff_ffff <> 0x0000_0000_0000_0001 -> true
    ascending 0xffff_ffff_ffff_ffff 0x0000_0000_0000_0001 -> Lt
    descending 0xffff_ffff_ffff_ffff 0x0000_0000_0000_0001 -> Gt
    cmp 0x0000_0000_0000_0000 0x0000_0000_0000_0000_8000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000 -> Lt
    0x0000_0000_0000_0000 >= 0x0000_0000_0000_0000_8000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000 -> false
    0x0000_0000_0000_0000 <= 0x0000_0000_0000_0000_8000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000 -> true
    0x0000_0000_0000_0000 = 0x0000_0000_0000_0000_8000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000 -> false
    0x0000_0000_0000_0000 > 0x0000_0000_0000_0000_8000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000 -> false
    0x0000_0000_0000_0000 < 0x0000_0000_0000_0000_8000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000 -> true
    0x0000_0000_0000_0000 <> 0x0000_0000_0000_0000_8000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000 -> true
    ascending 0x0000_0000_0000_0000 0x0000_0000_0000_0000_8000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000 -> Lt
    descending 0x0000_0000_0000_0000 0x0000_0000_0000_0000_8000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000 -> Gt

    cmp 0x0000_0000_0000_0000 0x0000_0000_0000_0000_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff -> Lt
    0x0000_0000_0000_0000 >= 0x0000_0000_0000_0000_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff -> false
    0x0000_0000_0000_0000 <= 0x0000_0000_0000_0000_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff -> true
    0x0000_0000_0000_0000 = 0x0000_0000_0000_0000_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff -> false
    0x0000_0000_0000_0000 > 0x0000_0000_0000_0000_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff -> false
    0x0000_0000_0000_0000 < 0x0000_0000_0000_0000_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff -> true
    0x0000_0000_0000_0000 <> 0x0000_0000_0000_0000_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff -> true
    ascending 0x0000_0000_0000_0000 0x0000_0000_0000_0000_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff -> Lt
    descending 0x0000_0000_0000_0000 0x0000_0000_0000_0000_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff -> Gt

    cmp 0x0000_0000_0000_0000_8000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000 0x7fff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff -> Gt
    0x0000_0000_0000_0000_8000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000 >= 0x7fff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff -> true
    0x0000_0000_0000_0000_8000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000 <= 0x7fff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff -> false
    0x0000_0000_0000_0000_8000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000 = 0x7fff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff -> false
    0x0000_0000_0000_0000_8000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000 > 0x7fff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff -> true
    0x0000_0000_0000_0000_8000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000 < 0x7fff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff -> false
    0x0000_0000_0000_0000_8000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000 <> 0x7fff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff -> true
    ascending 0x0000_0000_0000_0000_8000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000 0x7fff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff -> Gt
    descending 0x0000_0000_0000_0000_8000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000 0x7fff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff -> Lt

    clamp ~min:0x7fff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff ~max:0x0000_0000_0000_0000_8000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0001 0x7fff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_fffe -> 0x7fff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff
    between ~low:0x7fff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff ~high:0x0000_0000_0000_0000_8000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0001 0x7fff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_fffe -> false

    clamp ~min:0x7fff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff ~max:0x0000_0000_0000_0000_8000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0001 0x7fff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff -> 0x7fff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff
    between ~low:0x7fff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff ~high:0x0000_0000_0000_0000_8000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0001 0x7fff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff -> true

    clamp ~min:0x7fff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff ~max:0x0000_0000_0000_0000_8000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0001 0x0000_0000_0000_0000_8000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000 -> 0x0000_0000_0000_0000_8000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000
    between ~low:0x7fff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff ~high:0x0000_0000_0000_0000_8000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0001 0x0000_0000_0000_0000_8000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000 -> true

    clamp ~min:0x7fff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff ~max:0x0000_0000_0000_0000_8000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0001 0x0000_0000_0000_0000_8000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0001 -> 0x0000_0000_0000_0000_8000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0001
    between ~low:0x7fff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff ~high:0x0000_0000_0000_0000_8000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0001 0x0000_0000_0000_0000_8000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0001 -> true

    clamp ~min:0x7fff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff ~max:0x0000_0000_0000_0000_8000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0001 0x0000_0000_0000_0000_8000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0002 -> 0x0000_0000_0000_0000_8000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0001
    between ~low:0x7fff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff ~high:0x0000_0000_0000_0000_8000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0001 0x0000_0000_0000_0000_8000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0002 -> false
    |}]

let%expect_test "Z widening" =
  let open Z in
  let open Format in
  let u64_max = of_u64 U64.max_value in
  let fifteen = of_string "15" in
  printf "u64_max -> %a\n" pp_x u64_max;

  let r = (u64_max + one) in
  printf "u64_max + %a -> %a %a\n" pp one pp_x r pp r;

  let r = (zero - one) in
  printf "%a - %a -> %a %a\n" pp zero pp one pp_x r pp r;

  let r = (u64_max * fifteen) in
  printf "u64_max * %a -> %a %a\n" pp fifteen pp_x r pp r;

  [%expect{|
    u64_max -> 0x0000_0000_0000_0000_ffff_ffff_ffff_ffff
    u64_max + 1 -> 0x0000_0000_0000_0001_0000_0000_0000_0000 18_446_744_073_709_551_616
    0 - 1 -> 0xffff_ffff_ffff_ffff -1
    u64_max * 15 -> 0x0000_0000_0000_000e_ffff_ffff_ffff_fff1 276_701_161_105_643_274_225
    |}]

let%expect_test "Z +,-" =
  let open Z in
  let open Format in
  printf "@[<h>";
  let rec test_pairs = function
    | [] -> ()
    | (x, y) :: pairs' -> begin
        printf "%a +,- %a -> %a, %a\n" pp_x x pp_x y pp_x (x + y) pp_x
          (x - y);
        test_pairs pairs'
      end
  in
  let pairs = [
    (of_string "0", of_string "0");

    (of_string "0", of_string "1");
    (of_string "1", of_string "0");

    (of_string "0", of_string "-1");
    (of_string "-1", of_string "0");

    (of_string "1", of_string "-1");
    (of_string "-1", of_string "1");

    (of_string "1", of_string "0x3fff_ffff_ffff_ffff");
    (of_string "0x3fff_ffff_ffff_ffff", of_string "1");

    (of_string "1", of_string "0x7fff_ffff_ffff_ffff");
    (of_string "0x7fff_ffff_ffff_ffff", of_string "1");

    (of_string "1", of_string "0xffff_ffff_ffff_ffff");
    (of_string "0xffff_ffff_ffff_ffff", of_string "1");

    (of_string "1", of_string "0x3fff_ffff_ffff_ffff_ffff_ffff_ffff_ffff");
    (of_string "0x3fff_ffff_ffff_ffff_ffff_ffff_ffff_ffff", of_string "1");

    (of_string "1", of_string "0x7fff_ffff_ffff_ffff_ffff_ffff_ffff_ffff");
    (of_string "0x7fff_ffff_ffff_ffff_ffff_ffff_ffff_ffff", of_string "1");

    (of_string "1", of_string "0xffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff");
    (of_string "0xffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff", of_string "1");

    (of_string "1", of_string
        "0x3fff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff");
    (of_string
        "0x3fff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff",
      of_string "1");

    (of_string "1", of_string
        "0x7fff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff");
    (of_string
        "0x7fff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff",
      of_string "1");

    (of_string "1", of_string
        "0xffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff");
    (of_string
        "0xffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff",
      of_string "1");
  ] in
  test_pairs pairs;
  printf "@]";

  [%expect{|
    0x0000_0000_0000_0000 +,- 0x0000_0000_0000_0000 -> 0x0000_0000_0000_0000, 0x0000_0000_0000_0000
    0x0000_0000_0000_0000 +,- 0x0000_0000_0000_0001 -> 0x0000_0000_0000_0001, 0xffff_ffff_ffff_ffff
    0x0000_0000_0000_0001 +,- 0x0000_0000_0000_0000 -> 0x0000_0000_0000_0001, 0x0000_0000_0000_0001
    0x0000_0000_0000_0000 +,- 0xffff_ffff_ffff_ffff -> 0xffff_ffff_ffff_ffff, 0x0000_0000_0000_0001
    0xffff_ffff_ffff_ffff +,- 0x0000_0000_0000_0000 -> 0xffff_ffff_ffff_ffff, 0xffff_ffff_ffff_ffff
    0x0000_0000_0000_0001 +,- 0xffff_ffff_ffff_ffff -> 0x0000_0000_0000_0000, 0x0000_0000_0000_0002
    0xffff_ffff_ffff_ffff +,- 0x0000_0000_0000_0001 -> 0x0000_0000_0000_0000, 0xffff_ffff_ffff_fffe
    0x0000_0000_0000_0001 +,- 0x3fff_ffff_ffff_ffff -> 0x4000_0000_0000_0000, 0xc000_0000_0000_0002
    0x3fff_ffff_ffff_ffff +,- 0x0000_0000_0000_0001 -> 0x4000_0000_0000_0000, 0x3fff_ffff_ffff_fffe
    0x0000_0000_0000_0001 +,- 0x7fff_ffff_ffff_ffff -> 0x0000_0000_0000_0000_8000_0000_0000_0000, 0x8000_0000_0000_0002
    0x7fff_ffff_ffff_ffff +,- 0x0000_0000_0000_0001 -> 0x0000_0000_0000_0000_8000_0000_0000_0000, 0x7fff_ffff_ffff_fffe
    0x0000_0000_0000_0001 +,- 0x0000_0000_0000_0000_ffff_ffff_ffff_ffff -> 0x0000_0000_0000_0001_0000_0000_0000_0000, 0xffff_ffff_ffff_ffff_0000_0000_0000_0002
    0x0000_0000_0000_0000_ffff_ffff_ffff_ffff +,- 0x0000_0000_0000_0001 -> 0x0000_0000_0000_0001_0000_0000_0000_0000, 0x0000_0000_0000_0000_ffff_ffff_ffff_fffe
    0x0000_0000_0000_0001 +,- 0x3fff_ffff_ffff_ffff_ffff_ffff_ffff_ffff -> 0x4000_0000_0000_0000_0000_0000_0000_0000, 0xc000_0000_0000_0000_0000_0000_0000_0002
    0x3fff_ffff_ffff_ffff_ffff_ffff_ffff_ffff +,- 0x0000_0000_0000_0001 -> 0x4000_0000_0000_0000_0000_0000_0000_0000, 0x3fff_ffff_ffff_ffff_ffff_ffff_ffff_fffe
    0x0000_0000_0000_0001 +,- 0x7fff_ffff_ffff_ffff_ffff_ffff_ffff_ffff -> 0x0000_0000_0000_0000_8000_0000_0000_0000_0000_0000_0000_0000, 0x8000_0000_0000_0000_0000_0000_0000_0002
    0x7fff_ffff_ffff_ffff_ffff_ffff_ffff_ffff +,- 0x0000_0000_0000_0001 -> 0x0000_0000_0000_0000_8000_0000_0000_0000_0000_0000_0000_0000, 0x7fff_ffff_ffff_ffff_ffff_ffff_ffff_fffe
    0x0000_0000_0000_0001 +,- 0x0000_0000_0000_0000_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff -> 0x0000_0000_0000_0001_0000_0000_0000_0000_0000_0000_0000_0000, 0xffff_ffff_ffff_ffff_0000_0000_0000_0000_0000_0000_0000_0002
    0x0000_0000_0000_0000_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff +,- 0x0000_0000_0000_0001 -> 0x0000_0000_0000_0001_0000_0000_0000_0000_0000_0000_0000_0000, 0x0000_0000_0000_0000_ffff_ffff_ffff_ffff_ffff_ffff_ffff_fffe
    0x0000_0000_0000_0001 +,- 0x3fff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff -> 0x4000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000, 0xc000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0002
    0x3fff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff +,- 0x0000_0000_0000_0001 -> 0x4000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000, 0x3fff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_fffe
    0x0000_0000_0000_0001 +,- 0x7fff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff -> 0x0000_0000_0000_0000_8000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000, 0x8000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0002
    0x7fff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff +,- 0x0000_0000_0000_0001 -> 0x0000_0000_0000_0000_8000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000, 0x7fff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_fffe
    0x0000_0000_0000_0001 +,- 0x0000_0000_0000_0000_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff -> 0x0000_0000_0000_0001_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000, 0xffff_ffff_ffff_ffff_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0002
    0x0000_0000_0000_0000_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff +,- 0x0000_0000_0000_0001 -> 0x0000_0000_0000_0001_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000, 0x0000_0000_0000_0000_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_fffe
    |}]

let%expect_test "Z *" =
  let open Z in
  let open Format in
  printf "@[<h>";
  let rec test_pairs = function
    | [] -> ()
    | (x, y) :: pairs' -> begin
        let z = (x * y) in
        printf "%a * %a -> %a\n" pp_x x pp_x y pp_x z;
        test_pairs pairs'
      end
  in
  let pairs = [
    (of_string "0", of_string "0");
    (of_string "0", of_string "1");
    (of_string "1", of_string "0");
    (of_string "1", of_string "0x7fff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff");
    (of_string "1", of_string "0xffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff");
    (of_string "0xffff_ffff", of_string "0xffff_ffff");
    (of_string "0xffff_ffff_ffff_ffff", of_string "0xffff_ffff_ffff_ffff");
    (of_string "0xffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff",
      of_string "0xffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff");
    (of_string "0xffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff",
      of_string "0xffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff")
  ] in
  test_pairs pairs;
  printf "@]";

  [%expect{|
    0x0000_0000_0000_0000 * 0x0000_0000_0000_0000 -> 0x0000_0000_0000_0000
    0x0000_0000_0000_0000 * 0x0000_0000_0000_0001 -> 0x0000_0000_0000_0000
    0x0000_0000_0000_0001 * 0x0000_0000_0000_0000 -> 0x0000_0000_0000_0000
    0x0000_0000_0000_0001 * 0x7fff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff -> 0x7fff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff
    0x0000_0000_0000_0001 * 0x0000_0000_0000_0000_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff -> 0x0000_0000_0000_0000_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff
    0x0000_0000_ffff_ffff * 0x0000_0000_ffff_ffff -> 0x0000_0000_0000_0000_ffff_fffe_0000_0001
    0x0000_0000_0000_0000_ffff_ffff_ffff_ffff * 0x0000_0000_0000_0000_ffff_ffff_ffff_ffff -> 0x0000_0000_0000_0000_ffff_ffff_ffff_fffe_0000_0000_0000_0001
    0x0000_0000_0000_0000_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff * 0x0000_0000_0000_0000_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff -> 0x0000_0000_0000_0000_ffff_ffff_ffff_ffff_ffff_ffff_ffff_fffe_0000_0000_0000_0000_0000_0000_0000_0001
    0x0000_0000_0000_0000_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff * 0x0000_0000_0000_0000_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff -> 0x0000_0000_0000_0000_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_fffe_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0001
    |}]

let%expect_test "Z of_real,to_real" =
  let open Z in
  let open Format in
  printf "@[<h>";
  let rec test_rs rs = begin
    match rs with
    | [] -> ()
    | r :: rs' -> begin
        let x = of_real r in
        printf "of_real %h -> %a; to_real -> %h\n"
          r pp_x x (to_real x);
        test_rs rs'
      end
  end in
  let rs = [
    -1.;
    0.;
    0x1.1p-1;
    1.;

    0x1.f_ffff_ffff_ffffp48;
    0x1.f_ffff_ffff_ffffp52;
    0x1.f_ffff_ffff_ffffp56;

    0x1.f_ffff_ffff_ffffp127;
    0x1.f_ffff_ffff_ffffp128;
    0x1.f_ffff_ffff_ffffp132;

    0x1.f_ffff_ffff_ffffp255;
    0x1.f_ffff_ffff_ffffp256;
    0x1.f_ffff_ffff_ffffp260;
    0x1.f_ffff_ffff_ffffp1023;

    0x1p254;
    0x1p255;
    0x1p256;
  ] in
  test_rs rs;
  printf "\n";
  let rec test_xs xs = begin
    match xs with
    | [] -> ()
    | x :: xs' -> begin
        let r = to_real x in
        printf "to_real %a -> %h; of_real -> %a\n"
          pp_x x r pp_x (of_real r);
        test_xs xs'
      end
  end in
  let xs = [
    zero;
    one;
  ] in
  test_xs xs;
  printf "@]";

  [%expect{|
    of_real -0x1p+0 -> 0xffff_ffff_ffff_ffff; to_real -> -0x1p+0
    of_real 0x0p+0 -> 0x0000_0000_0000_0000; to_real -> 0x0p+0
    of_real 0x1.1p-1 -> 0x0000_0000_0000_0000; to_real -> 0x0p+0
    of_real 0x1p+0 -> 0x0000_0000_0000_0001; to_real -> 0x1p+0
    of_real 0x1.fffffffffffffp+48 -> 0x0001_ffff_ffff_ffff; to_real -> 0x1.ffffffffffffp+48
    of_real 0x1.fffffffffffffp+52 -> 0x001f_ffff_ffff_ffff; to_real -> 0x1.fffffffffffffp+52
    of_real 0x1.fffffffffffffp+56 -> 0x01ff_ffff_ffff_fff0; to_real -> 0x1.fffffffffffffp+56
    of_real 0x1.fffffffffffffp+127 -> 0x0000_0000_0000_0000_ffff_ffff_ffff_f800_0000_0000_0000_0000; to_real -> 0x1.fffffffffffffp+127
    of_real 0x1.fffffffffffffp+128 -> 0x0000_0000_0000_0001_ffff_ffff_ffff_f000_0000_0000_0000_0000; to_real -> 0x1.fffffffffffffp+128
    of_real 0x1.fffffffffffffp+132 -> 0x0000_0000_0000_001f_ffff_ffff_ffff_0000_0000_0000_0000_0000; to_real -> 0x1.fffffffffffffp+132
    of_real 0x1.fffffffffffffp+255 -> 0x0000_0000_0000_0000_ffff_ffff_ffff_f800_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000; to_real -> 0x1.fffffffffffffp+255
    of_real 0x1.fffffffffffffp+256 -> 0x0000_0000_0000_0001_ffff_ffff_ffff_f000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000; to_real -> 0x1.fffffffffffffp+256
    of_real 0x1.fffffffffffffp+260 -> 0x0000_0000_0000_001f_ffff_ffff_ffff_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000; to_real -> 0x1.fffffffffffffp+260
    of_real 0x1.fffffffffffffp+1023 -> 0x0000_0000_0000_0000_ffff_ffff_ffff_f800_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000; to_real -> 0x1.fffffffffffffp+1023
    of_real 0x1p+254 -> 0x4000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000; to_real -> 0x1p+254
    of_real 0x1p+255 -> 0x0000_0000_0000_0000_8000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000; to_real -> 0x1p+255
    of_real 0x1p+256 -> 0x0000_0000_0000_0001_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000; to_real -> 0x1p+256

    to_real 0x0000_0000_0000_0000 -> 0x0p+0; of_real -> 0x0000_0000_0000_0000
    to_real 0x0000_0000_0000_0001 -> 0x1p+0; of_real -> 0x0000_0000_0000_0001
    |}]

let%expect_test "Z /,%" =
  let open Z in
  let open Format in
  printf "@[<h>";
  let rec test_pairs = function
    | [] -> ()
    | (x, y) :: pairs' -> begin
        let quotient = x / y in
        let remainder = x % y in
        printf "%a /,%% %a -> %a, %a\n"
          pp_x x pp_x y pp_x quotient pp_x remainder;
        assert (x = (y * quotient + remainder));
        assert (x >= y || remainder = x);
        test_pairs pairs'
      end
  in
  let pairs = [
    (* < 1 *)
    (of_string "0", of_string "1");
    (of_string "1", of_string
        "0xffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff");
    (of_string "0xfffe", of_string "0xffff");
    (of_string "0xffff_fffe", of_string "0xffff_ffff");
    (of_string "0xffff_ffff_ffff_fffe", of_string "0xffff_ffff_ffff_ffff");
    (of_string "0xf_ffff_ffff_ffff_fffe", of_string "0xf_ffff_ffff_ffff_ffff");
    (of_string "0xffff_ffff_ffff_ffff_fffe",
      of_string "0xffff_ffff_ffff_ffff_ffff");
    (of_string "0xffff_ffff_ffff_ffff_ffff_ffff_ffff_fffe",
      of_string "0xffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff");
    (of_string
        "0xffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_fffe",
      of_string
        "0xffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff");

    (* Single-digit (base 2^32) divisor. *)
    (of_string "1", of_string "1");
    (of_string
        "0xffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff",
      of_string "1");
    (of_string
        "0xffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff",
      of_string "2");
    (of_string
        "0xffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff",
      of_string "3");
    (of_string
        "0xffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff",
      of_string "7");
    (of_string
        "0xffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff",
      of_string "0xffff");
    (of_string
        "0xffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff",
      of_string "0xffff_ffff");

    (* Multi-digit (base 2^32) divisor. *)
    (of_string "0x1_0000_0000", of_string "0x1_0000_0000");
    (of_string "0x1_ffff_ffff", of_string "0x1_0000_0000");
    (of_string "0x2_ffff_ffff", of_string "0x1_0000_0000");
    (of_string "0xffff_ffff_ffff_ffff", of_string "0xffff_ffff_ffff_ffff");
    (of_string "0xffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff",
      of_string "0xffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff");
    (of_string
        "0xffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff",
      of_string
        "0xffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff");
    (of_string "0xffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff",
      of_string "0xffff_ffff_ffff_ffff");
    (of_string
        "0xffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff",
      of_string "0xffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff");
  ] in
  test_pairs pairs;
  printf "@]";

  [%expect{|
    0x0000_0000_0000_0000 /,% 0x0000_0000_0000_0001 -> 0x0000_0000_0000_0000, 0x0000_0000_0000_0000
    0x0000_0000_0000_0001 /,% 0x0000_0000_0000_0000_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff -> 0x0000_0000_0000_0000, 0x0000_0000_0000_0001
    0x0000_0000_0000_fffe /,% 0x0000_0000_0000_ffff -> 0x0000_0000_0000_0000, 0x0000_0000_0000_fffe
    0x0000_0000_ffff_fffe /,% 0x0000_0000_ffff_ffff -> 0x0000_0000_0000_0000, 0x0000_0000_ffff_fffe
    0x0000_0000_0000_0000_ffff_ffff_ffff_fffe /,% 0x0000_0000_0000_0000_ffff_ffff_ffff_ffff -> 0x0000_0000_0000_0000, 0x0000_0000_0000_0000_ffff_ffff_ffff_fffe
    0x0000_0000_0000_000f_ffff_ffff_ffff_fffe /,% 0x0000_0000_0000_000f_ffff_ffff_ffff_ffff -> 0x0000_0000_0000_0000, 0x0000_0000_0000_000f_ffff_ffff_ffff_fffe
    0x0000_0000_0000_ffff_ffff_ffff_ffff_fffe /,% 0x0000_0000_0000_ffff_ffff_ffff_ffff_ffff -> 0x0000_0000_0000_0000, 0x0000_0000_0000_ffff_ffff_ffff_ffff_fffe
    0x0000_0000_0000_0000_ffff_ffff_ffff_ffff_ffff_ffff_ffff_fffe /,% 0x0000_0000_0000_0000_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff -> 0x0000_0000_0000_0000, 0x0000_0000_0000_0000_ffff_ffff_ffff_ffff_ffff_ffff_ffff_fffe
    0x0000_0000_0000_0000_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_fffe /,% 0x0000_0000_0000_0000_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff -> 0x0000_0000_0000_0000, 0x0000_0000_0000_0000_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_fffe
    0x0000_0000_0000_0001 /,% 0x0000_0000_0000_0001 -> 0x0000_0000_0000_0001, 0x0000_0000_0000_0000
    0x0000_0000_0000_0000_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff /,% 0x0000_0000_0000_0001 -> 0x0000_0000_0000_0000_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff, 0x0000_0000_0000_0000
    0x0000_0000_0000_0000_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff /,% 0x0000_0000_0000_0002 -> 0x7fff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff, 0x0000_0000_0000_0001
    0x0000_0000_0000_0000_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff /,% 0x0000_0000_0000_0003 -> 0x5555_5555_5555_5555_5555_5555_5555_5555_5555_5555_5555_5555_5555_5555_5555_5555, 0x0000_0000_0000_0000
    0x0000_0000_0000_0000_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff /,% 0x0000_0000_0000_0007 -> 0x2492_4924_9249_2492_4924_9249_2492_4924_9249_2492_4924_9249_2492_4924_9249_2492, 0x0000_0000_0000_0001
    0x0000_0000_0000_0000_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff /,% 0x0000_0000_0000_ffff -> 0x0001_0001_0001_0001_0001_0001_0001_0001_0001_0001_0001_0001_0001_0001_0001_0001, 0x0000_0000_0000_0000
    0x0000_0000_0000_0000_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff /,% 0x0000_0000_ffff_ffff -> 0x0000_0001_0000_0001_0000_0001_0000_0001_0000_0001_0000_0001_0000_0001_0000_0001, 0x0000_0000_0000_0000
    0x0000_0001_0000_0000 /,% 0x0000_0001_0000_0000 -> 0x0000_0000_0000_0001, 0x0000_0000_0000_0000
    0x0000_0001_ffff_ffff /,% 0x0000_0001_0000_0000 -> 0x0000_0000_0000_0001, 0x0000_0000_ffff_ffff
    0x0000_0002_ffff_ffff /,% 0x0000_0001_0000_0000 -> 0x0000_0000_0000_0002, 0x0000_0000_ffff_ffff
    0x0000_0000_0000_0000_ffff_ffff_ffff_ffff /,% 0x0000_0000_0000_0000_ffff_ffff_ffff_ffff -> 0x0000_0000_0000_0001, 0x0000_0000_0000_0000
    0x0000_0000_0000_0000_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff /,% 0x0000_0000_0000_0000_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff -> 0x0000_0000_0000_0001, 0x0000_0000_0000_0000
    0x0000_0000_0000_0000_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff /,% 0x0000_0000_0000_0000_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff -> 0x0000_0000_0000_0001, 0x0000_0000_0000_0000
    0x0000_0000_0000_0000_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff /,% 0x0000_0000_0000_0000_ffff_ffff_ffff_ffff -> 0x0000_0000_0000_0001_0000_0000_0000_0001, 0x0000_0000_0000_0000
    0x0000_0000_0000_0000_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff /,% 0x0000_0000_0000_0000_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff -> 0x0000_0000_0000_0001_0000_0000_0000_0000_0000_0000_0000_0001, 0x0000_0000_0000_0000
    |}]

let%expect_test "Z bit_and,bit_or,bit_xor" =
  let open Z in
  let open Format in
  printf "@[<h>";
  let rec test_pairs = function
    | [] -> ()
    | (x, y) :: pairs' -> begin
        printf "bit_{and,or,xor} %a %a -> %a, %a, %a\n"
          pp_x x pp_x y
          pp_x (bit_and x y)
          pp_x (bit_or x y)
          pp_x (bit_xor x y);
        test_pairs pairs'
      end
  in
  let pairs = [
    (zero, zero);
    (neg one, zero);
    (zero, neg one);
    (neg one, neg one);
  ] in
  test_pairs pairs;
  printf "@]";

  [%expect{|
    bit_{and,or,xor} 0x0000_0000_0000_0000 0x0000_0000_0000_0000 -> 0x0000_0000_0000_0000, 0x0000_0000_0000_0000, 0x0000_0000_0000_0000
    bit_{and,or,xor} 0xffff_ffff_ffff_ffff 0x0000_0000_0000_0000 -> 0x0000_0000_0000_0000, 0xffff_ffff_ffff_ffff, 0xffff_ffff_ffff_ffff
    bit_{and,or,xor} 0x0000_0000_0000_0000 0xffff_ffff_ffff_ffff -> 0x0000_0000_0000_0000, 0xffff_ffff_ffff_ffff, 0xffff_ffff_ffff_ffff
    bit_{and,or,xor} 0xffff_ffff_ffff_ffff 0xffff_ffff_ffff_ffff -> 0xffff_ffff_ffff_ffff, 0xffff_ffff_ffff_ffff, 0x0000_0000_0000_0000
    |}]

let%expect_test "Z bit_not" =
  let open Z in
  let open Format in
  printf "@[<h>";
  let rec test = function
    | [] -> ()
    | x :: xs' -> begin
        printf "bit_not %a -> %a\n"
          pp_x x pp_x (bit_not x);
        test xs'
      end
  in
  let xs = [
    zero; (* No bits, therefore the bitwise not is also no bits. *)
    one;
    (neg one)
  ] in
  test xs;
  printf "@]";

  [%expect{|
    bit_not 0x0000_0000_0000_0000 -> 0x0000_0000_0000_0000
    bit_not 0x0000_0000_0000_0001 -> 0xffff_ffff_ffff_fffe
    bit_not 0xffff_ffff_ffff_ffff -> 0x0000_0000_0000_0000
    |}]

let%expect_test "Z bit_pop,bit_clz,bit_ctz" =
  let open Z in
  let open Format in
  printf "@[<h>";
  let rec test = function
    | [] -> ()
    | x :: xs' -> begin
        printf "bit_length, bit_{pop,clz,ctz} %a -> %u, %u, %u, %u\n"
          pp_x x (bit_length x) (bit_pop x) (bit_clz x) (bit_ctz x);
        test xs'
      end
  in
  let xs = [
    of_string "0";
    of_string "1";
    of_string "0x8000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000";
    of_string "0xffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff"
  ] in
  test xs;
  printf "@]";

  [%expect{|
    bit_length, bit_{pop,clz,ctz} 0x0000_0000_0000_0000 -> 0, 0, 0, 0
    bit_length, bit_{pop,clz,ctz} 0x0000_0000_0000_0001 -> 64, 1, 63, 0
    bit_length, bit_{pop,clz,ctz} 0x0000_0000_0000_0000_8000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000 -> 320, 1, 64, 255
    bit_length, bit_{pop,clz,ctz} 0x0000_0000_0000_0000_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff -> 320, 256, 64, 0
    |}]

let%expect_test "Z **" =
  let open Z in
  let open Format in
  printf "@[<h>";
  let rec test_pairs = function
    | [] -> ()
    | (x, y) :: pairs' -> begin
        printf "%a ** %a -> %a\n" pp_x x pp_x y pp_x (x ** y);
        test_pairs pairs'
      end
  in
  let pairs = [
    (of_string "0", of_string "0");
    (of_string "0", of_string "1");

    (of_string
        "0xffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff",
      of_string "0");
    (of_string
        "0xffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff",
      of_string "1");

    (of_string "2", of_string "31");
    (of_string "2", of_string "32");
    (of_string "2", of_string "63");
    (of_string "2", of_string "64");
    (of_string "2", of_string "127");
    (of_string "2", of_string "128");
    (of_string "2", of_string "255");
    (of_string "2", of_string "256");

    (of_string "0xf", of_string "0xf");
    (of_string "0xff", of_string "0xff");

    (of_string "1", of_string
        "0xffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff");
    (of_string "0x1ff", of_string "0x1ff");
  ] in
  test_pairs pairs;
  printf "@]";

  [%expect{|
    0x0000_0000_0000_0000 ** 0x0000_0000_0000_0000 -> 0x0000_0000_0000_0001
    0x0000_0000_0000_0000 ** 0x0000_0000_0000_0001 -> 0x0000_0000_0000_0000
    0x0000_0000_0000_0000_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff ** 0x0000_0000_0000_0000 -> 0x0000_0000_0000_0001
    0x0000_0000_0000_0000_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff ** 0x0000_0000_0000_0001 -> 0x0000_0000_0000_0000_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff
    0x0000_0000_0000_0002 ** 0x0000_0000_0000_001f -> 0x0000_0000_8000_0000
    0x0000_0000_0000_0002 ** 0x0000_0000_0000_0020 -> 0x0000_0001_0000_0000
    0x0000_0000_0000_0002 ** 0x0000_0000_0000_003f -> 0x0000_0000_0000_0000_8000_0000_0000_0000
    0x0000_0000_0000_0002 ** 0x0000_0000_0000_0040 -> 0x0000_0000_0000_0001_0000_0000_0000_0000
    0x0000_0000_0000_0002 ** 0x0000_0000_0000_007f -> 0x0000_0000_0000_0000_8000_0000_0000_0000_0000_0000_0000_0000
    0x0000_0000_0000_0002 ** 0x0000_0000_0000_0080 -> 0x0000_0000_0000_0001_0000_0000_0000_0000_0000_0000_0000_0000
    0x0000_0000_0000_0002 ** 0x0000_0000_0000_00ff -> 0x0000_0000_0000_0000_8000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000
    0x0000_0000_0000_0002 ** 0x0000_0000_0000_0100 -> 0x0000_0000_0000_0001_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000
    0x0000_0000_0000_000f ** 0x0000_0000_0000_000f -> 0x0613_b62c_5977_07ef
    0x0000_0000_0000_00ff ** 0x0000_0000_0000_00ff -> 0x005e_5c8b_0eb9_5ab0_8f9d_37ef_127f_c01b_d0e3_3de5_2647_5283_96d7_8d5f_8da3_1989_e678_14f6_bba1_fb0f_0207_010f_f5f2_347b_19d5_f659_8fc9_1bf5_a88f_77da_a3d7_b382_fec4_84f3_d205_c06a_3444_5384_c0e7_ab0d_8837_88c6_8c01_2cb4_3305_5edd_a746_a484_0944_4ea9_1147_273b_79fc_3eab_b70e_ca55_2af6_50c2_34bb_01ed_4044_27f1_7cdd_dd71_d08e_39ef_9c39_82e3_ce44_e670_456a_a815_4c1f_dbd9_c359_47f4_9463_6a42_5c69_bf89_e9c7_5ad3_b7a0_a559_af0f_5da9_947c_8deb_a644_1731_0713_b23e_7ef4_de50_bb2a_3e90_bc2a_c3da_5201_cca8_d6e5_dfea_887c_4f7a_4e92_175d_9f88_bd27_79b5_7f9e_b35b_e752_8f96_5a06_da0a_c41d_cb3a_34f1_d8ab_7d8f_ee62_0a94_faa4_2c39_5997_756b_007f_feff
    0x0000_0000_0000_0001 ** 0x0000_0000_0000_0000_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff -> 0x0000_0000_0000_0001
    0x0000_0000_0000_01ff ** 0x0000_0000_0000_01ff -> 0x002f_2275_878c_70d5_a1b1_bd32_e9be_69ee_6031_0850_8abd_3586_c249_5d97_655c_39cd_c7f1_40f1_9cec_df9e_e0e3_be45_8a7c_0dc0_1c8d_dd04_c2de_effd_7c1a_b167_fbf1_6b25_77cb_6903_363f_4655_f97a_98ed_fc03_be77_953f_2575_d2df_3f79_532a_4751_69ec_77ca_9fc9_ee90_804f_4901_1bd9_13f2_a6f3_556c_d9fb_12ca_faa0_aef3_01eb_4243_3745_f677_fd48_03e6_8898_f3ed_b912_10af_068f_11c1_2919_2cfc_091c_6735_3d37_31b6_3c1b_1fe7_140f_f6b2_dcd5_26c6_9417_ee7d_d0de_62d3_f880_07ce_bd5b_a430_5f1e_96fb_916e_4b70_8643_a1d8_e5e9_b51c_d1d4_f54e_afe6_c65e_3db3_b671_d474_879e_5b70_3c51_81bc_31b0_6a0c_eb7d_c5b1_905a_a965_e795_1c7a_9dae_29cc_e8e0_e45f_1c22_d5bf_064b_7b51_e368_aa7a_f272_92ef_6580_7d37_1e7d_23dd_3a15_d100_ebbc_159a_aabd_315b_26bf_880d_aa1e_ab90_20c3_c0e2_d0eb_dedc_4b7f_3475_a98d_27be_0705_5a8e_c158_aa7f_07ec_a308_c70f_0dbe_8266_bd11_9bfe_644c_9468_8674_b1aa_6d74_d0fd_665d_b7b4_db73_3247_7fc8_c1ac_c162_13d7_f291_419b_0b46_3c4f_59eb_2bf5_c6bf_1270_06a8_65ca_a5e8_a46f_6b0f_33c0_21f6_7dd8_8fb7_f31b_8417_2e31_d1ac_e7ed_f662_d734_848b_4e33_d4f4_9836_8a4f_3160_cb65_ff93_9efc_a84c_3042_dc1f_6e97_6bf0_87bb_28d8_3b14_f834_03c0_cfb4_07ca_e0ed_6cb6_e2ea_ac2b_dfa8_cfca_39b0_010a_b6ed_7497_9fb5_a43d_e2c3_70ab_b67a_2c72_9a9c_bf22_a838_e4dd_0ce4_36dd_99c9_1a9d_6222_dceb_964c_0b7b_71ec_be59_f303_f1e5_f808_dae1_3bea_1dae_d8aa_c976_476d_5e0d_1376_b2ac_4b3d_9be1_8d37_eebd_9742_6b55_f5c9_d27e_af9b_92e2_7bd2_b430_fa73_4fdf_d326_44a2_b762_b132_e959_9888_02b0_03ff_fdff
    |}]

let%expect_test "Z is_pow2" =
  let open Z in
  let open Format in
  printf "@[<h>";
  let rec test = function
    | [] -> ()
    | u :: us' -> begin
        printf "is_pow2 %a -> %b\n"
          pp_x u
          (is_pow2 u);
        test us'
      end
  in
  let us = [
    of_string "0";
    of_string "1";
    of_string "2";
    of_string "3";
    of_string
      "0x8000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000";
    of_string
      "0xffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff"
  ] in
  test us;
  printf "@]";

  [%expect{|
    is_pow2 0x0000_0000_0000_0000 -> false
    is_pow2 0x0000_0000_0000_0001 -> true
    is_pow2 0x0000_0000_0000_0002 -> true
    is_pow2 0x0000_0000_0000_0003 -> false
    is_pow2 0x0000_0000_0000_0000_8000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000 -> true
    is_pow2 0x0000_0000_0000_0000_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff -> false
    |}]

let%expect_test "Z floor_pow2,ceil_pow2" =
  let open Z in
  let open Format in
  printf "@[<h>";
  let rec test = function
    | [] -> ()
    | u :: us' -> begin
        printf "floor_pow2,ceil_pow2 %a -> %a, %a\n"
          pp_x u
          pp_x (floor_pow2 u)
          pp_x (ceil_pow2 u);
        test us'
      end
  in
  let us = [
    of_string "1";
    of_string "2";
    of_string "3";

    of_string "0x8000_0000_0000_0000";
    of_string "0xffff_ffff_ffff_ffff";

    of_string "0x8000_0000_0000_0000_0000_0000_0000_0000";
    of_string "0xffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff";

    of_string
      "0x8000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000";
    of_string
      "0xffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff"
  ] in
  test us;
  printf "@]";

  [%expect{|
    floor_pow2,ceil_pow2 0x0000_0000_0000_0001 -> 0x0000_0000_0000_0001, 0x0000_0000_0000_0001
    floor_pow2,ceil_pow2 0x0000_0000_0000_0002 -> 0x0000_0000_0000_0002, 0x0000_0000_0000_0002
    floor_pow2,ceil_pow2 0x0000_0000_0000_0003 -> 0x0000_0000_0000_0002, 0x0000_0000_0000_0004
    floor_pow2,ceil_pow2 0x0000_0000_0000_0000_8000_0000_0000_0000 -> 0x0000_0000_0000_0000_8000_0000_0000_0000, 0x0000_0000_0000_0000_8000_0000_0000_0000
    floor_pow2,ceil_pow2 0x0000_0000_0000_0000_ffff_ffff_ffff_ffff -> 0x0000_0000_0000_0000_8000_0000_0000_0000, 0x0000_0000_0000_0001_0000_0000_0000_0000
    floor_pow2,ceil_pow2 0x0000_0000_0000_0000_8000_0000_0000_0000_0000_0000_0000_0000 -> 0x0000_0000_0000_0000_8000_0000_0000_0000_0000_0000_0000_0000, 0x0000_0000_0000_0000_8000_0000_0000_0000_0000_0000_0000_0000
    floor_pow2,ceil_pow2 0x0000_0000_0000_0000_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff -> 0x0000_0000_0000_0000_8000_0000_0000_0000_0000_0000_0000_0000, 0x0000_0000_0000_0001_0000_0000_0000_0000_0000_0000_0000_0000
    floor_pow2,ceil_pow2 0x0000_0000_0000_0000_8000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000 -> 0x0000_0000_0000_0000_8000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000, 0x0000_0000_0000_0000_8000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000
    floor_pow2,ceil_pow2 0x0000_0000_0000_0000_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff -> 0x0000_0000_0000_0000_8000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000, 0x0000_0000_0000_0001_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000
    |}]

let%expect_test "Z floor_lg,ceil_lg" =
  let open Z in
  let open Format in
  printf "@[<h>";
  let rec test = function
    | [] -> ()
    | u :: us' -> begin
        printf "floor_lg,ceil_lg %a -> %a, %a\n"
          pp_x u
          pp (floor_lg u)
          pp (ceil_lg u);
        test us'
      end
  in
  let us = [
    of_string "1";
    of_string "2";
    of_string "3";
    of_string
      "0x8000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000";
    of_string
      "0xffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff"
  ] in
  test us;
  printf "@]";

  [%expect{|
        floor_lg,ceil_lg 0x0000_0000_0000_0001 -> 0, 0
        floor_lg,ceil_lg 0x0000_0000_0000_0002 -> 1, 1
        floor_lg,ceil_lg 0x0000_0000_0000_0003 -> 1, 2
        floor_lg,ceil_lg 0x0000_0000_0000_0000_8000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000 -> 255, 255
        floor_lg,ceil_lg 0x0000_0000_0000_0000_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff -> 255, 256
    |}]

let%expect_test "Z min,max" =
  let open Z in
  let open Format in
  printf "@[<h>";
  let rec test_pairs = function
    | [] -> ()
    | (x, y) :: pairs' -> begin
        printf "min,max %a %a -> %a, %a\n"
          pp_x x pp_x y pp_x (min x y) pp_x (max x y);
        test_pairs pairs'
      end
  in
  let pairs = [
    (of_string "0", of_string "0");
    (of_string "0", of_string "1");
    (of_string "1", of_string "0");
    (of_string "1", of_string "1");
    (of_string "0", of_string
        "0xffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff");
  ] in
  test_pairs pairs;
  printf "@]";

  [%expect{|
    min,max 0x0000_0000_0000_0000 0x0000_0000_0000_0000 -> 0x0000_0000_0000_0000, 0x0000_0000_0000_0000
    min,max 0x0000_0000_0000_0000 0x0000_0000_0000_0001 -> 0x0000_0000_0000_0000, 0x0000_0000_0000_0001
    min,max 0x0000_0000_0000_0001 0x0000_0000_0000_0000 -> 0x0000_0000_0000_0000, 0x0000_0000_0000_0001
    min,max 0x0000_0000_0000_0001 0x0000_0000_0000_0001 -> 0x0000_0000_0000_0001, 0x0000_0000_0000_0001
    min,max 0x0000_0000_0000_0000 0x0000_0000_0000_0000_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff -> 0x0000_0000_0000_0000, 0x0000_0000_0000_0000_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff
   |}]

let%expect_test "Z neg,abs" =
  let open Z in
  let open Format in
  printf "@[<h>";
  let rec test = function
    | [] -> ()
    | u :: us' -> begin
        let fn u = begin
          printf "neg,abs %a -> %a, %a\n"
            pp_x u
            pp_x (neg u)
            pp_x (abs u)
        end in
        fn u;
        fn (neg u);
        test us'
      end
  in
  let us = [
    of_string "0";
    of_string "1";
    of_string "2";
    of_string "3";
    of_string "0x8000_0000_0000_0000";
    of_string "0xffff_ffff_ffff_ffff"
  ] in
  test us;
  printf "@]";

  [%expect{|
    neg,abs 0x0000_0000_0000_0000 -> 0x0000_0000_0000_0000, 0x0000_0000_0000_0000
    neg,abs 0x0000_0000_0000_0000 -> 0x0000_0000_0000_0000, 0x0000_0000_0000_0000
    neg,abs 0x0000_0000_0000_0001 -> 0xffff_ffff_ffff_ffff, 0x0000_0000_0000_0001
    neg,abs 0xffff_ffff_ffff_ffff -> 0x0000_0000_0000_0001, 0x0000_0000_0000_0001
    neg,abs 0x0000_0000_0000_0002 -> 0xffff_ffff_ffff_fffe, 0x0000_0000_0000_0002
    neg,abs 0xffff_ffff_ffff_fffe -> 0x0000_0000_0000_0002, 0x0000_0000_0000_0002
    neg,abs 0x0000_0000_0000_0003 -> 0xffff_ffff_ffff_fffd, 0x0000_0000_0000_0003
    neg,abs 0xffff_ffff_ffff_fffd -> 0x0000_0000_0000_0003, 0x0000_0000_0000_0003
    neg,abs 0x0000_0000_0000_0000_8000_0000_0000_0000 -> 0x8000_0000_0000_0000, 0x0000_0000_0000_0000_8000_0000_0000_0000
    neg,abs 0x8000_0000_0000_0000 -> 0x0000_0000_0000_0000_8000_0000_0000_0000, 0x0000_0000_0000_0000_8000_0000_0000_0000
    neg,abs 0x0000_0000_0000_0000_ffff_ffff_ffff_ffff -> 0xffff_ffff_ffff_ffff_0000_0000_0000_0001, 0x0000_0000_0000_0000_ffff_ffff_ffff_ffff
    neg,abs 0xffff_ffff_ffff_ffff_0000_0000_0000_0001 -> 0x0000_0000_0000_0000_ffff_ffff_ffff_ffff, 0x0000_0000_0000_0000_ffff_ffff_ffff_ffff
    |}]

let%expect_test "Z to_i64,to_sint" =
  let open Z in
  let open Format in
  printf "@[<h>";
  let rec test = function
    | [] -> ()
    | u :: us' -> begin
        let fn u = begin
          printf "to_i64,to_sint %a -> %a, %a\n"
            pp_x u
            I64.pp_x (to_i64 u)
            Sint.pp_x (to_sint u)
        end in
        fn u;
        fn (neg u);
        test us'
      end
  in
  let us = [
    of_string "0";
    of_string "1";
    of_string "2";
    of_string "3";
    of_string "0x8000_0000_0000_0000";
    of_string "0xffff_ffff_ffff_ffff"
  ] in
  test us;
  printf "@]";

  [%expect{|
    to_i64,to_sint 0x0000_0000_0000_0000 -> 0x0000_0000_0000_0000i64, 0x0000000000000000i
    to_i64,to_sint 0x0000_0000_0000_0000 -> 0x0000_0000_0000_0000i64, 0x0000000000000000i
    to_i64,to_sint 0x0000_0000_0000_0001 -> 0x0000_0000_0000_0001i64, 0x0000000000000001i
    to_i64,to_sint 0xffff_ffff_ffff_ffff -> 0xffff_ffff_ffff_ffffi64, 0x7fffffffffffffffi
    to_i64,to_sint 0x0000_0000_0000_0002 -> 0x0000_0000_0000_0002i64, 0x0000000000000002i
    to_i64,to_sint 0xffff_ffff_ffff_fffe -> 0xffff_ffff_ffff_fffei64, 0x7ffffffffffffffei
    to_i64,to_sint 0x0000_0000_0000_0003 -> 0x0000_0000_0000_0003i64, 0x0000000000000003i
    to_i64,to_sint 0xffff_ffff_ffff_fffd -> 0xffff_ffff_ffff_fffdi64, 0x7ffffffffffffffdi
    to_i64,to_sint 0x0000_0000_0000_0000_8000_0000_0000_0000 -> 0x8000_0000_0000_0000i64, 0x0000000000000000i
    to_i64,to_sint 0x8000_0000_0000_0000 -> 0x8000_0000_0000_0000i64, 0x0000000000000000i
    to_i64,to_sint 0x0000_0000_0000_0000_ffff_ffff_ffff_ffff -> 0xffff_ffff_ffff_ffffi64, 0x7fffffffffffffffi
    to_i64,to_sint 0xffff_ffff_ffff_ffff_0000_0000_0000_0001 -> 0x0000_0000_0000_0001i64, 0x0000000000000001i
    |}]
