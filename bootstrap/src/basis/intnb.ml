open RudimentsFunctions
open IntnbIntf

external intnb_icmp: uns -> uns -> Cmp.t = "hemlock_intnb_icmp"
external intnb_ucmp: uns -> uns -> Cmp.t = "hemlock_intnb_ucmp"

module MakeDerived (T : IDerived) : SDerived with type t := T.t = struct
  include Cmpable.MakeZero(T)

  let is_pow2 t =
    match t > T.zero with
    | false -> false
    | true -> (T.bit_and t T.(t - one)) = T.zero

  let floor_pow2 t =
    match T.cmp t T.one with
    | Lt -> halt "Invalid input"
    | Eq -> t
    | Gt -> begin
        let nb = T.bit_length in
        let lz = T.bit_clz t in
        T.bit_sl ~shift:Int64.(sub (pred nb) lz) T.one
      end

  let ceil_pow2 t =
    match T.cmp t T.one with
    | Lt -> halt "Invalid input"
    | Eq -> t
    | Gt -> begin
        let nb = T.bit_length in
        match T.bit_clz T.(t - one) with
        | 0L -> T.zero
        | lz -> T.bit_sl ~shift:Int64.(sub nb lz) T.one
      end

  let floor_lg_opt t =
    match T.cmp t T.zero with
    | Lt | Eq -> None
    | Gt -> begin
        let nb = T.bit_length in
        let lz = T.bit_clz t in
        Some (T.of_uns Int64.(sub (pred nb) lz))
      end

  let floor_lg t =
    match floor_lg_opt t with
    | None -> halt "Invalid input"
    | Some x -> x

  let ceil_lg t =
    match floor_lg_opt t with
    | None -> halt "Invalid input"
    | Some x -> T.(x + (if is_pow2 t then T.zero else T.one))

  let min t0 t1 =
    if t0 <= t1 then t0 else t1

  let max t0 t1 =
    if t0 < t1 then t1 else t0
end

module type ICommon = sig
  include I
  val signed: bool
end

module type SCommon = sig
  type t

  include S with type t := t
  include SNarrow with type t := t

  val narrow: t -> int64
  val widen: t -> int64
end

module MakeCommon (T : ICommon) : SCommon with type t := uns = struct
  module U = struct
    type t = int64

    let hash_fold t state =
      Hash.State.Gen.init state
      |> Hash.State.Gen.fold_u64 1L ~f:(fun _ -> t)
      |> Hash.State.Gen.fini

    let widen t =
      let nlb = Int64.(sub 64L T.bit_length) in
      match nlb with
      | 0L -> t
      | _ -> begin
          match T.signed with
          | false -> Int64.(logand (pred (shift_left 1L (to_int T.bit_length))) t)
          | true -> Int64.(shift_right (shift_left t (to_int nlb)) (to_int nlb))
        end

    let narrow t =
      let nlb = Int64.(sub 64L T.bit_length) in
      match nlb with
      | 0L -> t
      | _ -> begin
          match T.signed with
          | false -> Int64.(logand (pred (shift_left 1L (to_int T.bit_length))) t)
          | true ->
            Int64.(logor (shift_left (shift_right t 63) (to_int (pred T.bit_length))) (logand (pred
                (shift_left 1L (to_int (pred T.bit_length)))) t))
        end

    let cmp t0 t1 =
      assert (Int64.(compare (narrow t0) t0) = 0);
      assert (Int64.(compare (narrow t1) t1) = 0);
      match T.signed || Stdlib.(Int64.(compare 64L T.bit_length) > 0) with
      | true -> intnb_icmp t0 t1
      | false -> intnb_ucmp t0 t1

    let narrow_of_signed s =
      match T.signed with
      | true -> narrow (uns_of_sint s)
      | false -> begin
          let nlb = Int64.(sub 64L T.bit_length) in
          match nlb with
          | 0L -> uns_of_sint s
          | _ -> Int64.(logand (pred (shift_left 1L (to_int T.bit_length))) (uns_of_sint s))
        end

    let narrow_of_unsigned u =
      match T.signed with
      | true -> begin
          let nlb = Int64.(sub 64L T.bit_length) in
          match nlb with
          | 0L -> u
          | _ -> Int64.(shift_right (shift_left u (to_int nlb)) (to_int nlb))
        end
      | false -> narrow u

    let lbfill t =
      let nlb = Int64.(sub 64L T.bit_length) in
      match nlb with
      | 0L -> t
      | _ -> begin
          let mask = Int64.(pred (shift_left 1L (to_int T.bit_length))) in
          let lb = Int64.(lognot mask) in
          Int64.(logor lb t)
        end

    let lbclear t =
      match T.signed with
      | false -> t
      | true -> begin
          let nlb = Int64.(sub 64L T.bit_length) in
          match nlb with
          | 0L -> begin
              (* Leading bits are already zeros. *)
              assert Stdlib.(Int64.(compare (narrow t) t) = 0);
              t
            end
          | _ -> begin
              let mask = Int64.(pred (shift_left 1L (to_int T.bit_length))) in
              Int64.(logand t mask)
            end
        end

    let of_real r =
      (* OCaml handles overflow poorly, but this deficiency has no anticipated impact on
       * bootstrapping. *)
      match T.signed || (r >= 0.) with
      | true -> narrow (Int64.of_float r)
      | false -> 0L

    let to_real t =
      assert Stdlib.(Int64.(compare (narrow t) t) = 0);
      match T.signed with
      | true -> Int64.to_float t
      | false -> Int64.(to_float (shift_right_logical t 1)) *. 2. +. Int64.(to_float (logand t 1L))

    let pp ppf t =
      assert Stdlib.(Int64.(compare (narrow t) t) = 0);
      let nlb = Int64.(sub 64L T.bit_length) in
      match T.signed, nlb with
      | false, 0L -> Format.fprintf ppf "%Lu" t
      | true, 0L -> Format.fprintf ppf "%Ldi" t
      | false, _ -> Format.fprintf ppf "%Luu%Lu" t T.bit_length
      | true, _ -> Format.fprintf ppf "%Ldi%Lu" t T.bit_length

    let pp_b ppf t =
      assert Stdlib.(Int64.(compare (narrow t) t) = 0);
      let nlb = Int64.(sub 64L T.bit_length) in
      let tbits =
        Int64.(logand (pred (shift_left 1L (to_int T.bit_length))) t) in (* Mask leading ones. *)
      let rec string_of_bits bits i accum = begin
        match i = T.bit_length with
        | true -> Stdlib.String.concat "" accum
        | false -> begin
            let bits' = Int64.shift_right_logical bits 1 in
            let i' = Int64.succ i in
            let accum' = Int64.(to_string (logand bits 1L)) :: accum in
            string_of_bits bits' i' accum'
          end
      end in
      let bits_string = string_of_bits tbits 0L [] in
      match T.signed, nlb with
      | false, 0L -> Format.fprintf ppf "0b%s" bits_string
      | true, 0L -> Format.fprintf ppf "0b%si" bits_string
      | false, _ -> Format.fprintf ppf "0b%su%Lu" bits_string T.bit_length
      | true, _ -> Format.fprintf ppf "0b%si%Lu" bits_string T.bit_length

    let pp_o ppf t =
      assert Stdlib.(Int64.(compare (narrow t) t) = 0);
      let nlb = Int64.(sub 64L T.bit_length) in
      let oct_digits = ((Int64.to_int T.bit_length) + 2) / 3 in
      let tbits =
        Int64.(logand (pred (shift_left 1L (to_int T.bit_length))) t) in (* Mask leading ones. *)
      match T.signed, nlb with
      | false, 0L -> Format.fprintf ppf "0o%0*Lo" oct_digits tbits
      | true, 0L -> Format.fprintf ppf "0o%0*Loi" oct_digits tbits
      | false, _ -> Format.fprintf ppf "0o%0*Lou%Lu" oct_digits tbits T.bit_length
      | true, _ -> Format.fprintf ppf "0o%0*Loi%Lu" oct_digits tbits T.bit_length

    let pp_x ppf t =
      assert Stdlib.(Int64.(compare (narrow t) t) = 0);
      let nlb = Int64.(sub 64L T.bit_length) in
      let hex_digits = ((Int64.to_int T.bit_length) + 3) / 4 in
      let tbits =
        Int64.(logand (pred (shift_left 1L (to_int T.bit_length))) t) in (* Mask leading ones. *)
      match T.signed, nlb with
      | false, 0L -> Format.fprintf ppf "0x%0*Lx" hex_digits tbits
      | true, 0L -> Format.fprintf ppf "0x%0*Lxi" hex_digits tbits
      | false, _ -> Format.fprintf ppf "0x%0*Lxu%Lu" hex_digits tbits T.bit_length
      | true, _ -> Format.fprintf ppf "0x%0*Lxi%Lu" hex_digits tbits T.bit_length

    let of_string s =
      narrow (Int64.of_string s)

    let to_string ?(sign=Fmt.sign_default) ?(alt=Fmt.alt_default) ?(zpad=Fmt.zpad_default)
      ?(width=Fmt.width_default) ?(base=Fmt.base_default) t =
      assert Stdlib.(Int64.(compare (narrow t) t) = 0);
      let rec fn accum ndigits is_neg t = begin
        match Stdlib.(Int64.(unsigned_compare t 0L) = 0)
              && Stdlib.(not zpad || (ndigits >= (Int64.to_int width))) with
        | true -> begin
            (match sign, is_neg with
              | Implicit, false -> ""
              | Explicit, false -> "+"
              | Space, false -> " "
              | _, true -> "-"
            )
            ^ (match alt with
              | true -> begin
                  match base with
                  | Bin -> "0b"
                  | Oct -> "0o"
                  | Dec -> ""
                  | Hex -> "0x"
                end
              | false -> ""
            )
            ^ (Stdlib.String.concat "" (match ndigits with 0 -> ["0"] | _ -> accum))
            ^ (match T.signed with false -> "u" | true -> "i")
            ^ (Int64.to_string T.bit_length)
          end
        | _ -> begin
            let divisor, group = match base with
              | Bin -> 2L, 8
              | Oct -> 8L, 3
              | Dec -> 10L, 3
              | Hex -> 16L, 4
            in
            let sep = match alt && Stdlib.(ndigits > 0) && Stdlib.((ndigits mod group) = 0) with
              | true -> ["_"]
              | false -> []
            in
            let digit = Stdlib.String.init 1 (fun _ ->
              (Stdlib.String.get "0123456789abcdef" Int64.(to_int (unsigned_rem t divisor)))) in
            let t' = Int64.unsigned_div t divisor in
            fn (digit :: (sep @ accum)) Stdlib.(succ ndigits) is_neg t'
          end
      end in
      match T.signed && Stdlib.(Int64.(compare t 0L) < 0) with
      | false -> fn [] 0 false t
      | true -> fn [] 0 true (Stdlib.Int64.neg t)

    let xfmt ?pad ?just ?sign ?alt ?zpad ?width ?base t ((module Formatter):(module Fmt.Formatter))
      : (module Fmt.Formatter) =
      Fmt.xfmt ?pad ?just ?width (to_string ?sign ?alt ?zpad ?width ?base t) (module Formatter)

    let fmt t formatter =
      xfmt t formatter

    let zero = 0L

    let one = 1L

    let min_value = zero

    let max_value = narrow Int64.(sub zero one)

    let succ t =
      widen Int64.(succ t)

    let pred t =
      widen Int64.(pred t)

    let bit_and t0 t1 =
      widen Int64.(logand t0 t1)

    let bit_or t0 t1 =
      widen Int64.(logor t0 t1)

    let bit_xor t0 t1 =
      widen Int64.(logxor t0 t1)

    let bit_not t =
      widen Int64.(lognot t)

    let bit_sl ~shift t =
      widen Int64.(shift_left t (to_int shift))

    let bit_usr ~shift t =
      widen Int64.(shift_right_logical t (to_int shift))

    let bit_ssr ~shift t =
      widen Int64.(shift_right t (to_int shift))

    let bit_pop t =
      let x = lbclear t in
      let x = Int64.(sub x (logand (shift_right x 1) 0x5555_5555_5555_5555L)) in
      let c3s = 0x3333_3333_3333_3333L in
      let x = Int64.(add (logand x c3s) (logand (shift_right_logical x 2) c3s)) in
      let x = Int64.(logand (add x (shift_right_logical x 4)) 0x0f0f_0f0f_0f0f_0f0fL) in
      let x = Int64.(add x (shift_right_logical x 8)) in
      let x = Int64.(add x (shift_right_logical x 16)) in
      let x = Int64.(add x (shift_right_logical x 32)) in
      Int64.(logand x 0x3fL)

    let bit_clz t =
      let x = lbclear t in
      let x = Int64.(logor x (shift_right_logical x 1)) in
      let x = Int64.(logor x (shift_right_logical x 2)) in
      let x = Int64.(logor x (shift_right_logical x 4)) in
      let x = Int64.(logor x (shift_right_logical x 8)) in
      let x = Int64.(logor x (shift_right_logical x 16)) in
      let x = Int64.(logor x (shift_right_logical x 32)) in
      bit_pop (bit_not x)

    let bit_ctz t =
      let t' = lbfill t in
      bit_pop (bit_and (bit_not t') Int64.(pred t'))

    let ( + ) t0 t1 =
      widen Int64.(add t0 t1)

    let ( - ) t0 t1 =
      widen Int64.(sub t0 t1)

    let ( * ) t0 t1 =
      widen Int64.(mul t0 t1)

    let ( / ) t0 t1 =
      widen Int64.(div t0 t1)

    let ( % ) t0 t1 =
      widen Int64.(rem t0 t1)

    let ( ** ) t0 t1 =
      (* Decompose the exponent to limit algorithmic complexity. *)
      let neg, n = if T.signed && Stdlib.(Int64.(compare t1 0L) < 0) then
          true, Int64.(neg t1)
        else
          false, t1
      in
      let rec fn r p n = begin
        match n with
        | 0L -> r
        | _ -> begin
            let r' = match bit_and n 1L with
              | 0L -> r
              | 1L -> r * p
              | _ -> assert false
            in
            let p' = Int64.(mul p p) in
            let n' = bit_usr ~shift:1L n in
            fn r' p' n'
          end
      end in
      let r = fn 1L t0 n in
      narrow (
        match neg with
        | false -> r
        | true -> Int64.(div 1L r)
      )

    let ( // ) t0 t1 =
      assert Stdlib.(Int64.(compare (narrow t0) t0) = 0);
      assert Stdlib.(Int64.(compare (narrow t1) t1) = 0);
      (to_real t0) /. (to_real t1)
  end
  include U
  include Identifiable.Make(U)
  include Cmpable.MakeZero(U)
  module V = struct
    include U

    let bit_length = T.bit_length

    let of_uns t =
      U.narrow_of_unsigned t
  end
  include MakeDerived(V)
end

module MakeI (T : I) : SI with type t := sint = struct
  module U = struct
    module V = struct
      module W = struct
        type t = sint
        let bit_length = T.bit_length
        let signed = true
      end
      include W
      include MakeCommon(W)
    end
    type t = sint

    let hash_fold t state =
      state
      |> V.hash_fold (uns_of_sint t)

    let cmp t0 t1 =
      V.cmp (uns_of_sint t0) (uns_of_sint t1)

    let narrow_of_signed x =
      sint_of_uns (V.narrow_of_signed x)

    let narrow_of_unsigned x =
      sint_of_uns (V.narrow_of_unsigned x)

    let widen t =
      V.widen t

    let of_real r =
      sint_of_uns (V.of_real r)

    let to_real t =
      V.to_real t

    let of_string s =
      sint_of_uns (V.of_string s)

    let pp ppf t =
      V.pp ppf (uns_of_sint t)

    let pp_b ppf t =
      V.pp_b ppf (uns_of_sint t)

    let pp_o ppf t =
      V.pp_o ppf (uns_of_sint t)

    let pp_x ppf t =
      V.pp_x ppf (uns_of_sint t)


    let to_string ?sign ?alt ?zpad ?width ?base t =
      V.to_string ?sign ?alt ?zpad ?width ?base (uns_of_sint t)

    let xfmt ?pad ?just ?sign ?alt ?zpad ?width ?base t formatter : (module Fmt.Formatter) =
      V.xfmt ?pad ?just ?sign ?alt ?zpad ?width ?base (uns_of_sint t) formatter

    let fmt t formatter =
      xfmt t formatter

    let zero = sint_of_uns V.zero

    let one = sint_of_uns V.one

    let min_value = sint_of_uns (V.narrow Int64.min_int)

    let max_value = sint_of_uns (V.narrow Int64.max_int)

    let succ t =
      sint_of_uns (V.succ (uns_of_sint t))

    let pred t =
      sint_of_uns (V.pred (uns_of_sint t))

    let bit_and t0 t1 =
      sint_of_uns (V.bit_and (uns_of_sint t0) (uns_of_sint t1))

    let bit_or t0 t1 =
      sint_of_uns (V.bit_or (uns_of_sint t0) (uns_of_sint t1))

    let bit_xor t0 t1 =
      sint_of_uns (V.bit_xor (uns_of_sint t0) (uns_of_sint t1))

    let bit_not t =
      sint_of_uns (V.bit_not (uns_of_sint t))

    let bit_sl ~shift t =
      sint_of_uns (V.bit_sl ~shift (uns_of_sint t))

    let bit_usr ~shift t =
      sint_of_uns (V.bit_usr ~shift (uns_of_sint t))

    let bit_ssr ~shift t =
      sint_of_uns (V.bit_ssr ~shift (uns_of_sint t))

    let bit_pop t =
      V.bit_pop (uns_of_sint t)

    let bit_clz t =
      V.bit_clz (uns_of_sint t)

    let bit_ctz t =
      V.bit_ctz (uns_of_sint t)

    let is_pow2 t =
      V.is_pow2 (uns_of_sint t)

    let floor_pow2 t =
      sint_of_uns (V.floor_pow2 (uns_of_sint t))

    let ceil_pow2 t =
      sint_of_uns (V.ceil_pow2 (uns_of_sint t))

    let floor_lg t =
      sint_of_uns (V.floor_lg (uns_of_sint t))

    let ceil_lg t =
      sint_of_uns (V.ceil_lg (uns_of_sint t))

    let ( + ) t0 t1 =
      sint_of_uns (V.( + ) (uns_of_sint t0) (uns_of_sint t1))

    let ( - ) t0 t1 =
      sint_of_uns (V.( - ) (uns_of_sint t0) (uns_of_sint t1))

    let ( * ) t0 t1 =
      sint_of_uns (V.( * ) (uns_of_sint t0) (uns_of_sint t1))

    let ( / ) t0 t1 =
      sint_of_uns (V.( / ) (uns_of_sint t0) (uns_of_sint t1))

    let ( % ) t0 t1 =
      sint_of_uns (V.( % ) (uns_of_sint t0) (uns_of_sint t1))

    let ( ** ) t0 t1 =
      sint_of_uns (V.( ** ) (uns_of_sint t0) (uns_of_sint t1))

    let ( // ) t0 t1 =
      V.( // ) (uns_of_sint t0) (uns_of_sint t1)

    let min t0 t1 =
      sint_of_uns (V.min (uns_of_sint t0) (uns_of_sint t1))

    let max t0 t1 =
      sint_of_uns (V.max (uns_of_sint t0) (uns_of_sint t1))

    let neg_one =
      V.(narrow (zero - one))

    let ( ~- ) t =
      assert (Int64.(compare (V.narrow t) t) = 0);
      sint_of_uns Int64.(neg t)

    let ( ~+) t =
      assert (Int64.(compare (V.narrow t) t) = 0);
      t

    let neg t =
      assert (Int64.(compare (V.narrow t) t) = 0);
      -t

    let abs t =
      assert (Int64.(compare (V.narrow t) t) = 0);
      V.narrow Int64.(abs t)
  end
  include U
  include Identifiable.Make(U)
  include Cmpable.MakeZero(U)
end

module MakeU (T : I) : SU with type t := uns = struct
  module U = struct
    type t = uns
    let bit_length = T.bit_length
    let signed = false
  end
  include U
  include MakeCommon(U)
end
