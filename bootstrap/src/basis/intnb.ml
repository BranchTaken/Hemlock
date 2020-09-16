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
        let nb = T.num_bits in
        let lz = T.bit_clz t in
        T.bit_sl ~shift:(nb - 1 - lz) T.one
      end

  let ceil_pow2 t =
    match T.cmp t T.one with
    | Lt -> halt "Invalid input"
    | Eq -> t
    | Gt -> begin
        let nb = T.num_bits in
        match T.bit_clz T.(t - one) with
        | 0 -> T.zero
        | lz -> T.bit_sl ~shift:(nb - lz) T.one
      end

  let floor_lg_opt t =
    match T.cmp t T.zero with
    | Lt | Eq -> None
    | Gt -> begin
        let nb = T.num_bits in
        let lz = T.bit_clz t in
        Some (T.of_uns (nb - 1 - lz))
      end

  let floor_lg t =
    match floor_lg_opt t with
    | None -> halt "Invalid input"
    | Some x -> x

  let ceil_lg t =
    match floor_lg_opt t with
    | None -> halt "Invalid input"
    | Some x -> T.(x + (if is_pow2 t then zero else one))

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

  val narrow: t -> int
end

module MakeCommon (T : ICommon) : SCommon with type t := uns = struct
  module U = struct
    type t = int

    let hash_fold t state =
      Hash.State.Gen.init state
      |> Hash.State.Gen.fold_u128 1
        ~f:(fun _ -> u128_of_arr [|Int64.of_int t; Int64.zero|])
      |> Hash.State.Gen.fini

    let extend t =
      let nlb = Sys.int_size - T.num_bits in
      match nlb with
      | 0 -> t
      | _ -> begin
          match T.signed with
          | false -> ((1 lsl T.num_bits) - 1) land t
          | true -> (t lsl nlb) asr nlb
        end

    let narrow t =
      let nlb = Sys.int_size - T.num_bits in
      match nlb with
      | 0 ->  t
      | _ -> begin
          match T.signed with
          | false -> ((1 lsl T.num_bits) - 1) land t
          | true -> ((t asr (Sys.int_size - 1)) lsl (T.num_bits - 1)) lor
              (((1 lsl (T.num_bits - 1)) - 1) land t)
        end

    let cmp t0 t1 =
      assert (narrow t0 = t0);
      assert (narrow t1 = t1);
      match T.signed || (Sys.int_size > T.num_bits) with
      | true -> intnb_icmp t0 t1
      | false -> intnb_ucmp t0 t1

    let narrow_of_signed s =
      match T.signed with
      | true -> narrow (uns_of_sint s)
      | false -> begin
          let nlb = Sys.int_size - T.num_bits in
          (match nlb with
            | 0 -> uns_of_sint s
            | _ -> ((1 lsl T.num_bits) - 1) land (uns_of_sint s)
          )
        end

    let narrow_of_unsigned u =
      match T.signed with
      | true -> begin
          let nlb = Sys.int_size - T.num_bits in
          match nlb with
          | 0 -> u
          | _ -> ((1 lsl (T.num_bits - 1)) - 1) land u
        end
      | false -> narrow u

    let lbfill t =
      let nlb = Sys.int_size - T.num_bits in
      match nlb with
      | 0 -> t
      | _ -> begin
          let mask = ((1 lsl T.num_bits) - 1) in
          let lb = lnot mask in
          lb lor t
        end

    let lbclear t =
      match T.signed with
      | false -> t
      | true -> begin
          let nlb = Sys.int_size - T.num_bits in
          match nlb with
          | 0 -> begin
              (* Leading bits are already zeros. *)
              assert (narrow t = t);
              t
            end
          | _ -> begin
              let mask = ((1 lsl T.num_bits) - 1) in
              t land mask
            end
        end

    let of_real r =
      (* OCaml handles overflow poorly, but this deficiency has no anticipated
       * impact on bootstrapping. *)
      match T.signed || (r >= 0.) with
      | true -> narrow (int_of_float r)
      | false -> 0

    let to_real t =
      assert (narrow t = t);
      match T.signed with
      | true -> float_of_int t
      | false -> (float_of_int (t lsr 1)) *. 2. +. (float_of_int (t land 1))

    let pp ppf t =
      assert (narrow t = t);
      let nlb = Sys.int_size - T.num_bits in
      match T.signed, nlb with
      | false, 0 -> Format.fprintf ppf "%u" t
      | true, 0 -> Format.fprintf ppf "%di" t
      | false, _ -> Format.fprintf ppf "%uu%u" t T.num_bits
      | true, _ -> Format.fprintf ppf "%di%u" t T.num_bits

    let pp_x ppf t =
      assert (narrow t = t);
      let nlb = Sys.int_size - T.num_bits in
      let hex_digits = (T.num_bits + 3) / 4 in
      let tbits = ((1 lsl T.num_bits) - 1) land t in (* Mask sign extension. *)
      match T.signed, nlb with
      | false, 0 -> Format.fprintf ppf "0x%0*x" hex_digits tbits
      | true, 0 -> Format.fprintf ppf "0x%0*xi" hex_digits tbits
      | false, _ -> Format.fprintf ppf "0x%0*xu%u" hex_digits tbits T.num_bits
      | true, _ -> Format.fprintf ppf "0x%0*xi%u" hex_digits tbits T.num_bits

    let of_string s =
      narrow (int_of_string s)

    let to_string t =
      assert (narrow t = t);
      Format.asprintf "%a" pp t

    let zero = 0

    let one = 1

    let min_value = zero

    let max_value = narrow (zero - one)

    let succ t =
      extend (t + 1)

    let pred t =
      extend (t - 1)

    let bit_and t0 t1 =
      extend (t0 land t1)

    let bit_or t0 t1 =
      extend (t0 lor t1)

    let bit_xor t0 t1 =
      extend (t0 lxor t1)

    let bit_not t =
      extend (lnot t)

    let bit_sl ~shift t =
      extend (t lsl shift)

    let bit_usr ~shift t =
      extend (t lsr shift)

    let bit_ssr ~shift t =
      extend (t asr shift)

    let bit_pop t =
      let x = lbclear t in
      let x = x - ((x lsr 1) land 0x5555_5555_5555_5555) in
      let c3s = 0x3333_3333_3333_3333 in
      let x = (x land c3s) + ((x lsr 2) land c3s) in
      let x = (x + (x lsr 4)) land 0x0f0f_0f0f_0f0f_0f0f in
      let x = x + (x lsr 8) in
      let x = x + (x lsr 16) in
      let x = x + (x lsr 32) in
      x land 0x3f

    let bit_clz t =
      let x = lbclear t in
      let x = x lor (x lsr 1) in
      let x = x lor (x lsr 2) in
      let x = x lor (x lsr 4) in
      let x = x lor (x lsr 8) in
      let x = x lor (x lsr 16) in
      let x = x lor (x lsr 32) in
      bit_pop (bit_not x)

    let bit_ctz t =
      let t' = lbfill t in
      bit_pop (bit_and (bit_not t') (t' - 1))

    let ( + ) t0 t1 =
      extend (t0 + t1)

    let ( - ) t0 t1 =
      extend (t0 - t1)

    let ( * ) t0 t1 =
      extend (t0 * t1)

    let ( / ) t0 t1 =
      extend (t0 / t1)

    let ( % ) t0 t1 =
      extend (t0 mod t1)

    let ( ** ) t0 t1 =
      (* Decompose the exponent to limit algorithmic complexity. *)
      let neg, n = if T.signed && t1 < 0 then
          true, -t1
        else
          false, t1
      in
      let rec fn r p n = begin
        match n with
        | 0 -> r
        | _ -> begin
            let r' = match bit_and n 1 with
              | 0 -> r
              | 1 -> r * p
              | _ -> assert false
            in
            let p' = p * p in
            let n' = bit_usr ~shift:1 n in
            fn r' p' n'
          end
      end in
      let r = fn 1 t0 n in
      narrow (
        match neg with
        | false -> r
        | true -> 1 / r
      )

    let ( // ) t0 t1 =
      assert (narrow t0 = t0);
      assert (narrow t1 = t1);
      (to_real t0) /. (to_real t1)
  end
  include U
  include Identifiable.Make(U)
  include Cmpable.MakeZero(U)
  module V = struct
    include U

    let num_bits = T.num_bits

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
        let num_bits = T.num_bits
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
      sint_of_int (V.narrow_of_signed x)

    let narrow_of_unsigned x =
      sint_of_int (V.narrow_of_unsigned x)

    let of_real r =
      sint_of_int (V.of_real r)

    let to_real t =
      V.to_real (int_of_sint t)

    let of_string s =
      sint_of_uns (V.of_string s)

    let pp ppf t =
      V.pp ppf (uns_of_sint t)

    let pp_x ppf t =
      V.pp_x ppf (uns_of_sint t)

    let to_string t =
      V.to_string (uns_of_sint t)

    let zero = sint_of_uns V.zero

    let one = sint_of_uns V.one

    let min_value = sint_of_int (V.narrow min_int)

    let max_value = sint_of_int (V.narrow max_int)

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

    let neg_one = sint_of_uns (V.narrow (-1))

    let ( ~- ) t =
      assert (sint_of_int (V.narrow (int_of_sint t)) = t);
      sint_of_uns (-(int_of_sint t))

    let ( ~+) t =
      assert (sint_of_int (V.narrow (int_of_sint t)) = t);
      t

    let neg t =
      assert (sint_of_int (V.narrow (int_of_sint t)) = t);
      -t

    let abs t =
      assert (sint_of_int (V.narrow (int_of_sint t)) = t);
      sint_of_int (abs (int_of_sint t))
  end
  include U
  include Identifiable.Make(U)
  include Cmpable.MakeZero(U)
end

module MakeU (T : I) : SU with type t := uns = struct
  module U = struct
    type t = uns
    let num_bits = T.num_bits
    let signed = false
  end
  include U
  include MakeCommon(U)
end
