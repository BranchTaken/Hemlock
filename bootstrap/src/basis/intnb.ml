open Rudiments_functions
open Intnb_intf

external intnb_icmp: usize -> usize -> Cmp.t = "hemlock_intnb_icmp"
external intnb_ucmp: usize -> usize -> Cmp.t = "hemlock_intnb_ucmp"

module Make_derived (T : I_derived) : S_derived with type t := T.t = struct
  include Cmpable.Make_zero(T)

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
        Some (T.of_usize (nb - 1 - lz))
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

module type I_common = sig
  include I
  val signed: bool
end

module type S_common = sig
  type t

  include S with type t := t
  include S_narrow with type t := t

  val narrow: t -> int
end

module Make_common (T : I_common) : S_common with type t := usize = struct
  module U = struct
    type t = int

    let hash_fold t state =
      Hash.State.Gen.init state
      |> Hash.State.Gen.fold_u128 1
        ~f:(fun _ -> {hi=Int64.zero; lo=Int64.of_int t})
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
      | true -> narrow (usize_of_isize s)
      | false -> begin
          let nlb = Sys.int_size - T.num_bits in
          (match nlb with
            | 0 -> usize_of_isize s
            | _ -> ((1 lsl T.num_bits) - 1) land (usize_of_isize s)
          )
        end

    let narrow_of_unsigned u =
      match T.signed with
      | true -> begin
          let nlb = Sys.int_size - T.num_bits in
          match nlb with
          | 0 -> u
          | _ -> ((1 lsl T.num_bits) - 1) land u
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

    let of_float f =
      (* OCaml handles overflow poorly, but this deficiency has no anticipated
       * impact on bootstrapping. *)
      match T.signed || (f >= 0.) with
      | true -> narrow (int_of_float f)
      | false -> 0

    let to_float t =
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
      (to_float t0) /. (to_float t1)
  end
  include U
  include Identifiable.Make(U)
  include Cmpable.Make_zero(U)
  module V = struct
    include U

    let num_bits = T.num_bits

    let of_usize t =
      U.narrow_of_unsigned t
  end
  include Make_derived(V)
end

module Make_i (T : I) : S_i with type t := isize = struct
  module U = struct
    module V = struct
      module W = struct
        type t = isize
        let num_bits = T.num_bits
        let signed = true
      end
      include W
      include Make_common(W)
    end
    type t = isize

    let hash_fold t state =
      state
      |> V.hash_fold (usize_of_isize t)

    let cmp t0 t1 =
      V.cmp (usize_of_isize t0) (usize_of_isize t1)

    let narrow_of_signed x =
      isize_of_int (V.narrow_of_signed x)

    let narrow_of_unsigned x =
      isize_of_int (V.narrow_of_unsigned x)

    let of_float f =
      isize_of_int (V.of_float f)

    let to_float t =
      V.to_float (int_of_isize t)

    let of_string s =
      isize_of_usize (V.of_string s)

    let pp ppf t =
      V.pp ppf (usize_of_isize t)

    let pp_x ppf t =
      V.pp_x ppf (usize_of_isize t)

    let to_string t =
      V.to_string (usize_of_isize t)

    let zero = isize_of_usize V.zero

    let one = isize_of_usize V.one

    let min_value = isize_of_int (V.narrow min_int)

    let max_value = isize_of_int (V.narrow max_int)

    let succ t =
      isize_of_usize (V.succ (usize_of_isize t))

    let pred t =
      isize_of_usize (V.pred (usize_of_isize t))

    let bit_and t0 t1 =
      isize_of_usize (V.bit_and (usize_of_isize t0) (usize_of_isize t1))

    let bit_or t0 t1 =
      isize_of_usize (V.bit_or (usize_of_isize t0) (usize_of_isize t1))

    let bit_xor t0 t1 =
      isize_of_usize (V.bit_xor (usize_of_isize t0) (usize_of_isize t1))

    let bit_not t =
      isize_of_usize (V.bit_not (usize_of_isize t))

    let bit_sl ~shift t =
      isize_of_usize (V.bit_sl ~shift (usize_of_isize t))

    let bit_usr ~shift t =
      isize_of_usize (V.bit_usr ~shift (usize_of_isize t))

    let bit_ssr ~shift t =
      isize_of_usize (V.bit_ssr ~shift (usize_of_isize t))

    let bit_pop t =
      V.bit_pop (usize_of_isize t)

    let bit_clz t =
      V.bit_clz (usize_of_isize t)

    let bit_ctz t =
      V.bit_ctz (usize_of_isize t)

    let is_pow2 t =
      V.is_pow2 (usize_of_isize t)

    let floor_pow2 t =
      isize_of_usize (V.floor_pow2 (usize_of_isize t))

    let ceil_pow2 t =
      isize_of_usize (V.ceil_pow2 (usize_of_isize t))

    let floor_lg t =
      isize_of_usize (V.floor_lg (usize_of_isize t))

    let ceil_lg t =
      isize_of_usize (V.ceil_lg (usize_of_isize t))

    let ( + ) t0 t1 =
      isize_of_usize (V.( + ) (usize_of_isize t0) (usize_of_isize t1))

    let ( - ) t0 t1 =
      isize_of_usize (V.( - ) (usize_of_isize t0) (usize_of_isize t1))

    let ( * ) t0 t1 =
      isize_of_usize (V.( * ) (usize_of_isize t0) (usize_of_isize t1))

    let ( / ) t0 t1 =
      isize_of_usize (V.( / ) (usize_of_isize t0) (usize_of_isize t1))

    let ( % ) t0 t1 =
      isize_of_usize (V.( % ) (usize_of_isize t0) (usize_of_isize t1))

    let ( ** ) t0 t1 =
      isize_of_usize (V.( ** ) (usize_of_isize t0) (usize_of_isize t1))

    let ( // ) t0 t1 =
      V.( // ) (usize_of_isize t0) (usize_of_isize t1)

    let min t0 t1 =
      isize_of_usize (V.min (usize_of_isize t0) (usize_of_isize t1))

    let max t0 t1 =
      isize_of_usize (V.max (usize_of_isize t0) (usize_of_isize t1))

    let neg_one = isize_of_usize (V.narrow (-1))

    let ( ~- ) t =
      assert (isize_of_int (V.narrow (int_of_isize t)) = t);
      isize_of_usize (-(int_of_isize t))

    let ( ~+) t =
      assert (isize_of_int (V.narrow (int_of_isize t)) = t);
      t

    let neg t =
      assert (isize_of_int (V.narrow (int_of_isize t)) = t);
      -t

    let abs t =
      assert (isize_of_int (V.narrow (int_of_isize t)) = t);
      isize_of_int (abs (int_of_isize t))
  end
  include U
  include Identifiable.Make(U)
  include Cmpable.Make_zero(U)
end

module Make_u (T : I) : S_u with type t := usize = struct
  module U = struct
    type t = usize
    let num_bits = T.num_bits
    let signed = false
  end
  include U
  include Make_common(U)
end
