open RudimentsInt0

type t = u128

let pp = u128_pp_x

let fmt = u128_fmt_x

module State = struct
  type t = u128

  let pp = u128_pp_x

  let fmt = u128_fmt_x

  let empty = u128_zero

  let of_u128 u =
    u

  let seed = Entropy.seed

  module Gen = struct
    type outer = t
    type t = {
      (* Hash state. *)
      state: outer;
      (* Number of u128 blocks folded. *)
      nfolded: uns;
      (* The bottom nrem bytes of rem are remainder bytes that have yet to be hashed, and the top
       * pad bytes are always zeroed. *)
      rem: u128;
      nrem: uns;
    }

    let init state =
      {
        state;
        nfolded=0L;
        rem=u128_zero;
        nrem=0L;
      }

    let rotl x r =
      Int64.logor (Int64.shift_left x r) (Int64.shift_right_logical x (64 - r))

    let fmix_c1 = 0xff51afd7ed558ccdL
    let fmix_c2 = 0xc4ceb9fe1a85ec53L

    let fmix k =
      let k = Int64.(logxor k (shift_right_logical k 33)) in
      let k = Int64.(mul k fmix_c1) in
      let k = Int64.(logxor k (shift_right_logical k 33)) in
      let k = Int64.(mul k fmix_c2) in
      let k = Int64.(logxor k (shift_right_logical k 33)) in
      k

    let hash_c1 = 0x87c37b91114253d5L
    let hash_c2 = 0x4cf5ad432745937fL

    let hash u t =
      let h1, h2 = u128_to_tup t.state in
      let k1, k2 = u128_to_tup u in

      let k1 = Int64.mul k1 hash_c1 in
      let k1 = rotl k1 31 in
      let k1 = Int64.mul k1 hash_c2 in
      let h1 = Int64.logxor h1 k1 in

      let h1 = rotl h1 27 in
      let h1 = Int64.add h1 h2 in
      let h1 = Int64.(add (mul h1 5L) 0x52dce729L) in

      let k2 = Int64.mul k2 hash_c2 in
      let k2 = rotl k2 33 in
      let k2 = Int64.mul k2 hash_c1 in
      let h2 = Int64.logxor h2 k2 in

      let h2 = rotl h2 31 in
      let h2 = Int64.add h2 h1 in
      let h2 = Int64.(add (mul h2 5L) 0x38495ab5L) in

      let state = u128_of_tup (h1, h2) in
      {t with state; nfolded=Int64.(succ t.nfolded)}

    let fold_u128 n ~f t =
      let feed u t = begin
        match t.nrem = 0L with
        | true -> hash u t
        | false -> begin
            let u' = u128_bit_or t.rem (u128_bit_sl ~shift:t.nrem u) in
            let rem = u128_bit_usr ~shift:Int64.(sub 16L t.nrem) u in
            let t' = {t with rem} in
            hash u' t'
          end
      end in
      let rec fn i n ~f t = begin
        match i = n with
        | true -> t
        | false -> fn Int64.(succ i) n ~f (feed (f i) t)
      end in
      fn 0L n ~f t

    let fold_u64 n ~f t =
      let feed w t = begin
        let u = u128_of_tup (w, Int64.zero) in
        match t.nrem >= 8L with
        | true -> begin
            let u' = u128_bit_or t.rem (u128_bit_sl ~shift:Int64.(mul t.nrem 8L) u) in
            let t' = {t with
              rem=u128_bit_usr ~shift:Int64.(mul (sub 16L t.nrem) 8L) u;
              nrem=Int64.(rem t.nrem 8L)
            } in
            hash u' t'
          end
        | false -> begin
            let rem = u128_bit_or t.rem (u128_bit_sl ~shift:Int64.(mul t.nrem 8L) u) in
            {t with rem; nrem=Int64.(add t.nrem 8L)}
          end
      end in
      let rec fn i n ~f t = begin
        match i = n with
        | true -> t
        | false -> fn Int64.(succ i) n ~f (feed (f i) t)
      end in
      fn 0L n ~f t

    let fold_u8 n ~f t =
      let feed b t = begin
        let u = u128_of_tup Int64.(b, zero) in
        match t.nrem = 15L with
        | true -> begin
            let u' = u128_bit_or t.rem (u128_bit_sl ~shift:120L u) in
            let t' = {t with rem=u128_zero; nrem=0L} in
            hash u' t'
          end
        | false -> begin
            let rem = u128_bit_or t.rem (u128_bit_sl ~shift:Int64.(mul t.nrem 8L) u) in
            {t with rem; nrem=Int64.(succ t.nrem)}
          end
      end in
      let rec fn i n ~f t = begin
        match i = n with
        | true -> t
        | false -> fn Int64.(succ i) n ~f (feed (Int64.logand (f i) 0xffL) t)
      end in
      fn 0L n ~f t

    let fini t =
      let fold_rem t = begin
        let len = Int64.(add (mul t.nfolded 16L) t.nrem) in
        match t.nrem > 0L with
        | false -> t, len
        | true -> begin
            let h1, h2 = u128_to_tup t.state in
            let k1, k2 = u128_to_tup t.rem in

            let k2 = Int64.mul k2 hash_c2 in
            let k2 = rotl k2 33 in
            let k2 = Int64.mul k2 hash_c1 in
            let h2 = Int64.logxor h2 k2 in

            let k1 = Int64.mul k1 hash_c1 in
            let k1 = rotl k1 31 in
            let k1 = Int64.mul k1 hash_c2 in
            let h1 = Int64.logxor h1 k1 in

            let state = u128_of_tup (h1, h2) in
            {t with state; rem=u128_zero; nrem=0L}, len
          end
      end in
      let t', len = fold_rem t in
      let h1, h2 = u128_to_tup t'.state in

      let h1 = Int64.(logxor h1 len) in
      let h2 = Int64.(logxor h2 len) in

      let h1 = Int64.add h1 h2 in
      let h2 = Int64.add h2 h1 in

      let h1 = fmix h1 in
      let h2 = fmix h2 in

      let h1 = Int64.add h1 h2 in
      let h2 = Int64.add h2 h1 in

      u128_of_tup (h1, h2)
  end
end

let t_of_state (state:State.t) : t =
  state
