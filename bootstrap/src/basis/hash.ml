open Rudiments_int0

type t = u128

let pp = u128_pp_x

module State = struct
  type t = u128

  let pp = u128_pp_x

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
      nfolded: usize;
      (* The bottom nrem bytes of rem are remainder bytes that have yet to be
       * hashed, and the top pad bytes are always zeroed. *)
      rem: u128;
      nrem: usize;
    }

    let init state =
      {
        state;
        nfolded=0;
        rem=u128_zero;
        nrem=0;
      }

    let rotl x r =
      Int64.logor (Int64.shift_left x r)
        (Int64.shift_right_logical x (64 - r))

    let fmix_c1 = Int64.of_string "0xff51afd7ed558ccd"
    let fmix_c2 = Int64.of_string "0xc4ceb9fe1a85ec53"

    let fmix k =
      let k = Int64.(logxor k (shift_right_logical k 33)) in
      let k = Int64.(mul k fmix_c1) in
      let k = Int64.(logxor k (shift_right_logical k 33)) in
      let k = Int64.(mul k fmix_c2) in
      let k = Int64.(logxor k (shift_right_logical k 33)) in
      k

    let hash_c1 = Int64.of_string "0x87c37b91114253d5"
    let hash_c2 = Int64.of_string "0x4cf5ad432745937f"

    let hash u t =
      let h1, h2 = match t.state with {hi; lo} -> lo, hi in
      let k1, k2 = match u with {hi; lo} -> lo, hi in

      let k1 = Int64.mul k1 hash_c1 in
      let k1 = rotl k1 31 in
      let k1 = Int64.mul k1 hash_c2 in
      let h1 = Int64.logxor h1 k1 in

      let h1 = rotl h1 27 in
      let h1 = Int64.add h1 h2 in
      let h1 = Int64.(add (mul h1 (of_int 5)) (of_int 0x52dce729)) in

      let k2 = Int64.mul k2 hash_c2 in
      let k2 = rotl k2 33 in
      let k2 = Int64.mul k2 hash_c1 in
      let h2 = Int64.logxor h2 k2 in

      let h2 = rotl h2 31 in
      let h2 = Int64.add h2 h1 in
      let h2 = Int64.(add (mul h2 (of_int 5)) (of_int 0x38495ab5)) in

      let state = {hi=h2; lo=h1} in
      {t with state; nfolded=succ t.nfolded}

    let fold_u128 n ~f t =
      let feed u t = begin
        match t.nrem = 0 with
        | true -> hash u t
        | false -> begin
            let u' = u128_bit_or t.rem (u128_bit_sl ~shift:t.nrem u) in
            let rem = u128_bit_usr ~shift:(16 - t.nrem) u in
            let t' = {t with rem} in
            hash u' t'
          end
      end in
      let rec fn i n ~f t = begin
        match i = n with
        | true -> t
        | false -> fn (succ i) n ~f (feed (f i) t)
      end in
      fn 0 n ~f t

    let fold_u8 n ~f t =
      let feed b t = begin
        let u = {hi=Int64.zero; lo=Int64.of_int b} in
        match t.nrem = 15 with
        | true -> begin
            let u' = u128_bit_or t.rem (u128_bit_sl ~shift:120 u) in
            let t' = {t with rem=u128_zero; nrem=0} in
            hash u' t'
          end
        | false -> begin
            let rem = u128_bit_or t.rem (u128_bit_sl ~shift:(t.nrem * 8) u) in
            {t with rem; nrem=succ t.nrem}
          end
      end in
      let rec fn i n ~f t = begin
        match i = n with
        | true -> t
        | false -> fn (succ i) n ~f (feed (Int.logand (f i) 0xff) t)
      end in
      fn 0 n ~f t

    let fini t =
      let fold_rem t = begin
        let len = t.nfolded * 16 + t.nrem in
        match t.nrem > 0 with
        | false -> t, len
        | true -> begin
            let h1, h2 = match t.state with {hi; lo} -> lo, hi in
            let k1, k2 = match t.rem with {hi; lo} -> lo, hi in

            let k2 = Int64.mul k2 hash_c2 in
            let k2 = rotl k2 33 in
            let k2 = Int64.mul k2 hash_c1 in
            let h2 = Int64.logxor h2 k2 in

            let k1 = Int64.mul k1 hash_c1 in
            let k1 = rotl k1 31 in
            let k1 = Int64.mul k1 hash_c2 in
            let h1 = Int64.logxor h1 k1 in

            let state = {hi=h2; lo=h1} in
            {t with state; rem=u128_zero; nrem=0}, len
          end
      end in
      let t', len = fold_rem t in
      let h1, h2 = match t'.state with {hi; lo} -> lo, hi in

      let h1 = Int64.logxor h1 (Int64.of_int len) in
      let h2 = Int64.logxor h2 (Int64.of_int len) in

      let h1 = Int64.add h1 h2 in
      let h2 = Int64.add h2 h1 in

      let h1 = fmix h1 in
      let h2 = fmix h2 in

      let h1 = Int64.add h1 h2 in
      let h2 = Int64.add h2 h1 in

      {hi=h2; lo=h1}
  end
end

let t_of_state (state:State.t) : t =
  state

(******************************************************************************)
(* Begin tests. *)

let%expect_test "pp,empty,t_of_state" =
  let open Format in
  printf "@[<h>";
  printf "hash=%a\n" pp (t_of_state State.empty);
  printf "state=%a\n" State.pp State.empty;
  printf "@]";

  [%expect{|
    hash=0x0000_0000_0000_0000_0000_0000_0000_0000u128
    state=0x0000_0000_0000_0000_0000_0000_0000_0000u128
    |}]

let%expect_test "hash_fold_u128" =
  let open Format in
  let hash_fold u128s t = begin
    State.Gen.init t
    |> State.Gen.fold_u128 (Caml.Array.length u128s) ~f:(fun i ->
      Caml.Array.get u128s i
    )
    |> State.Gen.fini
  end in
  let pp_arr pp_elm ppf arr = begin
    let rec fn arr i len = begin
      match i = len with
      | true -> ()
      | false -> begin
          if i > 0 then fprintf ppf ";@ ";
          fprintf ppf "%a" pp_elm (Caml.Array.get arr i);
          fn arr (succ i) len
        end
    end in
    fprintf ppf "@[<h>[|";
    fn arr 0 (Caml.Array.length arr);
    fprintf ppf "|]@]"
  end in
  printf "@[<h>";
  let rec test_hash_fold u128s_list = begin
    match u128s_list with
    | [] -> ()
    | u128s :: u128s_list' -> begin
        printf "hash_fold %a -> %a\n"
          (pp_arr pp) u128s pp (t_of_state State.(hash_fold u128s empty));
        test_hash_fold u128s_list'
      end
  end in
  (* These test inputs were manually verified against the reference
   * MurmurHash3 implementation. *)
  let u128s_list = [
    [||];

    [|{hi=Int64.of_string "0xfedcba9876543210";
      lo=Int64.of_string "0x0123456789abcdef"}|];

    [|u128_zero|];

    [|{hi=Int64.of_string "0x0123456789abcdef";
      lo=Int64.of_string "0xfedcba9876543210"}|];

    [|{hi=Int64.of_string "0xfedcba9876543210";
      lo=Int64.of_string "0x0123456789abcdef"};

      u128_zero;

      {hi=Int64.of_string "0x0123456789abcdef";
        lo=Int64.of_string "0xfedcba9876543210"}|]
  ] in
  test_hash_fold u128s_list;
  printf "@]";

  [%expect{|
    hash_fold [||] -> 0x0000_0000_0000_0000_0000_0000_0000_0000u128
    hash_fold [|0xfedc_ba98_7654_3210_0123_4567_89ab_cdefu128|] -> 0x6bc0_bad4_001a_c79a_3a7b_e286_a34a_7a71u128
    hash_fold [|0x0000_0000_0000_0000_0000_0000_0000_0000u128|] -> 0xb465_a9ec_cd79_1cb6_4bbd_1bf2_7da9_18d6u128
    hash_fold [|0x0123_4567_89ab_cdef_fedc_ba98_7654_3210u128|] -> 0x0eb8_4470_9565_7807_4f87_4a06_b0db_435eu128
    hash_fold [|0xfedc_ba98_7654_3210_0123_4567_89ab_cdefu128; 0x0000_0000_0000_0000_0000_0000_0000_0000u128; 0x0123_4567_89ab_cdef_fedc_ba98_7654_3210u128|] -> 0x368d_c0ef_bb0a_0838_afb5_c175_06c0_c15eu128
    |}]

let%expect_test "hash_fold_u8" =
  let open Format in
  let hash_fold u8s t = begin
    State.Gen.init t
    |> State.Gen.fold_u8 (Caml.Array.length u8s) ~f:(fun i ->
      Caml.Array.get u8s i
    )
    |> State.Gen.fini
  end in
  let pp_u8 ppf u = Format.fprintf ppf "0x%02x" u in
  let pp_arr pp_elm ppf arr = begin
    let rec fn arr i len = begin
      match i = len with
      | true -> ()
      | false -> begin
          if i > 0 then fprintf ppf ";@ ";
          fprintf ppf "%a" pp_elm (Caml.Array.get arr i);
          fn arr (succ i) len
        end
    end in
    fprintf ppf "@[<h>[|";
    fn arr 0 (Caml.Array.length arr);
    fprintf ppf "|]@]"
  end in
  printf "@[<h>";
  let rec test_hash_fold u8s_list = begin
    match u8s_list with
    | [] -> ()
    | u8s :: u8s_list' -> begin
        printf "hash_fold %a -> %a\n"
          (pp_arr pp_u8) u8s pp (t_of_state State.(hash_fold u8s empty));
        test_hash_fold u8s_list'
      end
  end in
  (* These test inputs were manually verified against the reference
   * MurmurHash3 implementation. *)
  let u8s_list = [
    [||];

    [|0|];

    [|0xef; 0xcd; 0xab; 0x89; 0x67; 0x45; 0x23; 0x01;
      0x10; 0x32; 0x54; 0x76; 0x98; 0xba; 0xdc; 0xfe|];

    [|0; 0; 0; 0; 0; 0; 0; 0;
      0; 0; 0; 0; 0; 0; 0; 0|];

    [|0x10; 0x32; 0x54; 0x76; 0x98; 0xba; 0xdc; 0xfe;
      0xef; 0xcd; 0xab; 0x89; 0x67; 0x45; 0x23; 0x01|];

    [|0xef; 0xcd; 0xab; 0x89; 0x67; 0x45; 0x23; 0x01;
      0x10; 0x32; 0x54; 0x76; 0x98; 0xba; 0xdc; 0xfe;

      0; 0; 0; 0; 0; 0; 0; 0;
      0; 0; 0; 0; 0; 0; 0; 0;

      0x10; 0x32; 0x54; 0x76; 0x98; 0xba; 0xdc; 0xfe;
      0xef; 0xcd; 0xab; 0x89; 0x67; 0x45; 0x23; 0x01|]
  ] in
  test_hash_fold u8s_list;
  printf "@]";

  [%expect{|
    hash_fold [||] -> 0x0000_0000_0000_0000_0000_0000_0000_0000u128
    hash_fold [|0x00|] -> 0x5162_2daa_78f8_3583_4610_abe5_6eff_5cb5u128
    hash_fold [|0xef; 0xcd; 0xab; 0x89; 0x67; 0x45; 0x23; 0x01; 0x10; 0x32; 0x54; 0x76; 0x98; 0xba; 0xdc; 0xfe|] -> 0x6bc0_bad4_001a_c79a_3a7b_e286_a34a_7a71u128
    hash_fold [|0x00; 0x00; 0x00; 0x00; 0x00; 0x00; 0x00; 0x00; 0x00; 0x00; 0x00; 0x00; 0x00; 0x00; 0x00; 0x00|] -> 0xb465_a9ec_cd79_1cb6_4bbd_1bf2_7da9_18d6u128
    hash_fold [|0x10; 0x32; 0x54; 0x76; 0x98; 0xba; 0xdc; 0xfe; 0xef; 0xcd; 0xab; 0x89; 0x67; 0x45; 0x23; 0x01|] -> 0x0eb8_4470_9565_7807_4f87_4a06_b0db_435eu128
    hash_fold [|0xef; 0xcd; 0xab; 0x89; 0x67; 0x45; 0x23; 0x01; 0x10; 0x32; 0x54; 0x76; 0x98; 0xba; 0xdc; 0xfe; 0x00; 0x00; 0x00; 0x00; 0x00; 0x00; 0x00; 0x00; 0x00; 0x00; 0x00; 0x00; 0x00; 0x00; 0x00; 0x00; 0x10; 0x32; 0x54; 0x76; 0x98; 0xba; 0xdc; 0xfe; 0xef; 0xcd; 0xab; 0x89; 0x67; 0x45; 0x23; 0x01|] -> 0x368d_c0ef_bb0a_0838_afb5_c175_06c0_c15eu128
    |}]
