(* Partial Rudiments. *)
open Rudiments_int
open Rudiments_functions
type byte = Byte.t

module T = struct
  type t = uns
  let num_bits = 21
end
include T
include Intnb.Make_u(T)

let max_codepoint = narrow_of_unsigned 0x10ffff
let replacement = 0xfffd

let is_surrogate t =
  t >= 0xd800 && t < 0xe000

let narrow_of_unsigned u =
  let t = narrow_of_unsigned u in
  match t > max_codepoint || is_surrogate t with
  | true -> replacement
  | false -> t

let kv x =
  narrow_of_unsigned x

let to_uns t =
  t

let of_uns x =
  narrow_of_unsigned x

let of_uns_hlt x =
  let t = of_uns x in
  let x' = to_uns t in
  match Uns.(x' = x) with
  | false -> halt "Lossy conversion"
  | true -> t

let of_char c =
  Stdlib.Char.code c

let nul = 0x00
let soh = 0x01
let stx = 0x02
let etx = 0x03
let eot = 0x04
let enq = 0x05
let ack = 0x06
let bel = 0x07
let bs = 0x08
let ht = of_char '\t'
let lf = of_char '\n'
let nl = of_char '\n'
let vt = 0x0b
let ff = 0x0c
let cr = of_char '\r'
let so = 0x0e
let si = 0x0f
let dle = 0x10
let dc1 = 0x11
let dc2 = 0x12
let dc3 = 0x13
let dc4 = 0x14
let nak = 0x15
let syn = 0x16
let etb = 0x17
let can = 0x18
let em = 0x19
let sub = 0x1a
let esc = 0x1b
let fs = 0x1c
let gs = 0x1d
let rs = 0x1e
let us = 0x1f
let del = 0x7f

module Utf8 = struct
  type outer = t
  type t =
    | One   of byte
    | Two   of byte * byte
    | Three of byte * byte * byte
    | Four  of byte * byte * byte * byte

  let length_of_codepoint cp =
    assert (cp <= max_codepoint);
    let lz = bit_clz cp in
    let sigbits = 21 - lz in
    Caml.Array.get [|
      1; 1; 1; 1; 1; 1; 1; 1; (* [0..7] *)
      2; 2; 2; 2;             (* [8..11] *)
      3; 3; 3; 3; 3;          (* [12..16] *)
      4; 4; 4; 4; 4;          (* [17..21] *)
    |] sigbits

  let of_codepoint cp =
    assert (cp <= max_codepoint);
    let u = to_uns cp in
    match length_of_codepoint cp with
    | 1 -> One (Byte.of_uns u)
    | 2 ->
      Two (
        Byte.of_uns Uns.(bit_or 0b110_00000 (bit_usr ~shift:6 u)),
        Byte.of_uns Uns.(bit_or 0b10_000000 (bit_and u 0x3f))
      )
    | 3 ->
      Three (
        Byte.of_uns Uns.(bit_or 0b1110_0000 (bit_usr ~shift:12 u)),
        Byte.of_uns Uns.(bit_or 0b10_000000
            (bit_and (bit_usr ~shift:6 u) 0x3f)),
        Byte.of_uns Uns.(bit_or 0b10_000000 (bit_and u 0x3f))
      )
    | 4 ->
      Four (
        Byte.of_uns Uns.(bit_or 0b11110_000 (bit_usr ~shift:18 u)),
        Byte.of_uns Uns.(bit_or 0b10_000000
            (bit_and (bit_usr ~shift:12 u) 0x3f)),
        Byte.of_uns Uns.(bit_or 0b10_000000
            (bit_and (bit_usr ~shift:6 u) 0x3f)),
        Byte.of_uns Uns.(bit_or 0b10_000000 (bit_and u 0x3f))
      )
    | _ -> not_reached ()

  let to_codepoint = function
    | One b0 -> of_uns (Byte.to_uns b0)
    | Two (b0, b1) ->
      of_uns Uns.(bit_or
          (bit_sl ~shift:6 (bit_and (Byte.to_uns b0) 0x1f))
          (bit_and (Byte.to_uns b1) 0x3f))
    | Three (b0, b1, b2) ->
      of_uns Uns.(bit_or (bit_or
          (bit_sl ~shift:12 (bit_and (Byte.to_uns b0) 0xf))
          (bit_sl ~shift:6 (bit_and (Byte.to_uns b1) 0x3f)))
        (bit_and (Byte.to_uns b2) 0x3f))
    | Four (b0, b1, b2, b3) ->
      of_uns Uns.(bit_or (bit_or (bit_or
          (bit_sl ~shift:18 (bit_and (Byte.to_uns b0) 0x7))
          (bit_sl ~shift:12 (bit_and (Byte.to_uns b1) 0x3f)))
        (bit_sl ~shift:6 (bit_and (Byte.to_uns b2) 0x3f)))
        (bit_and (Byte.to_uns b3) 0x3f))

  let to_bytes = function
    | One b0 -> [b0]
    | Two (b0, b1) -> [b0; b1]
    | Three (b0, b1, b2) -> [b0; b1; b2]
    | Four (b0, b1, b2, b3) -> [b0; b1; b2; b3]

  let length = function
    | One _ -> 1
    | Two _ -> 2
    | Three _ -> 3
    | Four _ -> 4

  let to_string = function
    | One b0 -> Stdlib.String.init 1 (fun _ ->
      Stdlib.Char.chr (int_of_sint (Byte.to_sint b0))
    )
    | Two (b0, b1) -> Stdlib.String.init 2 (fun i ->
      Stdlib.Char.chr (int_of_sint (Byte.to_sint (
        match i with
        | 0 -> b0
        | 1 -> b1
        | _ -> not_reached ()
      ))))
    | Three (b0, b1, b2) -> Stdlib.String.init 3 (fun i ->
      Stdlib.Char.chr (int_of_sint (Byte.to_sint (
        match i with
        | 0 -> b0
        | 1 -> b1
        | 2 -> b2
        | _ -> not_reached ()
      ))))
    | Four (b0, b1, b2, b3) -> Stdlib.String.init 4 (fun i ->
      Stdlib.Char.chr (int_of_sint (Byte.to_sint (
        match i with
        | 0 -> b0
        | 1 -> b1
        | 2 -> b2
        | 3 -> b3
        | _ -> not_reached ()
      ))))

  let escape t =
    match t with
    | One b0 -> begin
        match of_uns (Byte.to_uns b0) with
        | cp when (cp = nul) -> "\\u{0}"
        | cp when (cp = soh) -> "\\u{1}"
        | cp when (cp = stx) -> "\\u{2}"
        | cp when (cp = etx) -> "\\u{3}"
        | cp when (cp = eot) -> "\\u{4}"
        | cp when (cp = enq) -> "\\u{5}"
        | cp when (cp = ack) -> "\\u{6}"
        | cp when (cp = bel) -> "\\u{7}"
        | cp when (cp = bs) -> "\\u{8}"
        | cp when (cp = ht) -> "\\t"
        | cp when (cp = nl) -> "\\n"
        | cp when (cp = vt) -> "\\u{b}"
        | cp when (cp = ff) -> "\\u{c}"
        | cp when (cp = cr) -> "\\r"
        | cp when (cp = so) -> "\\u{e}"
        | cp when (cp = si) -> "\\u{f}"
        | cp when (cp = dle) -> "\\u{10}"
        | cp when (cp = dc1) -> "\\u{11}"
        | cp when (cp = dc2) -> "\\u{12}"
        | cp when (cp = dc3) -> "\\u{13}"
        | cp when (cp = dc4) -> "\\u{14}"
        | cp when (cp = nak) -> "\\u{15}"
        | cp when (cp = syn) -> "\\u{16}"
        | cp when (cp = etb) -> "\\u{17}"
        | cp when (cp = can) -> "\\u{18}"
        | cp when (cp = em) -> "\\u{19}"
        | cp when (cp = sub) -> "\\u{1a}"
        | cp when (cp = esc) -> "\\u{1b}"
        | cp when (cp = fs) -> "\\u{1c}"
        | cp when (cp = gs) -> "\\u{1d}"
        | cp when (cp = rs) -> "\\u{1e}"
        | cp when (cp = us) -> "\\u{1f}"
        | cp when (cp = (of_char '"')) -> "\\\""
        | cp when (cp = (of_char '\\')) -> "\\\\"
        | cp when (cp = del) -> "\\u{7f}"
        | _ -> to_string t
      end
    | Two _
    | Three _
    | Four _ -> to_string t

  let pp ppf t =
    Format.fprintf ppf "%s" (escape t)
end

let to_bytes t =
  Utf8.(to_bytes (of_codepoint t))

let to_string t =
  Utf8.(to_string (of_codepoint t))

let escape t =
  match t with
  | t when (t = (of_char '"')) -> "'\"'"
  | t when (t = (of_char '\'')) -> "'\\\''"
  | _ -> begin
      let utf8_str = Utf8.(escape (of_codepoint t)) in
      let len = (Stdlib.String.length utf8_str) + 2 in
      Stdlib.String.init len (fun i ->
        if i = 0 || i == (pred len) then
          '\''
        else
          Stdlib.String.get utf8_str (Uns.pred i)
      )
    end

let pp ppf t =
  Format.fprintf ppf "%s" (escape t)

module Seq = struct
  type outer = t
  (* Silence ocp-indent. *)

  module type S = sig
    type t
    type decoded =
      | Valid    of outer * t
      | Invalid  of t
    val to_codepoint: t -> decoded option
  end

  module Make (T : Seq_intf.I_mono_indef with type elm := byte) :
    S with type t := T.t = struct
    type fragment = {
      u: uns;
      n: uns;
      nrem: uns;
    }

    (* It should be equivalent to write t vs T.t , but the compiler gets
     * confused unless we write T.t here. An alternative is to explicitly define
     * outer and t as:
     *
     *   type nonrec outer = outer
     *   type t = T.t *)
    type decoded =
      | Valid    of outer * T.t
      | Invalid  of T.t

    let rec to_codepoint_cont fragment t =
      match fragment.nrem with
      | 0 -> begin
          let cp = of_uns fragment.u in
          match Uns.((Utf8.length_of_codepoint cp) = fragment.n) with
          | true -> Valid (cp, t)
          | false -> Invalid t
        end
      | _ -> begin
          match T.next t with
          | None -> Invalid t
          | Some (b, _)
            when Byte.((bit_and b (kv 0b11_000000)) <> (kv 0b10_000000)) ->
            Invalid t
          | Some (b, t') -> begin
              let u' = bit_or (bit_sl ~shift:6 fragment.u)
                (bit_and 0x3f (Byte.to_uns b)) in
              to_codepoint_cont {fragment with u=u'; nrem=pred fragment.nrem} t'
            end
        end

    let to_codepoint t =
      match T.next t with
      | None -> None
      | Some (b, t') -> begin
          match Byte.(bit_clz (bit_not b)) with
          | 0 -> begin
              (* 0xxxxxxx *)
              Some (to_codepoint_cont {u=(Byte.to_uns b); n=1; nrem=0} t')
            end
          | 2 -> begin
              (* 110xxxxx *)
              Some (to_codepoint_cont
                  {u=(bit_and 0x1f (Byte.to_uns b)); n=2; nrem=1} t')
            end
          | 3 -> begin
              (* 1110xxxx *)
              Some (to_codepoint_cont
                  {u=(bit_and 0x0f (Byte.to_uns b)); n=3; nrem=2} t')
            end
          | 4 -> begin
              (* 11110xxx *)
              Some (to_codepoint_cont
                  {u=(bit_and 0x07 (Byte.to_uns b)); n=4; nrem=3} t')
            end
          | _ -> Some (Invalid t')
        end
  end

  module Make_rev (T : Seq_intf.I_mono_indef with type elm := byte) :
    S with type t := T.t = struct
    type fragment = {
      u: uns;
      n: uns;
      t_one: T.t; (* Sequence after having consumed one byte. *)
    }

    type decoded =
      | Valid    of outer * T.t
      | Invalid  of T.t

    let merge x fragment =
      let u' = bit_or (bit_sl ~shift:(fragment.n*6) x) fragment.u in
      let n' = succ fragment.n in
      {fragment with u=u'; n=n'}

    let validate fragment t =
      let cp = of_uns fragment.u in
      match Uns.((Utf8.length_of_codepoint cp) = fragment.n) with
      | true -> Valid (cp, t)
      | false -> Invalid t (* Overlong. *)

    let rec to_codepoint_impl fragment bt_opt =
      match bt_opt with
      | None -> Invalid fragment.t_one (* Extra 0x10xxxxxx byte. *)
      | Some (b, t') -> begin
          match Byte.(bit_clz (bit_not b)), fragment.n with
          | 0, 0 -> begin
              (* 0xxxxxxx *)
              let u' = Byte.to_uns b in
              let fragment' = {fragment with u=u'; n=succ fragment.n} in
              validate fragment' t'
            end
          | 1, 0
          | 1, 1
          | 1, 2 -> begin
              (* 10xxxxxx *)
              let fragment' = merge (bit_and 0x3f (Byte.to_uns b)) fragment in
              to_codepoint_impl fragment' (T.next t')
            end
          | 2, 1 -> begin
              (* 110xxxxx *)
              let fragment' = merge (bit_and 0x1f (Byte.to_uns b)) fragment in
              validate fragment' t'
            end
          | 3, 2 -> begin
              (* 1110xxxx *)
              let fragment' = merge (bit_and 0x0f (Byte.to_uns b)) fragment in
              validate fragment' t'
            end
          | 4, 3 -> begin
              (* 11110xxx *)
              let fragment' = merge (bit_and 0x07 (Byte.to_uns b)) fragment in
              validate fragment' t'
            end
          | lz, ncont when (lz >= ncont + 2) -> Invalid t'
          | _ -> Invalid fragment.t_one
        end

    let to_codepoint t =
      match T.next t with
      | None -> None
      | Some (_, t') as bt ->
        Some (to_codepoint_impl {u=0; n=0; t_one=t'} bt)
  end
end

(******************************************************************************)
(* Begin tests. *)

let%expect_test "hash_fold" =
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
  let us = [zero; one; min_value; max_codepoint] in
  test_hash_fold us;
  printf "@]";

  [%expect{|
    hash_fold 0x000000u21 -> 0xb465_a9ec_cd79_1cb6_4bbd_1bf2_7da9_18d6u128
    hash_fold 0x000001u21 -> 0x17ed_c9d0_759f_4dce_c1c4_c5ee_1138_72dbu128
    hash_fold 0x000000u21 -> 0xb465_a9ec_cd79_1cb6_4bbd_1bf2_7da9_18d6u128
    hash_fold 0x10ffffu21 -> 0x5017_4d83_ef7a_1f63_7f02_8e26_ef80_61ebu128
    |}]

let%expect_test "pp,pp_x" =
  let open Format in
  let rec fn = function
    | [] -> ()
    | x :: xs' -> begin
        printf "%a %a\n" pp x pp_x x;
        fn xs'
      end
  in
  printf "@[<h>";
  fn [kv 0; kv 1; kv 0x27; kv 0x41; replacement; max_codepoint];
  printf "@]";

  [%expect{|
    '\u{0}' 0x000000u21
    '\u{1}' 0x000001u21
    '\'' 0x000027u21
    'A' 0x000041u21
    '�' 0x00fffdu21
    '􏿿' 0x10ffffu21
    |}]

let%expect_test "rel" =
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
  fn (kv 0) (kv 0x10_0000);
  printf "\n";
  fn (kv 0) (kv 0x10_ffff);
  printf "\n";
  fn (kv 0x10_0000) (kv 0x10_ffff);
  let fn2 t min max = begin
    printf "\n";
    printf "clamp ~min:%a ~max:%a %a -> %a\n"
      pp_x min pp_x max pp_x t pp_x (clamp ~min ~max t);
    printf "between ~low:%a ~high:%a %a -> %b\n"
      pp_x min pp_x max pp_x t (between ~low:min ~high:max t);
  end in
  fn2 (kv 0x0f_fffe) (kv 0x0f_ffff) (kv 0x10_0001);
  fn2 (kv 0x0f_ffff) (kv 0x0f_ffff) (kv 0x10_0001);
  fn2 (kv 0x10_0000) (kv 0x0f_ffff) (kv 0x10_0001);
  fn2 (kv 0x10_0001) (kv 0x0f_ffff) (kv 0x10_0001);
  fn2 (kv 0x10_0002) (kv 0x0f_ffff) (kv 0x10_0001);

  [%expect{|
    cmp 0x000000u21 0x100000u21 -> Lt
    0x000000u21 >= 0x100000u21 -> false
    0x000000u21 <= 0x100000u21 -> true
    0x000000u21 = 0x100000u21 -> false
    0x000000u21 > 0x100000u21 -> false
    0x000000u21 < 0x100000u21 -> true
    0x000000u21 <> 0x100000u21 -> true
    ascending 0x000000u21 0x100000u21 -> Lt
    descending 0x000000u21 0x100000u21 -> Gt

    cmp 0x000000u21 0x10ffffu21 -> Lt
    0x000000u21 >= 0x10ffffu21 -> false
    0x000000u21 <= 0x10ffffu21 -> true
    0x000000u21 = 0x10ffffu21 -> false
    0x000000u21 > 0x10ffffu21 -> false
    0x000000u21 < 0x10ffffu21 -> true
    0x000000u21 <> 0x10ffffu21 -> true
    ascending 0x000000u21 0x10ffffu21 -> Lt
    descending 0x000000u21 0x10ffffu21 -> Gt

    cmp 0x100000u21 0x10ffffu21 -> Lt
    0x100000u21 >= 0x10ffffu21 -> false
    0x100000u21 <= 0x10ffffu21 -> true
    0x100000u21 = 0x10ffffu21 -> false
    0x100000u21 > 0x10ffffu21 -> false
    0x100000u21 < 0x10ffffu21 -> true
    0x100000u21 <> 0x10ffffu21 -> true
    ascending 0x100000u21 0x10ffffu21 -> Lt
    descending 0x100000u21 0x10ffffu21 -> Gt

    clamp ~min:0x0fffffu21 ~max:0x100001u21 0x0ffffeu21 -> 0x0fffffu21
    between ~low:0x0fffffu21 ~high:0x100001u21 0x0ffffeu21 -> false

    clamp ~min:0x0fffffu21 ~max:0x100001u21 0x0fffffu21 -> 0x0fffffu21
    between ~low:0x0fffffu21 ~high:0x100001u21 0x0fffffu21 -> true

    clamp ~min:0x0fffffu21 ~max:0x100001u21 0x100000u21 -> 0x100000u21
    between ~low:0x0fffffu21 ~high:0x100001u21 0x100000u21 -> true

    clamp ~min:0x0fffffu21 ~max:0x100001u21 0x100001u21 -> 0x100001u21
    between ~low:0x0fffffu21 ~high:0x100001u21 0x100001u21 -> true

    clamp ~min:0x0fffffu21 ~max:0x100001u21 0x100002u21 -> 0x100001u21
    between ~low:0x0fffffu21 ~high:0x100001u21 0x100002u21 -> false
    |}]

let%expect_test "conversion" =
  let open Format in
  let rec fn = function
    | [] -> ()
    | x :: xs' -> begin
        let i = sint_of_int x in
        let t = of_uns (Uns.of_sint i) in
        let u = to_uns t in
        let t' = of_uns u in
        printf "of_uns %a -> to_uns %a -> of_uns %a -> %a\n"
          Uns.pp_x x pp_x t Uns.pp_x u pp_x t';
        fn xs'
      end
  in
  fn [Uns.max_value; 0; 42; 0xd800; 0xdfff; 0x10_ffff; 0x11_0000; 0x20_0000;
    0x20_0001];

  [%expect{|
    of_uns 0x7fffffffffffffff -> to_uns 0x00fffdu21 -> of_uns 0x000000000000fffd -> 0x00fffdu21
    of_uns 0x0000000000000000 -> to_uns 0x000000u21 -> of_uns 0x0000000000000000 -> 0x000000u21
    of_uns 0x000000000000002a -> to_uns 0x00002au21 -> of_uns 0x000000000000002a -> 0x00002au21
    of_uns 0x000000000000d800 -> to_uns 0x00fffdu21 -> of_uns 0x000000000000fffd -> 0x00fffdu21
    of_uns 0x000000000000dfff -> to_uns 0x00fffdu21 -> of_uns 0x000000000000fffd -> 0x00fffdu21
    of_uns 0x000000000010ffff -> to_uns 0x10ffffu21 -> of_uns 0x000000000010ffff -> 0x10ffffu21
    of_uns 0x0000000000110000 -> to_uns 0x00fffdu21 -> of_uns 0x000000000000fffd -> 0x00fffdu21
    of_uns 0x0000000000200000 -> to_uns 0x000000u21 -> of_uns 0x0000000000000000 -> 0x000000u21
    of_uns 0x0000000000200001 -> to_uns 0x000001u21 -> of_uns 0x0000000000000001 -> 0x000001u21
    |}]

let%expect_test "bit_and,bit_or,bit_xor" =
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
    (kv 0, kv 0);
    (kv 0x10_ffff, kv 0);
    (kv 0, kv 0x10_ffff);
    (kv 0x10_ffff, kv 0x10_ffff);
  ] in
  test_pairs pairs;
  printf "@]";

  [%expect{|
    bit_{and,or,xor} 0x000000u21 0x000000u21 -> 0x000000u21, 0x000000u21, 0x000000u21
    bit_{and,or,xor} 0x10ffffu21 0x000000u21 -> 0x000000u21, 0x10ffffu21, 0x10ffffu21
    bit_{and,or,xor} 0x000000u21 0x10ffffu21 -> 0x000000u21, 0x10ffffu21, 0x10ffffu21
    bit_{and,or,xor} 0x10ffffu21 0x10ffffu21 -> 0x10ffffu21, 0x10ffffu21, 0x000000u21
    |}]

let%expect_test "bit_not" =
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
    kv 0;
    kv 0x10_ffff
  ] in
  test xs;
  printf "@]";

  [%expect{|
    bit_not 0x000000u21 -> 0x1fffffu21
    bit_not 0x10ffffu21 -> 0x0f0000u21
    |}]

let%expect_test "bit_pop,bit_clz,bit_ctz" =
  let open Format in
  printf "@[<h>";
  let rec test = function
    | [] -> ()
    | x :: xs' -> begin
        printf "bit_{pop,clz,ctz} %a -> %u, %u, %u\n"
          pp_x x (bit_pop x) (bit_clz x) (bit_ctz x);
        test xs'
      end
  in
  let xs = [
    kv 0;
    kv 1;
    kv 0x10_0000;
    kv 0xffff;
    kv 0x10_ffff;
  ] in
  test xs;
  printf "@]";

  [%expect{|
    bit_{pop,clz,ctz} 0x000000u21 -> 0, 21, 21
    bit_{pop,clz,ctz} 0x000001u21 -> 1, 20, 0
    bit_{pop,clz,ctz} 0x100000u21 -> 1, 0, 20
    bit_{pop,clz,ctz} 0x00ffffu21 -> 16, 5, 0
    bit_{pop,clz,ctz} 0x10ffffu21 -> 17, 0, 0
    |}]

let%expect_test "Utf8" =
  let open Format in
  let open Utf8 in
  let rec test_codepoints = function
    | [] -> ()
    | codepoint :: codepoints' -> begin
        let utf8 = of_codepoint codepoint in
        let codepoint' = to_codepoint utf8 in
        let bytes = to_bytes utf8 in
        let length = length utf8 in
        printf "codepoint=%a, codepoint'=%a, bytes=["
          pp_x codepoint pp_x codepoint';
        let rec bytes_iteri i = function
          | [] -> ()
          | b :: bytes' -> begin
              let space = if Uns.(i = 0) then "" else " " in
              let sep = if Uns.(succ i < length) then ";" else "" in
              printf "%s%a%s" space Byte.pp_x b sep;
              bytes_iteri (succ i) bytes'
            end
        in
        bytes_iteri 0 bytes;
        printf "], length=%a\n" Uns.pp length;
        test_codepoints codepoints'
      end
  in
  let codepoints =
    [
      (kv 0x3c); (* < *)
      (kv 0xab); (* « *)
      (kv 0x2021); (* ‡ *)
      (kv 0x10197); (* 𐆗 *)
    ]
  in
  test_codepoints codepoints;

  [%expect{|
    codepoint=0x00003cu21, codepoint'=0x00003cu21, bytes=[0x3cu8], length=1
    codepoint=0x0000abu21, codepoint'=0x0000abu21, bytes=[0xc2u8; 0xabu8], length=2
    codepoint=0x002021u21, codepoint'=0x002021u21, bytes=[0xe2u8; 0x80u8; 0xa1u8], length=3
    codepoint=0x010197u21, codepoint'=0x010197u21, bytes=[0xf0u8; 0x90u8; 0x86u8; 0x97u8], length=4
    |}]

let%expect_test "pp,to_string,escape" =
  let open Format in

  let rec fn i = begin
    match i with
    | 0x80 -> ()
    | _ -> begin
        let cp = of_uns i in
        let utf8 = Utf8.of_codepoint cp in
        printf "%a -> %a {|%s|} \"%a\"\n"
          Uns.pp_x i
          pp cp
          (if i > 0x1f && i < 0x7f then (to_string cp) else "�")
          Utf8.pp utf8;
        fn (Uns.succ i)
      end
  end in
  fn 0;

  [%expect{xxx|
    0x0000000000000000 -> '\u{0}' {|�|} "\u{0}"
    0x0000000000000001 -> '\u{1}' {|�|} "\u{1}"
    0x0000000000000002 -> '\u{2}' {|�|} "\u{2}"
    0x0000000000000003 -> '\u{3}' {|�|} "\u{3}"
    0x0000000000000004 -> '\u{4}' {|�|} "\u{4}"
    0x0000000000000005 -> '\u{5}' {|�|} "\u{5}"
    0x0000000000000006 -> '\u{6}' {|�|} "\u{6}"
    0x0000000000000007 -> '\u{7}' {|�|} "\u{7}"
    0x0000000000000008 -> '\u{8}' {|�|} "\u{8}"
    0x0000000000000009 -> '\t' {|�|} "\t"
    0x000000000000000a -> '\n' {|�|} "\n"
    0x000000000000000b -> '\u{b}' {|�|} "\u{b}"
    0x000000000000000c -> '\u{c}' {|�|} "\u{c}"
    0x000000000000000d -> '\r' {|�|} "\r"
    0x000000000000000e -> '\u{e}' {|�|} "\u{e}"
    0x000000000000000f -> '\u{f}' {|�|} "\u{f}"
    0x0000000000000010 -> '\u{10}' {|�|} "\u{10}"
    0x0000000000000011 -> '\u{11}' {|�|} "\u{11}"
    0x0000000000000012 -> '\u{12}' {|�|} "\u{12}"
    0x0000000000000013 -> '\u{13}' {|�|} "\u{13}"
    0x0000000000000014 -> '\u{14}' {|�|} "\u{14}"
    0x0000000000000015 -> '\u{15}' {|�|} "\u{15}"
    0x0000000000000016 -> '\u{16}' {|�|} "\u{16}"
    0x0000000000000017 -> '\u{17}' {|�|} "\u{17}"
    0x0000000000000018 -> '\u{18}' {|�|} "\u{18}"
    0x0000000000000019 -> '\u{19}' {|�|} "\u{19}"
    0x000000000000001a -> '\u{1a}' {|�|} "\u{1a}"
    0x000000000000001b -> '\u{1b}' {|�|} "\u{1b}"
    0x000000000000001c -> '\u{1c}' {|�|} "\u{1c}"
    0x000000000000001d -> '\u{1d}' {|�|} "\u{1d}"
    0x000000000000001e -> '\u{1e}' {|�|} "\u{1e}"
    0x000000000000001f -> '\u{1f}' {|�|} "\u{1f}"
    0x0000000000000020 -> ' ' {| |} " "
    0x0000000000000021 -> '!' {|!|} "!"
    0x0000000000000022 -> '"' {|"|} "\""
    0x0000000000000023 -> '#' {|#|} "#"
    0x0000000000000024 -> '$' {|$|} "$"
    0x0000000000000025 -> '%' {|%|} "%"
    0x0000000000000026 -> '&' {|&|} "&"
    0x0000000000000027 -> '\'' {|'|} "'"
    0x0000000000000028 -> '(' {|(|} "("
    0x0000000000000029 -> ')' {|)|} ")"
    0x000000000000002a -> '*' {|*|} "*"
    0x000000000000002b -> '+' {|+|} "+"
    0x000000000000002c -> ',' {|,|} ","
    0x000000000000002d -> '-' {|-|} "-"
    0x000000000000002e -> '.' {|.|} "."
    0x000000000000002f -> '/' {|/|} "/"
    0x0000000000000030 -> '0' {|0|} "0"
    0x0000000000000031 -> '1' {|1|} "1"
    0x0000000000000032 -> '2' {|2|} "2"
    0x0000000000000033 -> '3' {|3|} "3"
    0x0000000000000034 -> '4' {|4|} "4"
    0x0000000000000035 -> '5' {|5|} "5"
    0x0000000000000036 -> '6' {|6|} "6"
    0x0000000000000037 -> '7' {|7|} "7"
    0x0000000000000038 -> '8' {|8|} "8"
    0x0000000000000039 -> '9' {|9|} "9"
    0x000000000000003a -> ':' {|:|} ":"
    0x000000000000003b -> ';' {|;|} ";"
    0x000000000000003c -> '<' {|<|} "<"
    0x000000000000003d -> '=' {|=|} "="
    0x000000000000003e -> '>' {|>|} ">"
    0x000000000000003f -> '?' {|?|} "?"
    0x0000000000000040 -> '@' {|@|} "@"
    0x0000000000000041 -> 'A' {|A|} "A"
    0x0000000000000042 -> 'B' {|B|} "B"
    0x0000000000000043 -> 'C' {|C|} "C"
    0x0000000000000044 -> 'D' {|D|} "D"
    0x0000000000000045 -> 'E' {|E|} "E"
    0x0000000000000046 -> 'F' {|F|} "F"
    0x0000000000000047 -> 'G' {|G|} "G"
    0x0000000000000048 -> 'H' {|H|} "H"
    0x0000000000000049 -> 'I' {|I|} "I"
    0x000000000000004a -> 'J' {|J|} "J"
    0x000000000000004b -> 'K' {|K|} "K"
    0x000000000000004c -> 'L' {|L|} "L"
    0x000000000000004d -> 'M' {|M|} "M"
    0x000000000000004e -> 'N' {|N|} "N"
    0x000000000000004f -> 'O' {|O|} "O"
    0x0000000000000050 -> 'P' {|P|} "P"
    0x0000000000000051 -> 'Q' {|Q|} "Q"
    0x0000000000000052 -> 'R' {|R|} "R"
    0x0000000000000053 -> 'S' {|S|} "S"
    0x0000000000000054 -> 'T' {|T|} "T"
    0x0000000000000055 -> 'U' {|U|} "U"
    0x0000000000000056 -> 'V' {|V|} "V"
    0x0000000000000057 -> 'W' {|W|} "W"
    0x0000000000000058 -> 'X' {|X|} "X"
    0x0000000000000059 -> 'Y' {|Y|} "Y"
    0x000000000000005a -> 'Z' {|Z|} "Z"
    0x000000000000005b -> '[' {|[|} "["
    0x000000000000005c -> '\\' {|\|} "\\"
    0x000000000000005d -> ']' {|]|} "]"
    0x000000000000005e -> '^' {|^|} "^"
    0x000000000000005f -> '_' {|_|} "_"
    0x0000000000000060 -> '`' {|`|} "`"
    0x0000000000000061 -> 'a' {|a|} "a"
    0x0000000000000062 -> 'b' {|b|} "b"
    0x0000000000000063 -> 'c' {|c|} "c"
    0x0000000000000064 -> 'd' {|d|} "d"
    0x0000000000000065 -> 'e' {|e|} "e"
    0x0000000000000066 -> 'f' {|f|} "f"
    0x0000000000000067 -> 'g' {|g|} "g"
    0x0000000000000068 -> 'h' {|h|} "h"
    0x0000000000000069 -> 'i' {|i|} "i"
    0x000000000000006a -> 'j' {|j|} "j"
    0x000000000000006b -> 'k' {|k|} "k"
    0x000000000000006c -> 'l' {|l|} "l"
    0x000000000000006d -> 'm' {|m|} "m"
    0x000000000000006e -> 'n' {|n|} "n"
    0x000000000000006f -> 'o' {|o|} "o"
    0x0000000000000070 -> 'p' {|p|} "p"
    0x0000000000000071 -> 'q' {|q|} "q"
    0x0000000000000072 -> 'r' {|r|} "r"
    0x0000000000000073 -> 's' {|s|} "s"
    0x0000000000000074 -> 't' {|t|} "t"
    0x0000000000000075 -> 'u' {|u|} "u"
    0x0000000000000076 -> 'v' {|v|} "v"
    0x0000000000000077 -> 'w' {|w|} "w"
    0x0000000000000078 -> 'x' {|x|} "x"
    0x0000000000000079 -> 'y' {|y|} "y"
    0x000000000000007a -> 'z' {|z|} "z"
    0x000000000000007b -> '{' {|{|} "{"
    0x000000000000007c -> '|' {|||} "|"
    0x000000000000007d -> '}' {|}|} "}"
    0x000000000000007e -> '~' {|~|} "~"
    0x000000000000007f -> '\u{7f}' {|�|} "\u{7f}"
    |xxx}]
