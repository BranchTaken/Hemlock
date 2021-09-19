(* Partial Rudiments. *)
open RudimentsInt
open RudimentsFunctions
type byte = Byte.t

module T = struct
  type t = uns
  let bit_length = 21
end
include T
include Intnb.MakeU(T)

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

let of_uns_opt x =
  let t = of_uns x in
  let x' = to_uns t in
  match Uns.(x' = x) with
  | false -> None
  | true -> Some t

let of_uns_hlt x =
  match of_uns_opt x with
  | None -> halt "Lossy conversion"
  | Some t -> t

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
        Byte.of_uns Uns.(bit_or 0b10_000000 (bit_and (bit_usr ~shift:6 u) 0x3f)),
        Byte.of_uns Uns.(bit_or 0b10_000000 (bit_and u 0x3f))
      )
    | 4 ->
      Four (
        Byte.of_uns Uns.(bit_or 0b11110_000 (bit_usr ~shift:18 u)),
        Byte.of_uns Uns.(bit_or 0b10_000000 (bit_and (bit_usr ~shift:12 u) 0x3f)),
        Byte.of_uns Uns.(bit_or 0b10_000000 (bit_and (bit_usr ~shift:6 u) 0x3f)),
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

  module Make (T : SeqIntf.IMonoIndef with type elm := byte) :
    S with type t := T.t = struct
    type fragment = {
      u: uns;
      n: uns;
      nrem: uns;
    }

    (* It should be equivalent to write t vs T.t , but the compiler gets confused unless we write
     * T.t here. An alternative is to explicitly define outer and t as:
     *
     *   type nonrec outer = outer type t = T.t *)
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
          | Some (b, _) when Byte.((bit_and b (kv 0b11_000000)) <> (kv 0b10_000000)) -> Invalid t
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
              Some (to_codepoint_cont {u=(bit_and 0x1f (Byte.to_uns b)); n=2; nrem=1} t')
            end
          | 3 -> begin
              (* 1110xxxx *)
              Some (to_codepoint_cont {u=(bit_and 0x0f (Byte.to_uns b)); n=3; nrem=2} t')
            end
          | 4 -> begin
              (* 11110xxx *)
              Some (to_codepoint_cont {u=(bit_and 0x07 (Byte.to_uns b)); n=4; nrem=3} t')
            end
          | _ -> Some (Invalid t')
        end
  end

  module MakeRev (T : SeqIntf.IMonoIndef with type elm := byte) :
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
