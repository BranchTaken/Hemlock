(* Partial Rudiments. *)
open RudimentsInt0
open RudimentsFunctions
type byte = Byte.t

module T = struct
  module U = struct
    type t = uns
    let bit_length = 21L
  end
  include U
  include Intnb.MakeU(U)

  let max_codepoint = narrow_of_unsigned 0x10ffffL
  let replacement = 0xfffdL

  let is_surrogate t =
    t >= 0xd800L && t < 0xe000L

  let narrow_of_unsigned u =
    let t = narrow_of_unsigned u in
    match t > max_codepoint || is_surrogate t with
    | true -> replacement
    | false -> t
end
include T
include Convert.Make_nbU(T)

let of_char c =
  Int64.of_int (Stdlib.Char.code c)

let nul = 0x00L
let soh = 0x01L
let stx = 0x02L
let etx = 0x03L
let eot = 0x04L
let enq = 0x05L
let ack = 0x06L
let bel = 0x07L
let bs = 0x08L
let ht = of_char '\t'
let lf = of_char '\n'
let nl = of_char '\n'
let vt = 0x0bL
let ff = 0x0cL
let cr = of_char '\r'
let so = 0x0eL
let si = 0x0fL
let dle = 0x10L
let dc1 = 0x11L
let dc2 = 0x12L
let dc3 = 0x13L
let dc4 = 0x14L
let nak = 0x15L
let syn = 0x16L
let etb = 0x17L
let can = 0x18L
let em = 0x19L
let sub = 0x1aL
let esc = 0x1bL
let fs = 0x1cL
let gs = 0x1dL
let rs = 0x1eL
let us = 0x1fL
let del = 0x7fL

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
    let sigbits = 21L - lz in
    Stdlib.Array.get [|
      1L; 1L; 1L; 1L; 1L; 1L; 1L; 1L; (* [0..7] *)
      2L; 2L; 2L; 2L;                 (* [8..11] *)
      3L; 3L; 3L; 3L; 3L;             (* [12..16] *)
      4L; 4L; 4L; 4L; 4L;             (* [17..21] *)
    |] (Int64.to_int sigbits)

  let of_codepoint cp =
    assert (cp <= max_codepoint);
    let u = extend_to_uns cp in
    match length_of_codepoint cp with
    | 1L -> One (Byte.trunc_of_uns u)
    | 2L ->
      Two (
        Byte.trunc_of_uns Uns.(bit_or 0b110_00000L (bit_usr ~shift:6L u)),
        Byte.trunc_of_uns Uns.(bit_or 0b10_000000L (bit_and u 0x3fL))
      )
    | 3L ->
      Three (
        Byte.trunc_of_uns Uns.(bit_or 0b1110_0000L (bit_usr ~shift:12L u)),
        Byte.trunc_of_uns Uns.(bit_or 0b10_000000L (bit_and (bit_usr ~shift:6L u) 0x3fL)),
        Byte.trunc_of_uns Uns.(bit_or 0b10_000000L (bit_and u 0x3fL))
      )
    | 4L ->
      Four (
        Byte.trunc_of_uns Uns.(bit_or 0b11110_000L (bit_usr ~shift:18L u)),
        Byte.trunc_of_uns Uns.(bit_or 0b10_000000L (bit_and (bit_usr ~shift:12L u) 0x3fL)),
        Byte.trunc_of_uns Uns.(bit_or 0b10_000000L (bit_and (bit_usr ~shift:6L u) 0x3fL)),
        Byte.trunc_of_uns Uns.(bit_or 0b10_000000L (bit_and u 0x3fL))
      )
    | _ -> not_reached ()

  let to_codepoint = function
    | One b0 -> trunc_of_uns (Byte.extend_to_uns b0)
    | Two (b0, b1) ->
      trunc_of_uns Uns.(bit_or
          (bit_sl ~shift:6L (bit_and (Byte.extend_to_uns b0) 0x1fL))
          (bit_and (Byte.extend_to_uns b1) 0x3fL))
    | Three (b0, b1, b2) ->
      trunc_of_uns Uns.(bit_or (bit_or
          (bit_sl ~shift:12L (bit_and (Byte.extend_to_uns b0) 0xfL))
          (bit_sl ~shift:6L (bit_and (Byte.extend_to_uns b1) 0x3fL)))
        (bit_and (Byte.extend_to_uns b2) 0x3fL))
    | Four (b0, b1, b2, b3) ->
      trunc_of_uns Uns.(bit_or (bit_or (bit_or
          (bit_sl ~shift:18L (bit_and (Byte.extend_to_uns b0) 0x7L))
          (bit_sl ~shift:12L (bit_and (Byte.extend_to_uns b1) 0x3fL)))
        (bit_sl ~shift:6L (bit_and (Byte.extend_to_uns b2) 0x3fL)))
        (bit_and (Byte.extend_to_uns b3) 0x3fL))

  let to_bytes = function
    | One b0 -> [b0]
    | Two (b0, b1) -> [b0; b1]
    | Three (b0, b1, b2) -> [b0; b1; b2]
    | Four (b0, b1, b2, b3) -> [b0; b1; b2; b3]

  let length = function
    | One _ -> 1L
    | Two _ -> 2L
    | Three _ -> 3L
    | Four _ -> 4L

  let to_string = function
    | One b0 -> Stdlib.String.init 1 (fun _ ->
      Stdlib.Char.chr (int_of_sint (Byte.extend_to_sint b0))
    )
    | Two (b0, b1) -> Stdlib.String.init 2 (fun i ->
      Stdlib.Char.chr (int_of_sint (Byte.extend_to_sint (
        match i with
        | 0 -> b0
        | 1 -> b1
        | _ -> not_reached ()
      ))))
    | Three (b0, b1, b2) -> Stdlib.String.init 3 (fun i ->
      Stdlib.Char.chr (int_of_sint (Byte.extend_to_sint (
        match i with
        | 0 -> b0
        | 1 -> b1
        | 2 -> b2
        | _ -> not_reached ()
      ))))
    | Four (b0, b1, b2, b3) -> Stdlib.String.init 4 (fun i ->
      Stdlib.Char.chr (int_of_sint (Byte.extend_to_sint (
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
        match trunc_of_uns (Byte.extend_to_uns b0) with
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
      let len = (Int64.of_int (Stdlib.String.length utf8_str)) + 2L in
      Stdlib.String.init (Int64.to_int len) (fun i ->
        if Stdlib.(i = 0) || i == (Int64.to_int (pred len)) then
          '\''
        else
          Stdlib.String.get utf8_str Stdlib.(i - 1)
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
      | 0L -> begin
          let cp = trunc_of_uns fragment.u in
          match Uns.((Utf8.length_of_codepoint cp) = fragment.n) with
          | true -> Valid (cp, t)
          | false -> Invalid t
        end
      | _ -> begin
          match T.next t with
          | None -> Invalid t
          | Some (b, _) when Byte.((bit_and b (kv 0b11_000000L)) <> (kv 0b10_000000L)) -> Invalid t
          | Some (b, t') -> begin
              let u' = bit_or (bit_sl ~shift:6L fragment.u)
                (bit_and 0x3fL (Byte.extend_to_uns b)) in
              to_codepoint_cont {fragment with u=u'; nrem=pred fragment.nrem} t'
            end
        end

    let to_codepoint t =
      match T.next t with
      | None -> None
      | Some (b, t') -> begin
          match Byte.(bit_clz (bit_not b)) with
          | 0L -> begin
              (* 0xxxxxxx *)
              Some (to_codepoint_cont {u=(Byte.extend_to_uns b); n=1L; nrem=0L} t')
            end
          | 2L -> begin
              (* 110xxxxx *)
              Some (to_codepoint_cont {u=(bit_and 0x1fL (Byte.extend_to_uns b)); n=2L; nrem=1L} t')
            end
          | 3L -> begin
              (* 1110xxxx *)
              Some (to_codepoint_cont {u=(bit_and 0x0fL (Byte.extend_to_uns b)); n=3L; nrem=2L} t')
            end
          | 4L -> begin
              (* 11110xxx *)
              Some (to_codepoint_cont {u=(bit_and 0x07L (Byte.extend_to_uns b)); n=4L; nrem=3L} t')
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
      let u' = bit_or (bit_sl ~shift:(fragment.n*6L) x) fragment.u in
      let n' = succ fragment.n in
      {fragment with u=u'; n=n'}

    let validate fragment t =
      let cp = trunc_of_uns fragment.u in
      match Uns.((Utf8.length_of_codepoint cp) = fragment.n) with
      | true -> Valid (cp, t)
      | false -> Invalid t (* Overlong. *)

    let rec to_codepoint_impl fragment bt_opt =
      match bt_opt with
      | None -> Invalid fragment.t_one (* Extra 0x10xxxxxx byte. *)
      | Some (b, t') -> begin
          match Byte.(bit_clz (bit_not b)), fragment.n with
          | 0L, 0L -> begin
              (* 0xxxxxxx *)
              let u' = Byte.extend_to_uns b in
              let fragment' = {fragment with u=u'; n=succ fragment.n} in
              validate fragment' t'
            end
          | 1L, 0L
          | 1L, 1L
          | 1L, 2L -> begin
              (* 10xxxxxx *)
              let fragment' = merge (bit_and 0x3fL (Byte.extend_to_uns b)) fragment in
              to_codepoint_impl fragment' (T.next t')
            end
          | 2L, 1L -> begin
              (* 110xxxxx *)
              let fragment' = merge (bit_and 0x1fL (Byte.extend_to_uns b)) fragment in
              validate fragment' t'
            end
          | 3L, 2L -> begin
              (* 1110xxxx *)
              let fragment' = merge (bit_and 0x0fL (Byte.extend_to_uns b)) fragment in
              validate fragment' t'
            end
          | 4L, 3L -> begin
              (* 11110xxx *)
              let fragment' = merge (bit_and 0x07L (Byte.extend_to_uns b)) fragment in
              validate fragment' t'
            end
          | lz, ncont when (lz >= ncont + 2L) -> Invalid t'
          | _ -> Invalid fragment.t_one
        end

    let to_codepoint t =
      match T.next t with
      | None -> None
      | Some (_, t') as bt ->
        Some (to_codepoint_impl {u=0L; n=0L; t_one=t'} bt)
  end
end
