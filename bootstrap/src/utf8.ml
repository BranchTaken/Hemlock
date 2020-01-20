open Rudiments

module T = struct
  type t =
  | One   of byte
  | Two   of byte * byte
  | Three of byte * byte * byte
  | Four  of byte * byte * byte * byte

  let of_codepoint cp =
    assert (Codepoint.(cp <= (kv 0x10ffff)));
    let lz = Codepoint.bit_clz cp in
    let sigbits = Int.(21 - (Uint.to_int lz)) in
    if Int.(sigbits < 8) then
      One (Byte.of_codepoint cp)
    else if Int.(sigbits < 12) then
      Two (
        Byte.(bit_or (kv 0b110_00000) (of_codepoint (Codepoint.bit_usr cp
                (Uint.kv 6)))),
        Byte.(bit_or (kv 0b10_000000)
            (of_codepoint (Codepoint.(bit_and cp (kv 0x3f)))))
      )
    else if Int.(sigbits < 17) then
      Three (
        Byte.(bit_or (kv 0b1110_0000) (of_codepoint (Codepoint.bit_usr cp
                (Uint.kv 12)))),
        Byte.(bit_or (kv 0b10_000000) (of_codepoint
              Codepoint.(bit_and (bit_usr cp (Uint.kv 6)) (kv 0x3f)))),
        Byte.(bit_or (kv 0b10_000000)
            (of_codepoint Codepoint.(bit_and cp (kv 0x3f))))
      )
    else if Int.(sigbits < 22) then
      Four (
        Byte.(bit_or (kv 0b11110_000) (of_codepoint (Codepoint.bit_usr cp
                (Uint.kv 18)))),
        Byte.(bit_or (kv 0b10_000000) (of_codepoint
              Codepoint.(bit_and (bit_usr cp (Uint.kv 12)) (kv 0x3f)))),
        Byte.(bit_or (kv 0b10_000000) (of_codepoint
              Codepoint.(bit_and (bit_usr cp (Uint.kv 6)) (kv 0x3f)))),
        Byte.(bit_or (kv 0b10_000000)
            (of_codepoint Codepoint.(bit_and cp (kv 0x3f))))
      )
    else not_reached ()

  let to_codepoint = function
    | One b0 -> Byte.to_codepoint b0
    | Two (b0, b1) -> Codepoint.(bit_or
          Byte.(to_codepoint (bit_sl (bit_and b0 (kv 0x1f)) (Uint.kv 6)))
          Byte.(to_codepoint (bit_and b1 (kv 0x3f))))
    | Three (b0, b1, b2) -> Codepoint.(bit_or (bit_or
            Codepoint.(bit_sl Byte.(to_codepoint (bit_and b0 (kv 0xf)))
                (Uint.kv 12))
            Codepoint.(bit_sl Byte.(to_codepoint (bit_and b1 (kv 0x3f)))
                (Uint.kv 6)))
          Byte.(to_codepoint (bit_and b2 (kv 0x3f))))
    | Four (b0, b1, b2, b3) -> Codepoint.(bit_or (bit_or (bit_or
              Codepoint.(bit_sl Byte.(to_codepoint (bit_and b0 (kv 0x7)))
                  (Uint.kv 18))
              Codepoint.(bit_sl Byte.(to_codepoint (bit_and b1 (kv 0x3f)))
                  (Uint.kv 12)))
            Codepoint.(bit_sl Byte.(to_codepoint (bit_and b2 (kv 0x3f)))
                (Uint.kv 6)))
          Byte.(to_codepoint (bit_and b3 (kv 0x3f))))

  let cmp t0 t1 =
    Codepoint.cmp (to_codepoint t0) (to_codepoint t1)
end
include T
include Cmpable.Make(T)

module Seq = struct
  type outer = t
  module type S = sig
    type t
    val to_utf8: t -> ((outer, byte list) result * t) option
    val to_utf8_hlt: t -> (outer * t) option
  end

  module Make (T : Seq_intf.I_mono_indef with type elm := byte) :
    S with type t := T.t = struct
    let to_utf8 t =
      let rec fn t bytes nrem = begin
        match nrem with
        | nrem when Uint.(nrem = (kv 0)) -> begin
            match bytes with
            |                   b0 :: [] -> Some (Ok (One b0), t)
            |             b1 :: b0 :: [] -> Some (Ok (Two (b0, b1)), t)
            |       b2 :: b1 :: b0 :: [] -> Some (Ok (Three (b0, b1, b2)), t)
            | b3 :: b2 :: b1 :: b0 :: [] -> Some (Ok (Four (b0, b1, b2, b3)), t)
            | _ -> not_reached ()
          end
        | _ -> begin
            match T.next t with
            | None -> Some (Error bytes, t)
            | Some (b, t')
              when Byte.((bit_and b (kv 0b11_000000)) <> (kv 0b10_000000)) ->
              Some (Error (List.rev (b :: bytes)), t')
            | Some (b, t') -> fn t' (b :: bytes) (Uint.pred nrem)
          end
      end in
      match T.next t with
      | None -> None
      | Some (b, t') -> begin
          match Byte.(bit_clz (bit_not b)) with
            | lz when Uint.(lz = (kv 0)) -> fn t' [b] (Uint.kv 0)
            | lz when Uint.(lz = (kv 2)) -> fn t' [b] (Uint.kv 1)
            | lz when Uint.(lz = (kv 3)) -> fn t' [b] (Uint.kv 2)
            | lz when Uint.(lz = (kv 4)) -> fn t' [b] (Uint.kv 3)
            | _ -> Some (Error [b], t')
        end

    let to_utf8_hlt t =
      match to_utf8 t with
      | Some (Error _, _) -> halt "Invalid utf8 sequence"
      | Some (Ok utf8, t') -> Some (utf8, t')
      | None -> None
  end

  module Make_rev (T : Seq_intf.I_mono_indef with type elm := byte) :
    S with type t := T.t = struct
    let to_utf8 t =
      let rec fn t bytes = begin
        match (T.next t), bytes with
        | None, [] -> None
        | None, _ :: _ -> Some (Error bytes, t)
        | Some (b, t'), _ -> begin
            let bytes' = b :: bytes in
            match Uint.to_int Byte.(bit_clz (bit_not b)), bytes' with
            | 0, b0 :: [] -> Some (Ok (One b0), t')
            | 2, b0 :: b1 :: [] ->
              Some (Ok (Two (b0, b1)), t')
            | 3, b0 :: b1 :: b2 :: [] ->
              Some (Ok (Three (b0, b1, b2)), t')
            | 4, b0 :: b1 :: b2 :: b3 :: [] ->
              Some (Ok (Four (b0, b1, b2, b3)), t')
            | 1, _ ->
              (* It's possible that an excessive number of 0b10_xxxxxx bytes
               * will be processed, but these excesses will cause match failure
               * later on. *)
              fn t' bytes'
            | _ -> Some (Error bytes', t')
          end
      end in
      fn t []

    let to_utf8_hlt t =
      match to_utf8 t with
      | Some (Error _, _) -> halt "Invalid utf8 sequence"
      | Some (Ok utf8, t') -> Some (utf8, t')
      | None -> None
  end
end

let to_bytes = function
  | One b0 -> [b0]
  | Two (b0, b1) -> [b0; b1]
  | Three (b0, b1, b2) -> [b0; b1; b2]
  | Four (b0, b1, b2, b3) -> [b0; b1; b2; b3]

let length = function
  | One _ -> Uint.kv 1
  | Two _ -> Uint.kv 2
  | Three _ -> Uint.kv 3
  | Four _ -> Uint.kv 4

let to_string = function
  | One b0 -> Stdlib.String.init 1 (fun _ -> Stdlib.Char.chr (Byte.to_int b0))
  | Two (b0, b1) -> Stdlib.String.init 2 (fun i ->
      Stdlib.Char.chr (Byte.to_int (
        match i with
        | 0 -> b0
        | 1 -> b1
        | _ -> not_reached ()
    )))
  | Three (b0, b1, b2) -> Stdlib.String.init 3 (fun i ->
      Stdlib.Char.chr (Byte.to_int (
        match i with
        | 0 -> b0
        | 1 -> b1
        | 2 -> b2
        | _ -> not_reached ()
    )))
  | Four (b0, b1, b2, b3) -> Stdlib.String.init 4 (fun i ->
      Stdlib.Char.chr (Byte.to_int (
        match i with
        | 0 -> b0
        | 1 -> b1
        | 2 -> b2
        | 3 -> b3
        | _ -> not_reached ()
    )))

let escape t =
  match t with
  | One b0 -> begin
      match Byte.to_codepoint b0 with
      | cp when Codepoint.(cp = nul) -> "\\0"
      | cp when Codepoint.(cp = soh) -> "\\x01"
      | cp when Codepoint.(cp = stx) -> "\\x02"
      | cp when Codepoint.(cp = etx) -> "\\x03"
      | cp when Codepoint.(cp = eot) -> "\\x04"
      | cp when Codepoint.(cp = enq) -> "\\x05"
      | cp when Codepoint.(cp = ack) -> "\\x06"
      | cp when Codepoint.(cp = bel) -> "\\a"
      | cp when Codepoint.(cp = bs) -> "\\b"
      | cp when Codepoint.(cp = ht) -> "\\t"
      | cp when Codepoint.(cp = nl) -> "\\n"
      | cp when Codepoint.(cp = vt) -> "\\v"
      | cp when Codepoint.(cp = ff) -> "\\f"
      | cp when Codepoint.(cp = cr) -> "\\r"
      | cp when Codepoint.(cp = so) -> "\\x0e"
      | cp when Codepoint.(cp = si) -> "\\x0f"
      | cp when Codepoint.(cp = dle) -> "\\x10"
      | cp when Codepoint.(cp = dc1) -> "\\x11"
      | cp when Codepoint.(cp = dc2) -> "\\x12"
      | cp when Codepoint.(cp = dc3) -> "\\x13"
      | cp when Codepoint.(cp = dc4) -> "\\x14"
      | cp when Codepoint.(cp = nak) -> "\\x15"
      | cp when Codepoint.(cp = syn) -> "\\x16"
      | cp when Codepoint.(cp = etb) -> "\\x17"
      | cp when Codepoint.(cp = can) -> "\\x18"
      | cp when Codepoint.(cp = em) -> "\\x19"
      | cp when Codepoint.(cp = sub) -> "\\x1a"
      | cp when Codepoint.(cp = esc) -> "\\x1b"
      | cp when Codepoint.(cp = fs) -> "\\x1c"
      | cp when Codepoint.(cp = gs) -> "\\x1d"
      | cp when Codepoint.(cp = rs) -> "\\x1e"
      | cp when Codepoint.(cp = us) -> "\\x1f"
      | cp when Codepoint.(cp = (of_char '"')) -> "\\\""
      | cp when Codepoint.(cp = (of_char '\\')) -> "\\\\"
      | cp when Codepoint.(cp = del) -> "\\x7f"
      | _ -> to_string t
    end
  | Two _
  | Three _
  | Four _ -> to_string t

let pp ppf t =
  Format.fprintf ppf "\"%s\"" (escape t)

(*******************************************************************************
 * Begin tests.
 *)

let%expect_test "utf8" =
  let open Format in
  let codepoints = begin
    let open Codepoint in
    [
      (kv 0x3c); (* < *)
      (kv 0xab); (* « *)
      (kv 0x2021); (* ‡ *)
      (kv 0x10197); (* 𐆗 *)
    ]
  end in
  List.iter codepoints ~f:(fun codepoint ->
    let utf8 = of_codepoint codepoint in
    let codepoint' = to_codepoint utf8 in
    let bytes = to_bytes utf8 in
    let length = length utf8 in
    printf "codepoint=%a, codepoint'=%a, bytes=["
      Codepoint.pp_x codepoint Codepoint.pp_x codepoint';
    List.iteri bytes ~f:(fun i b ->
      let space = if Uint.(i = (kv 0)) then "" else " " in
      let sep = if Uint.(succ i < length) then ";" else "" in
      printf "%s%a%s" space Byte.pp_x b sep
    );
    printf "], length=%a\n" Uint.pp length
  );

  [%expect{|
    codepoint=0x3c, codepoint'=0x3c, bytes=[0x3c], length=1
    codepoint=0xab, codepoint'=0xab, bytes=[0xc2; 0xab], length=2
    codepoint=0x2021, codepoint'=0x2021, bytes=[0xe2; 0x80; 0xa1], length=3
    codepoint=0x10197, codepoint'=0x10197, bytes=[0xf0; 0x90; 0x86; 0x97], length=4
    |}]

let%expect_test "pp,escape" =
  let open Format in

  let rec fn i = begin
    match i with
    | i when Uint.(i = (kv 0x80)) -> ()
    | _ -> begin
        let utf8 = of_codepoint Codepoint.(of_uint i) in
        printf "0x%02x -> %a\n" (Uint.to_int i) pp utf8;
        fn (Uint.succ i)
      end
  end in
  fn (Uint.kv 0);

  [%expect{|
    0x00 -> "\0"
    0x01 -> "\x01"
    0x02 -> "\x02"
    0x03 -> "\x03"
    0x04 -> "\x04"
    0x05 -> "\x05"
    0x06 -> "\x06"
    0x07 -> "\a"
    0x08 -> "\b"
    0x09 -> "\t"
    0x0a -> "\n"
    0x0b -> "\v"
    0x0c -> "\f"
    0x0d -> "\r"
    0x0e -> "\x0e"
    0x0f -> "\x0f"
    0x10 -> "\x10"
    0x11 -> "\x11"
    0x12 -> "\x12"
    0x13 -> "\x13"
    0x14 -> "\x14"
    0x15 -> "\x15"
    0x16 -> "\x16"
    0x17 -> "\x17"
    0x18 -> "\x18"
    0x19 -> "\x19"
    0x1a -> "\x1a"
    0x1b -> "\x1b"
    0x1c -> "\x1c"
    0x1d -> "\x1d"
    0x1e -> "\x1e"
    0x1f -> "\x1f"
    0x20 -> " "
    0x21 -> "!"
    0x22 -> "\""
    0x23 -> "#"
    0x24 -> "$"
    0x25 -> "%"
    0x26 -> "&"
    0x27 -> "'"
    0x28 -> "("
    0x29 -> ")"
    0x2a -> "*"
    0x2b -> "+"
    0x2c -> ","
    0x2d -> "-"
    0x2e -> "."
    0x2f -> "/"
    0x30 -> "0"
    0x31 -> "1"
    0x32 -> "2"
    0x33 -> "3"
    0x34 -> "4"
    0x35 -> "5"
    0x36 -> "6"
    0x37 -> "7"
    0x38 -> "8"
    0x39 -> "9"
    0x3a -> ":"
    0x3b -> ";"
    0x3c -> "<"
    0x3d -> "="
    0x3e -> ">"
    0x3f -> "?"
    0x40 -> "@"
    0x41 -> "A"
    0x42 -> "B"
    0x43 -> "C"
    0x44 -> "D"
    0x45 -> "E"
    0x46 -> "F"
    0x47 -> "G"
    0x48 -> "H"
    0x49 -> "I"
    0x4a -> "J"
    0x4b -> "K"
    0x4c -> "L"
    0x4d -> "M"
    0x4e -> "N"
    0x4f -> "O"
    0x50 -> "P"
    0x51 -> "Q"
    0x52 -> "R"
    0x53 -> "S"
    0x54 -> "T"
    0x55 -> "U"
    0x56 -> "V"
    0x57 -> "W"
    0x58 -> "X"
    0x59 -> "Y"
    0x5a -> "Z"
    0x5b -> "["
    0x5c -> "\\"
    0x5d -> "]"
    0x5e -> "^"
    0x5f -> "_"
    0x60 -> "`"
    0x61 -> "a"
    0x62 -> "b"
    0x63 -> "c"
    0x64 -> "d"
    0x65 -> "e"
    0x66 -> "f"
    0x67 -> "g"
    0x68 -> "h"
    0x69 -> "i"
    0x6a -> "j"
    0x6b -> "k"
    0x6c -> "l"
    0x6d -> "m"
    0x6e -> "n"
    0x6f -> "o"
    0x70 -> "p"
    0x71 -> "q"
    0x72 -> "r"
    0x73 -> "s"
    0x74 -> "t"
    0x75 -> "u"
    0x76 -> "v"
    0x77 -> "w"
    0x78 -> "x"
    0x79 -> "y"
    0x7a -> "z"
    0x7b -> "{"
    0x7c -> "|"
    0x7d -> "}"
    0x7e -> "~"
    0x7f -> "\x7f"
    |}]
