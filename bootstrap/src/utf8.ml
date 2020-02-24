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
    let sigbits = 21 - lz in
    if sigbits < 8 then
      One (Byte.of_codepoint cp)
    else if sigbits < 12 then
      Two (
        Byte.(bit_or (kv 0b110_00000) (of_codepoint (Codepoint.bit_usr 6 cp))),
        Byte.(bit_or (kv 0b10_000000)
          (of_codepoint (Codepoint.(bit_and cp (kv 0x3f)))))
      )
    else if sigbits < 17 then
      Three (
        Byte.(bit_or (kv 0b1110_0000) (of_codepoint (Codepoint.bit_usr 12 cp))),
        Byte.(bit_or (kv 0b10_000000) (of_codepoint
            Codepoint.(bit_and (bit_usr 6 cp) (kv 0x3f)))),
        Byte.(bit_or (kv 0b10_000000)
          (of_codepoint Codepoint.(bit_and cp (kv 0x3f))))
      )
    else if sigbits < 22 then
      Four (
        Byte.(bit_or (kv 0b11110_000) (of_codepoint (Codepoint.bit_usr 18 cp))),
        Byte.(bit_or (kv 0b10_000000) (of_codepoint
            Codepoint.(bit_and (bit_usr 12 cp) (kv 0x3f)))),
        Byte.(bit_or (kv 0b10_000000) (of_codepoint
            Codepoint.(bit_and (bit_usr 6 cp) (kv 0x3f)))),
        Byte.(bit_or (kv 0b10_000000)
          (of_codepoint Codepoint.(bit_and cp (kv 0x3f))))
      )
    else not_reached ()

  let to_codepoint = function
    | One b0 -> Byte.to_codepoint b0
    | Two (b0, b1) -> Codepoint.(bit_or
        Byte.(to_codepoint (bit_sl 6 (bit_and b0 (kv 0x1f))))
        Byte.(to_codepoint (bit_and b1 (kv 0x3f))))
    | Three (b0, b1, b2) -> Codepoint.(bit_or (bit_or
        Codepoint.(bit_sl 12 Byte.(to_codepoint (bit_and b0 (kv 0xf))))
        Codepoint.(bit_sl 6 Byte.(to_codepoint (bit_and b1 (kv 0x3f)))))
      Byte.(to_codepoint (bit_and b2 (kv 0x3f))))
    | Four (b0, b1, b2, b3) -> Codepoint.(bit_or (bit_or (bit_or
        Codepoint.(bit_sl 18 Byte.(to_codepoint (bit_and b0 (kv 0x7))))
        Codepoint.(bit_sl 12 Byte.(to_codepoint (bit_and b1 (kv 0x3f)))))
      Codepoint.(bit_sl 6 Byte.(to_codepoint (bit_and b2 (kv 0x3f)))))
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
        | 0 -> begin
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
            | Some (b, t') -> fn t' (b :: bytes) (Usize.pred nrem)
          end
      end in
      match T.next t with
      | None -> None
      | Some (b, t') -> begin
          match Byte.(bit_clz (bit_not b)) with
          | 0 -> fn t' [b] 0
          | 2 -> fn t' [b] 1
          | 3 -> fn t' [b] 2
          | 4 -> fn t' [b] 3
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
            match Byte.(bit_clz (bit_not b)), bytes' with
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
  | One _ -> 1
  | Two _ -> 2
  | Three _ -> 3
  | Four _ -> 4

let to_string = function
  | One b0 -> Stdlib.String.init 1 (fun _ ->
    Stdlib.Char.chr (int_of_isize (Byte.to_isize b0))
  )
  | Two (b0, b1) -> Stdlib.String.init 2 (fun i ->
    Stdlib.Char.chr (int_of_isize (Byte.to_isize (
      match i with
      | 0 -> b0
      | 1 -> b1
      | _ -> not_reached ()
    ))))
  | Three (b0, b1, b2) -> Stdlib.String.init 3 (fun i ->
    Stdlib.Char.chr (int_of_isize (Byte.to_isize (
      match i with
      | 0 -> b0
      | 1 -> b1
      | 2 -> b2
      | _ -> not_reached ()
    ))))
  | Four (b0, b1, b2, b3) -> Stdlib.String.init 4 (fun i ->
    Stdlib.Char.chr (int_of_isize (Byte.to_isize (
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
  Format.fprintf ppf "'%s'" (escape t)

(******************************************************************************)
(* Begin tests. *)

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
      let space = if Usize.(i = 0) then "" else " " in
      let sep = if Usize.(succ i < length) then ";" else "" in
      printf "%s%a%s" space Byte.pp_x b sep
    );
    printf "], length=%a\n" Usize.pp length
  );

  [%expect{|
    codepoint=0x00003cu21, codepoint'=0x00003cu21, bytes=[0x3cu8], length=1
    codepoint=0x0000abu21, codepoint'=0x0000abu21, bytes=[0xc2u8; 0xabu8], length=2
    codepoint=0x002021u21, codepoint'=0x002021u21, bytes=[0xe2u8; 0x80u8; 0xa1u8], length=3
    codepoint=0x010197u21, codepoint'=0x010197u21, bytes=[0xf0u8; 0x90u8; 0x86u8; 0x97u8], length=4
    |}]

let%expect_test "pp,escape" =
  let open Format in

  let rec fn i = begin
    match i with
    | 0x80 -> ()
    | _ -> begin
        let utf8 = of_codepoint Codepoint.(of_usize i) in
        printf "%a -> %a\n" Usize.pp_x i pp utf8;
        fn (Usize.succ i)
      end
  end in
  fn 0;

  [%expect{|
    0x0000000000000000 -> '\0'
    0x0000000000000001 -> '\x01'
    0x0000000000000002 -> '\x02'
    0x0000000000000003 -> '\x03'
    0x0000000000000004 -> '\x04'
    0x0000000000000005 -> '\x05'
    0x0000000000000006 -> '\x06'
    0x0000000000000007 -> '\a'
    0x0000000000000008 -> '\b'
    0x0000000000000009 -> '\t'
    0x000000000000000a -> '\n'
    0x000000000000000b -> '\v'
    0x000000000000000c -> '\f'
    0x000000000000000d -> '\r'
    0x000000000000000e -> '\x0e'
    0x000000000000000f -> '\x0f'
    0x0000000000000010 -> '\x10'
    0x0000000000000011 -> '\x11'
    0x0000000000000012 -> '\x12'
    0x0000000000000013 -> '\x13'
    0x0000000000000014 -> '\x14'
    0x0000000000000015 -> '\x15'
    0x0000000000000016 -> '\x16'
    0x0000000000000017 -> '\x17'
    0x0000000000000018 -> '\x18'
    0x0000000000000019 -> '\x19'
    0x000000000000001a -> '\x1a'
    0x000000000000001b -> '\x1b'
    0x000000000000001c -> '\x1c'
    0x000000000000001d -> '\x1d'
    0x000000000000001e -> '\x1e'
    0x000000000000001f -> '\x1f'
    0x0000000000000020 -> ' '
    0x0000000000000021 -> '!'
    0x0000000000000022 -> '\"'
    0x0000000000000023 -> '#'
    0x0000000000000024 -> '$'
    0x0000000000000025 -> '%'
    0x0000000000000026 -> '&'
    0x0000000000000027 -> '''
    0x0000000000000028 -> '('
    0x0000000000000029 -> ')'
    0x000000000000002a -> '*'
    0x000000000000002b -> '+'
    0x000000000000002c -> ','
    0x000000000000002d -> '-'
    0x000000000000002e -> '.'
    0x000000000000002f -> '/'
    0x0000000000000030 -> '0'
    0x0000000000000031 -> '1'
    0x0000000000000032 -> '2'
    0x0000000000000033 -> '3'
    0x0000000000000034 -> '4'
    0x0000000000000035 -> '5'
    0x0000000000000036 -> '6'
    0x0000000000000037 -> '7'
    0x0000000000000038 -> '8'
    0x0000000000000039 -> '9'
    0x000000000000003a -> ':'
    0x000000000000003b -> ';'
    0x000000000000003c -> '<'
    0x000000000000003d -> '='
    0x000000000000003e -> '>'
    0x000000000000003f -> '?'
    0x0000000000000040 -> '@'
    0x0000000000000041 -> 'A'
    0x0000000000000042 -> 'B'
    0x0000000000000043 -> 'C'
    0x0000000000000044 -> 'D'
    0x0000000000000045 -> 'E'
    0x0000000000000046 -> 'F'
    0x0000000000000047 -> 'G'
    0x0000000000000048 -> 'H'
    0x0000000000000049 -> 'I'
    0x000000000000004a -> 'J'
    0x000000000000004b -> 'K'
    0x000000000000004c -> 'L'
    0x000000000000004d -> 'M'
    0x000000000000004e -> 'N'
    0x000000000000004f -> 'O'
    0x0000000000000050 -> 'P'
    0x0000000000000051 -> 'Q'
    0x0000000000000052 -> 'R'
    0x0000000000000053 -> 'S'
    0x0000000000000054 -> 'T'
    0x0000000000000055 -> 'U'
    0x0000000000000056 -> 'V'
    0x0000000000000057 -> 'W'
    0x0000000000000058 -> 'X'
    0x0000000000000059 -> 'Y'
    0x000000000000005a -> 'Z'
    0x000000000000005b -> '['
    0x000000000000005c -> '\\'
    0x000000000000005d -> ']'
    0x000000000000005e -> '^'
    0x000000000000005f -> '_'
    0x0000000000000060 -> '`'
    0x0000000000000061 -> 'a'
    0x0000000000000062 -> 'b'
    0x0000000000000063 -> 'c'
    0x0000000000000064 -> 'd'
    0x0000000000000065 -> 'e'
    0x0000000000000066 -> 'f'
    0x0000000000000067 -> 'g'
    0x0000000000000068 -> 'h'
    0x0000000000000069 -> 'i'
    0x000000000000006a -> 'j'
    0x000000000000006b -> 'k'
    0x000000000000006c -> 'l'
    0x000000000000006d -> 'm'
    0x000000000000006e -> 'n'
    0x000000000000006f -> 'o'
    0x0000000000000070 -> 'p'
    0x0000000000000071 -> 'q'
    0x0000000000000072 -> 'r'
    0x0000000000000073 -> 's'
    0x0000000000000074 -> 't'
    0x0000000000000075 -> 'u'
    0x0000000000000076 -> 'v'
    0x0000000000000077 -> 'w'
    0x0000000000000078 -> 'x'
    0x0000000000000079 -> 'y'
    0x000000000000007a -> 'z'
    0x000000000000007b -> '{'
    0x000000000000007c -> '|'
    0x000000000000007d -> '}'
    0x000000000000007e -> '~'
    0x000000000000007f -> '\x7f'
    |}]
