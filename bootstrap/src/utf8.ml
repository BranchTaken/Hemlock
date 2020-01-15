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

(*******************************************************************************
 * Begin tests.
 *)

let%expect_test "utf8" =
  let open Printf in
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
    printf "codepoint=0x%x, codepoint'=0x%x, bytes=["
      (Codepoint.to_int codepoint) (Codepoint.to_int codepoint');
    List.iteri bytes ~f:(fun i b ->
      let space = if Uint.(i = (kv 0)) then "" else " " in
      let sep = if Uint.(succ i < length) then ";" else "" in
      printf "%s0x%x%s" space (Byte.to_int b) sep
    );
    printf "], length=%u\n" (Uint.to_int length)
  );

  [%expect{|
    codepoint=0x3c, codepoint'=0x3c, bytes=[0x3c], length=1
    codepoint=0xab, codepoint'=0xab, bytes=[0xc2; 0xab], length=2
    codepoint=0x2021, codepoint'=0x2021, bytes=[0xe2; 0x80; 0xa1], length=3
    codepoint=0x10197, codepoint'=0x10197, bytes=[0xf0; 0x90; 0x86; 0x97], length=4
    |}]
