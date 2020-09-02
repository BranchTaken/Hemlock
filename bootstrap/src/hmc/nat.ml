open Basis
open Basis.Rudiments

module T = struct
  type t = u64 array
  let min_word_length = 0
  let max_word_length = Uns.(2 ** 26) (* 2**32 bits. *)
  let init = Array.init
  let word_length = Array.length
  let get = Array.get
end
include T
include Intw.MakeVU(T)

let k_0 = of_uns 0x0
let k_1 = of_uns 0x1
let k_2 = of_uns 0x2
let k_3 = of_uns 0x3
let k_4 = of_uns 0x4
let k_5 = of_uns 0x5
let k_6 = of_uns 0x6
let k_7 = of_uns 0x7
let k_8 = of_uns 0x8
let k_9 = of_uns 0x9
let k_a = of_uns 0xa
let k_b = of_uns 0xb
let k_c = of_uns 0xc
let k_d = of_uns 0xd
let k_e = of_uns 0xe
let k_f = of_uns 0xf
let k_g = of_uns 0x10

let max_u8 = (bit_sl ~shift:8 k_1) - k_1
let max_abs_i8 = bit_sl ~shift:7 k_1
let max_i8 = max_abs_i8 - k_1

let max_u16 = (bit_sl ~shift:16 k_1) - k_1
let max_abs_i16 = bit_sl ~shift:15 k_1
let max_i16 = max_abs_i16 - k_1

let max_u32 = (bit_sl ~shift:32 k_1) - k_1
let max_abs_i32 = bit_sl ~shift:31 k_1
let max_i32 = max_abs_i32 - k_1

let max_uns = (bit_sl ~shift:63 k_1) - k_1
let max_abs_int = bit_sl ~shift:62 k_1
let max_int = max_abs_int - k_1

let max_u64 = (bit_sl ~shift:64 k_1) - k_1
let max_abs_i64 = bit_sl ~shift:63 k_1
let max_i64 = max_abs_i64 - k_1

let max_u128 = (bit_sl ~shift:128 k_1) - k_1
let max_abs_i128 = bit_sl ~shift:127 k_1
let max_i128 = max_abs_i128 - k_1

let max_u256 = (bit_sl ~shift:256 k_1) - k_1
let max_abs_i256 = bit_sl ~shift:255 k_1
let max_i256 = max_abs_i256 - k_1

let max_u512 = (bit_sl ~shift:512 k_1) - k_1
let max_abs_i512 = bit_sl ~shift:511 k_1
let max_i512 = max_abs_i512 - k_1

let to_u8_opt t =
  match t <= max_u8 with
  | true -> Some (U8.of_uns (to_uns t))
  | false -> None

let to_i8_opt t =
  match t <= max_i8, t = max_abs_i8 with
  | true, _ -> Some (I8.of_uns (to_uns t))
  | false, true -> Some I8.min_value
  | false, false -> None

let to_u16_opt t =
  match t <= max_u16 with
  | true -> Some (U16.of_uns (to_uns t))
  | false -> None

let to_i16_opt t =
  match t <= max_i16, t = max_abs_i16 with
  | true, _ -> Some (I16.of_uns (to_uns t))
  | false, true -> Some I16.min_value
  | false, false -> None

let to_u32_opt t =
  match t <= max_u32 with
  | true -> Some (U32.of_uns (to_uns t))
  | false -> None

let to_i32_opt t =
  match t <= max_i32, t = max_abs_i32 with
  | true, _ -> Some (I32.of_uns (to_uns t))
  | false, true -> Some I32.min_value
  | false, false -> None

let to_uns_opt t =
  match t <= max_uns with
  | true -> Some (to_uns t)
  | false -> None

let to_int_opt t =
  match t <= max_int, t = max_abs_int with
  | true, _ -> Some (Uns.to_sint (to_uns t))
  | false, true -> Some Sint.min_value
  | false, false -> None

let to_u64_opt t =
  match t <= max_u64 with
  | true -> Some (to_u64 t)
  | false -> None

let to_i64_opt t =
  match t <= max_i64, t = max_abs_i64 with
  | true, _ -> Some (U64.to_i64 (to_u64 t))
  | false, true -> Some I64.min_value
  | false, false -> None

let to_u128_opt t =
  match t <= max_u128 with
  | true -> Some (U128.init ~f:(fun i -> get i t))
  | false -> None

let to_i128_opt t =
  match t <= max_i128, t = max_abs_i128 with
  | true, _ -> Some (I128.init ~f:(fun i -> get i t))
  | false, true -> Some I128.min_value
  | false, false -> None

let to_u256_opt t =
  match t <= max_u256 with
  | true -> Some (U256.init ~f:(fun i -> get i t))
  | false -> None

let to_i256_opt t =
  match t <= max_i256, t = max_abs_i256 with
  | true, _ -> Some (I256.init ~f:(fun i -> get i t))
  | false, true -> Some I256.min_value
  | false, false -> None

let to_u512_opt t =
  match t <= max_u512 with
  | true -> Some (U512.init ~f:(fun i -> get i t))
  | false -> None

let to_i512_opt t =
  match t <= max_i512, t = max_abs_i512 with
  | true, _ -> Some (I512.init ~f:(fun i -> get i t))
  | false, true -> Some I512.min_value
  | false, false -> None

let to_u8_hlt t =
  match to_u8_opt t with
  | Some x -> x
  | None -> halt "Lossy conversion"

let to_i8_hlt t =
  match to_i8_opt t with
  | Some x -> x
  | None -> halt "Lossy conversion"

let to_u16_hlt t =
  match to_u16_opt t with
  | Some x -> x
  | None -> halt "Lossy conversion"

let to_i16_hlt t =
  match to_i16_opt t with
  | Some x -> x
  | None -> halt "Lossy conversion"

let to_u32_hlt t =
  match to_u32_opt t with
  | Some x -> x
  | None -> halt "Lossy conversion"

let to_i32_hlt t =
  match to_i32_opt t with
  | Some x -> x
  | None -> halt "Lossy conversion"

let to_uns_hlt t =
  match to_uns_opt t with
  | Some x -> x
  | None -> halt "Lossy conversion"

let to_int_hlt t =
  match to_int_opt t with
  | Some x -> x
  | None -> halt "Lossy conversion"

let to_u64_hlt t =
  match to_u64_opt t with
  | Some x -> x
  | None -> halt "Lossy conversion"

let to_i64_hlt t =
  match to_i64_opt t with
  | Some x -> x
  | None -> halt "Lossy conversion"

let to_u128_hlt t =
  match to_u128_opt t with
  | Some x -> x
  | None -> halt "Lossy conversion"

let to_i128_hlt t =
  match to_i128_opt t with
  | Some x -> x
  | None -> halt "Lossy conversion"

let to_u256_hlt t =
  match to_u256_opt t with
  | Some x -> x
  | None -> halt "Lossy conversion"

let to_i256_hlt t =
  match to_i256_opt t with
  | Some x -> x
  | None -> halt "Lossy conversion"

let to_u512_hlt t =
  match to_u512_opt t with
  | Some x -> x
  | None -> halt "Lossy conversion"

let to_i512_hlt t =
  match to_i512_opt t with
  | Some x -> x
  | None -> halt "Lossy conversion"
