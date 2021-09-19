(* Partial Rudiments. *)
open RudimentsInt
open RudimentsFunctions

module T = struct
  type t = uns
  let bit_length = 16
end
include T
include Intnb.MakeU(T)

let to_sint t =
  Uns.to_sint t

let of_sint x =
  narrow_of_signed x

let of_sint_opt x =
  let t = of_sint x in
  let x' = to_sint t in
  match Sint.(x' = x) with
  | false -> None
  | true -> Some t

let of_sint_hlt x =
  match of_sint_opt x with
  | None -> halt "Lossy conversion"
  | Some t -> t

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
