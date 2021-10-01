module type ToL = sig
  type t
  type l
  val to_l: t -> l
end

module Make (T : IntnbIntf.S) (L : IntnbIntf.S) (TL : ToL with type t := T.t with type l := L.t)
= struct
  module U = struct
    type t = {
      base: T.t;
      last: T.t;
    }
    type elm = T.t

    let ( =:= ) base last =
      {base; last}

    let base t =
      t.base

    let last t =
      t.last

    module Cursor = struct
      module V = struct
        type t = T.t

        let cmp = T.cmp

        let hd range =
          base range

        let tl range =
          T.succ (last range)

        let pred = T.pred
        let succ = T.succ
        let lget = T.pred
        let rget cursor =
          cursor

        let prev cursor =
          (lget cursor), (pred cursor)

        let next cursor =
          (rget cursor), (succ cursor)
      end
      include V
      include Cmpable.Make(V)
    end
  end
  include U
  include ContainerCommon.MakeMonoFold(U)

  let length t =
    match T.cmp t.base (T.succ t.last) with
    | Cmp.Lt -> RangeIntf.Length (L.succ (L.( - ) (TL.to_l t.last) (TL.to_l t.base)))
    | Cmp.Eq -> RangeIntf.Overflow
    | Cmp.Gt ->
      RangeIntf.Length (L.( + )
          (L.succ (L.( - ) L.max_value (TL.to_l t.base)))
          (L.succ (TL.to_l t.last)))

  let mem elm t =
    match (T.cmp t.base t.last), (T.cmp t.base elm), (T.cmp elm t.last) with
    | Cmp.Lt, Cmp.Lt, Cmp.Lt (* base < elm < last *)
    | Cmp.Lt, Cmp.Lt, Cmp.Eq (* base < elm = last *)
    | Cmp.Lt, Cmp.Eq, Cmp.Lt (* base = elm < last *)
    | Cmp.Eq, Cmp.Eq, Cmp.Eq (* base = elm = last *)
    | Cmp.Gt, Cmp.Gt, Cmp.Lt (* elm < last < base *)
    | Cmp.Gt, Cmp.Gt, Cmp.Eq (* elm = last < base *)
    | Cmp.Gt, Cmp.Eq, Cmp.Gt (* last < base = elm *)
    | Cmp.Gt, Cmp.Lt, Cmp.Gt (* last < base < elm *)
      -> true
    | _ -> false
end

module I8 = Make(I8)(U8)(struct let to_l = U8.bits_of_i8 end)
module I16 = Make(I16)(U16)(struct let to_l = U16.bits_of_i16 end)
module I32 = Make(I32)(U32)(struct let to_l = U32.bits_of_i32 end)
module I64 = Make(I64)(U64)(struct let to_l = U64.bits_of_sint end)
module Sint = I64
module I128 = Make(I128)(U128)(struct let to_l = U128.bits_of_i128 end)
module I256 = Make(I256)(U256)(struct let to_l = U256.bits_of_i256 end)
module I512 = Make(I512)(U512)(struct let to_l = U512.bits_of_i512 end)

module U8 = Make(U8)(U8)(struct let to_l t = t end)
module U16 = Make(U16)(U16)(struct let to_l t = t end)
module U32 = Make(U32)(U32)(struct let to_l t = t end)
module U64 = Make(U64)(U64)(struct let to_l t = t end)
module Uns = U64
module U128 = Make(U128)(U128)(struct let to_l t = t end)
module U256 = Make(U256)(U256)(struct let to_l t = t end)
module U512 = Make(U512)(U512)(struct let to_l t = t end)
