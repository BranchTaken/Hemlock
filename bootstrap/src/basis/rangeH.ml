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
      past: T.t;
    }
    type elm = T.t

    let ( =:< ) base past =
      {base; past}

    let base t =
      t.base

    let past t =
      t.past

    module Cursor = struct
      module V = struct
        type t = T.t

        let cmp = T.cmp

        let hd range =
          base range

        let tl range =
          past range

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
    match T.cmp t.base t.past with
    | Cmp.Lt
    | Cmp.Eq -> L.( - ) (TL.to_l t.past) (TL.to_l t.base)
    | Cmp.Gt -> L.( - ) (TL.to_l t.base) (TL.to_l t.past)

  let mem elm t =
    match (T.cmp t.base t.past), (T.cmp t.base elm), (T.cmp elm t.past) with
    | Cmp.Lt, Cmp.Lt, Cmp.Lt (* base < elm < past *)
    | Cmp.Lt, Cmp.Eq, Cmp.Lt (* base = elm < past *)
    | Cmp.Gt, Cmp.Gt, Cmp.Lt (* elm < past < base *)
    | Cmp.Gt, Cmp.Eq, Cmp.Gt (* past < base = elm *)
    | Cmp.Gt, Cmp.Lt, Cmp.Gt (* past < base < elm *)
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
