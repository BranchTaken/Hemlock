module type ToL = sig
  type t
  type l
  val to_l: t -> l
end

(* The extreme code duplication between Make{,_w,_nb} requires higher-kinded modules to factor out.
*)

module Make_w (T : IntwIntf.SFCommon) (L : IntwIntf.SFU)
    (TL : ToL with type t := T.t with type l := L.t)
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

    let length t =
      match T.cmp t.base (T.succ t.last) with
      | Cmp.Lt -> RangeIntf.Length (L.succ (L.( - ) (TL.to_l t.last) (TL.to_l t.base)))
      | Cmp.Eq -> RangeIntf.Overflow
      | Cmp.Gt ->
        RangeIntf.Length (L.( + )
            (L.succ (L.( - ) L.max_value (TL.to_l t.base)))
            (L.succ (TL.to_l t.last)))
  end
  include U
  include Container.MakeMonoIter(U)

  module V = struct
    module W = struct
      include U

      let length t =
        match U.length t with
        | RangeIntf.Overflow -> Uns.max_value
        | RangeIntf.Length l -> begin
            match L.to_uns_opt l with
            | None -> Uns.max_value
            | Some u -> u
          end
    end

    include Container.MakeMonoLength(W)
    include Container.MakeMonoArray(W)
  end
  let is_empty = V.is_empty
  let to_array = V.to_array

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

module Make (T : IntnbIntf.S) (L : IntnbIntf.SU) (TL : ToL with type t := T.t with type l := L.t)
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

    let length t =
      match T.cmp t.base (T.succ t.last) with
      | Cmp.Lt -> RangeIntf.Length (L.succ (L.( - ) (TL.to_l t.last) (TL.to_l t.base)))
      | Cmp.Eq -> RangeIntf.Overflow
      | Cmp.Gt ->
        RangeIntf.Length (L.( + )
            (L.succ (L.( - ) L.max_value (TL.to_l t.base)))
            (L.succ (TL.to_l t.last)))
  end
  include U
  include Container.MakeMonoIter(U)

  module V = struct
    module W = struct
      include U

      let length t =
        match U.length t with
        | RangeIntf.Overflow -> Uns.max_value
        | RangeIntf.Length l -> L.widen l
    end

    include Container.MakeMonoLength(W)
    include Container.MakeMonoArray(W)
  end
  let is_empty = V.is_empty
  let to_array = V.to_array

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

module Make_nb (T : IntnbIntf.S) (L : IntnbIntf.SU) (TL : ToL with type t := T.t with type l := L.t)
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

    let length t =
      match T.cmp t.base (T.succ t.last) with
      | Cmp.Lt -> RangeIntf.Length (L.succ (L.( - ) (TL.to_l t.last) (TL.to_l t.base)))
      | Cmp.Eq -> RangeIntf.Overflow
      | Cmp.Gt ->
        RangeIntf.Length (L.( + )
            (L.succ (L.( - ) L.max_value (TL.to_l t.base)))
            (L.succ (TL.to_l t.last)))
  end
  include U
  include Container.MakeMonoIter(U)

  module V = struct
    module W = struct
      include U

      let length t =
        match U.length t with
        | RangeIntf.Overflow -> Uns.succ L.(widen max_value)
        | RangeIntf.Length l -> L.widen l
    end

    include Container.MakeMonoLength(W)
    include Container.MakeMonoArray(W)
  end
  let is_empty = V.is_empty
  let to_array = V.to_array

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

module I512 = Make_w(I512)(U512)(struct let to_l = U512.bits_of_i512 end)
module I256 = Make_w(I256)(U256)(struct let to_l = U256.bits_of_i256 end)
module I128 = Make_w(I128)(U128)(struct let to_l = U128.bits_of_i128 end)
module I64 = Make(I64)(U64)(struct let to_l = U64.bits_of_sint end)
module Sint = I64
module I32 = Make_nb(I32)(U32)(struct let to_l = U32.bits_of_i32 end)
module I16 = Make_nb(I16)(U16)(struct let to_l = U16.bits_of_i16 end)
module I8 = Make_nb(I8)(U8)(struct let to_l = U8.bits_of_i8 end)

module U512 = Make_w(U512)(U512)(struct let to_l t = t end)
module U256 = Make_w(U256)(U256)(struct let to_l t = t end)
module U128 = Make_w(U128)(U128)(struct let to_l t = t end)
module U64 = Make(U64)(U64)(struct let to_l t = t end)
module Uns = U64
module U32 = Make_nb(U32)(U32)(struct let to_l t = t end)
module U16 = Make_nb(U16)(U16)(struct let to_l t = t end)
module U8 = Make_nb(U8)(U8)(struct let to_l t = t end)
