open RudimentsFunctions

let pp_limit pp_a limit formatter =
  match limit with
  | RangeIntf.Excl past -> formatter |> Fmt.fmt "Excl " |> pp_a past
  | RangeIntf.Incl last -> formatter |> Fmt.fmt "Incl " |> pp_a last

let pp_length pp_a length formatter =
  match length with
  | RangeIntf.Overflow -> formatter |> Fmt.fmt "Overflow"
  | RangeIntf.Length a -> formatter |> Fmt.fmt "Length " |> pp_a a

module type TL = sig
  type t
  type l
  val to_l: t -> l
  val of_l: l -> t
end

(* The extreme code duplication between Make{,_w,_nb} requires higher-kinded modules to factor out.
*)

module Make_w (T : IntwIntf.SFCommon) (L : IntwIntf.SFU)
    (TL : TL with type t := T.t with type l := L.t)
= struct
  module U = struct
    type t =
      | RangeH of {base: T.t; past: T.t}
      | RangeF of {base: T.t; last: T.t}
    type elm = T.t

    let ( =:< ) base past =
      RangeH {base; past}

    let ( =:= ) base last =
      RangeF {base; last}

    let base = function
      | RangeH {base; _}
      | RangeF {base; _} -> base

    let limit = function
      | RangeH {past; _} -> RangeIntf.Excl past
      | RangeF {last; _} -> RangeIntf.Incl last

    let pp t formatter =
      formatter
      |> Fmt.fmt "["
      |> T.pp (base t)
      |> Fmt.fmt ".."
      |> (fun formatter ->
        match t with
        | RangeH {past; _} -> formatter |> T.pp past |> Fmt.fmt ")"
        | RangeF {last; _} -> formatter |> T.pp last |> Fmt.fmt "]"
      )

    module Cursor = struct
      module V = struct
        type t = T.t

        let cmp = T.cmp

        let hd range =
          base range

        let tl range =
          match range with
          | RangeH {past; _} -> past
          | RangeF {last; _} -> T.succ last

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
      match t with
      | RangeH {base; past} -> begin
          match T.cmp base past with
          | Cmp.Lt
          | Cmp.Eq -> RangeIntf.Length (L.( - ) (TL.to_l past) (TL.to_l base))
          | Cmp.Gt -> RangeIntf.Length (L.( - ) (TL.to_l base) (TL.to_l past))
        end
      | RangeF {base; last} -> begin
          match T.cmp base (T.succ last) with
          | Cmp.Lt -> RangeIntf.Length (L.succ (L.( - ) (TL.to_l last) (TL.to_l base)))
          | Cmp.Eq -> RangeIntf.Overflow
          | Cmp.Gt ->
            RangeIntf.Length (L.( + )
                (L.succ (L.( - ) L.max_value (TL.to_l base)))
                (L.succ (TL.to_l last)))
        end

    let length_hlt t =
      match length t with
      | Overflow -> halt "length overflow"
      | Length l -> TL.of_l l
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
    match t with
    | RangeH {base; past} -> begin
        match (T.cmp base past), (T.cmp base elm), (T.cmp elm past) with
        | Cmp.Lt, Cmp.Lt, Cmp.Lt (* base < elm < past *)
        | Cmp.Lt, Cmp.Eq, Cmp.Lt (* base = elm < past *)
        | Cmp.Gt, Cmp.Gt, Cmp.Lt (* elm < past < base *)
        | Cmp.Gt, Cmp.Eq, Cmp.Gt (* past < base = elm *)
        | Cmp.Gt, Cmp.Lt, Cmp.Gt (* past < base < elm *)
          -> true
        | _ -> false
      end
    | RangeF {base; last} -> begin
        match (T.cmp base last), (T.cmp base elm), (T.cmp elm last) with
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
end

module Make (T : IntnbIntf.S) (L : IntnbIntf.SU)
    (TL : TL with type t := T.t with type l := L.t)
= struct
  module U = struct
    type t =
      | RangeH of {base: T.t; past: T.t}
      | RangeF of {base: T.t; last: T.t}
    type elm = T.t

    let ( =:< ) base past =
      RangeH {base; past}

    let ( =:= ) base last =
      RangeF {base; last}

    let base = function
      | RangeH {base; _}
      | RangeF {base; _} -> base

    let limit = function
      | RangeH {past; _} -> RangeIntf.Excl past
      | RangeF {last; _} -> RangeIntf.Incl last

    let pp t formatter =
      formatter
      |> Fmt.fmt "["
      |> T.pp (base t)
      |> Fmt.fmt ".."
      |> (fun formatter ->
        match t with
        | RangeH {past; _} -> formatter |> T.pp past |> Fmt.fmt ")"
        | RangeF {last; _} -> formatter |> T.pp last |> Fmt.fmt "]"
      )

    module Cursor = struct
      module V = struct
        type t = T.t

        let cmp = T.cmp

        let hd range =
          base range

        let tl range =
          match range with
          | RangeH {past; _} -> past
          | RangeF {last; _} -> T.succ last

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
      match t with
      | RangeH {base; past} -> begin
          match T.cmp base past with
          | Cmp.Lt
          | Cmp.Eq -> RangeIntf.Length (L.( - ) (TL.to_l past) (TL.to_l base))
          | Cmp.Gt -> RangeIntf.Length (L.( - ) (TL.to_l base) (TL.to_l past))
        end
      | RangeF {base; last} -> begin
          match T.cmp base (T.succ last) with
          | Cmp.Lt -> RangeIntf.Length (L.succ (L.( - ) (TL.to_l last) (TL.to_l base)))
          | Cmp.Eq -> RangeIntf.Overflow
          | Cmp.Gt ->
            RangeIntf.Length (L.( + )
                (L.succ (L.( - ) L.max_value (TL.to_l base)))
                (L.succ (TL.to_l last)))
        end

    let length_hlt t =
      match length t with
      | Overflow -> halt "length overflow"
      | Length l -> TL.of_l l
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
    match t with
    | RangeH {base; past} -> begin
        match (T.cmp base past), (T.cmp base elm), (T.cmp elm past) with
        | Cmp.Lt, Cmp.Lt, Cmp.Lt (* base < elm < past *)
        | Cmp.Lt, Cmp.Eq, Cmp.Lt (* base = elm < past *)
        | Cmp.Gt, Cmp.Gt, Cmp.Lt (* elm < past < base *)
        | Cmp.Gt, Cmp.Eq, Cmp.Gt (* past < base = elm *)
        | Cmp.Gt, Cmp.Lt, Cmp.Gt (* past < base < elm *)
          -> true
        | _ -> false
      end
    | RangeF {base; last} -> begin
        match (T.cmp base last), (T.cmp base elm), (T.cmp elm last) with
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
end

module Make_nb (T : IntnbIntf.S) (L : IntnbIntf.SU)
    (TL : TL with type t := T.t with type l := L.t)
= struct
  module U = struct
    type t =
      | RangeH of {base: T.t; past: T.t}
      | RangeF of {base: T.t; last: T.t}
    type elm = T.t

    let ( =:< ) base past =
      RangeH {base; past}

    let ( =:= ) base last =
      RangeF {base; last}

    let base = function
      | RangeH {base; _}
      | RangeF {base; _} -> base

    let limit = function
      | RangeH {past; _} -> RangeIntf.Excl past
      | RangeF {last; _} -> RangeIntf.Incl last

    let pp t formatter =
      formatter
      |> Fmt.fmt "["
      |> T.pp (base t)
      |> Fmt.fmt ".."
      |> (fun formatter ->
        match t with
        | RangeH {past; _} -> formatter |> T.pp past |> Fmt.fmt ")"
        | RangeF {last; _} -> formatter |> T.pp last |> Fmt.fmt "]"
      )

    module Cursor = struct
      module V = struct
        type t = T.t

        let cmp = T.cmp

        let hd range =
          base range

        let tl range =
          match range with
          | RangeH {past; _} -> past
          | RangeF {last; _} -> T.succ last

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
      match t with
      | RangeH {base; past} -> begin
          match T.cmp base past with
          | Cmp.Lt
          | Cmp.Eq -> RangeIntf.Length (L.( - ) (TL.to_l past) (TL.to_l base))
          | Cmp.Gt -> RangeIntf.Length (L.( - ) (TL.to_l base) (TL.to_l past))
        end
      | RangeF {base; last} -> begin
          match T.cmp base (T.succ last) with
          | Cmp.Lt -> RangeIntf.Length (L.succ (L.( - ) (TL.to_l last) (TL.to_l base)))
          | Cmp.Eq -> RangeIntf.Overflow
          | Cmp.Gt ->
            RangeIntf.Length (L.( + )
                (L.succ (L.( - ) L.max_value (TL.to_l base)))
                (L.succ (TL.to_l last)))
        end

    let length_hlt t =
      match length t with
      | Overflow -> halt "length overflow"
      | Length l -> TL.of_l l
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
    match t with
    | RangeH {base; past} -> begin
        match (T.cmp base past), (T.cmp base elm), (T.cmp elm past) with
        | Cmp.Lt, Cmp.Lt, Cmp.Lt (* base < elm < past *)
        | Cmp.Lt, Cmp.Eq, Cmp.Lt (* base = elm < past *)
        | Cmp.Gt, Cmp.Gt, Cmp.Lt (* elm < past < base *)
        | Cmp.Gt, Cmp.Eq, Cmp.Gt (* past < base = elm *)
        | Cmp.Gt, Cmp.Lt, Cmp.Gt (* past < base < elm *)
          -> true
        | _ -> false
      end
    | RangeF {base; last} -> begin
        match (T.cmp base last), (T.cmp base elm), (T.cmp elm last) with
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
end

module I512 = Make_w(I512)(U512)
  (struct let to_l = U512.bits_of_i512 let of_l = U512.bits_to_i512 end)
module I256 = Make_w(I256)(U256)
  (struct let to_l = U256.bits_of_i256 let of_l = U256.bits_to_i256 end)
module I128 = Make_w(I128)(U128)
  (struct let to_l = U128.bits_of_i128 let of_l = U128.bits_to_i128 end)
module I64 = Make(I64)(U64)(struct let to_l = U64.bits_of_sint let of_l = U64.bits_to_sint end)
module Sint = I64
module I32 = Make_nb(I32)(U32)(struct let to_l = U32.bits_of_i32 let of_l = U32.bits_to_i32 end)
module I16 = Make_nb(I16)(U16)(struct let to_l = U16.bits_of_i16 let of_l = U16.bits_to_i16 end)
module I8 = Make_nb(I8)(U8)(struct let to_l = U8.bits_of_i8 let of_l = U8.bits_to_i8 end)

module U512 = Make_w(U512)(U512)(struct let to_l t = t let of_l t = t end)
module U256 = Make_w(U256)(U256)(struct let to_l t = t let of_l t = t end)
module U128 = Make_w(U128)(U128)(struct let to_l t = t let of_l t = t end)
module U64 = Make(U64)(U64)(struct let to_l t = t let of_l t = t end)
module Uns = U64
module U32 = Make_nb(U32)(U32)(struct let to_l t = t let of_l t = t end)
module U16 = Make_nb(U16)(U16)(struct let to_l t = t let of_l t = t end)
module U8 = Make_nb(U8)(U8)(struct let to_l t = t let of_l t = t end)

type range = Uns.t
