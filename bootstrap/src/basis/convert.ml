open RudimentsFunctions
open ConvertIntf

module Make_wI_wX (T : IntwIntf.SFI) (X : IntwIntf.SFI) : IX with type t := T.t with type x := X.t
= struct
  let trunc_of_x x =
    T.init ~f:(fun i -> X.get i x)

  let extend_to_x t =
    let ws = match T.is_negative t with
      | true -> 0xffff_ffff_ffff_ffffL
      | false -> 0L
    in
    X.init ~f:(fun i ->
      match Stdlib.((Int64.compare i T.word_length) < 0) with
      | true -> T.get i t
      | false -> ws
    )

  let narrow_of_x_opt x =
    let t = trunc_of_x x in
    match X.(x = (extend_to_x t)) with
    | false -> None
    | true -> Some t

  let narrow_of_x_hlt x =
    match narrow_of_x_opt x with
    | None -> halt "Lossy conversion"
    | Some t -> t
end

module Make_wI_wU (T : IntwIntf.SFI) (U : IntwIntf.SFU) : IU with type t := T.t with type u := U.t
= struct
  let trunc_of_u u =
    T.init ~f:(fun i -> U.get i u)

  let narrow_of_u_opt u =
    let lsb = U.(bit_and (pred (bit_sl ~shift:Int64.(pred (mul T.word_length 64L)) one)) u) in
    match U.(u = lsb) with
    | false -> None
    | true -> Some (T.init ~f:(fun i -> U.get i u))

  let widen_to_u_opt t =
    match T.(t < zero) with
    | true -> None
    | false ->
      Some (
        U.init ~f:(fun i ->
          match Stdlib.((Int64.compare i T.word_length) < 0) with
          | true -> T.get i t
          | false -> 0L
        )
      )

  let narrow_of_u_hlt u =
    match narrow_of_u_opt u with
    | None -> halt "Lossy conversion"
    | Some t -> t

  let widen_to_u_hlt t =
    match widen_to_u_opt t with
    | None -> halt "Lossy conversion"
    | Some u -> u
end

module Make_wU_wX (T : IntwIntf.SFU) (X : IntwIntf.SFI) : UX with type t := T.t with type x := X.t
= struct
  let trunc_of_x x =
    T.init ~f:(fun i -> X.get i x)

  let extend_to_x t =
    X.init ~f:(fun i ->
      match Stdlib.((Int64.compare i T.word_length) < 0) with
      | true -> T.get i t
      | false -> 0L
    )

  let narrow_of_x_opt x =
    let t = trunc_of_x x in
    match X.(x = (extend_to_x t)) with
    | false -> None
    | true -> Some (trunc_of_x x)

  let narrow_of_x_hlt x =
    match narrow_of_x_opt x with
    | None -> halt "Lossy conversion"
    | Some t -> t
end

module Make_wU_wU (T : IntwIntf.SFU) (U : IntwIntf.SFU) : UU with type t := T.t with type u := U.t
= struct
  let lsb_of_u u =
    U.(bit_and (pred (bit_sl ~shift:Stdlib.Int64.(mul T.word_length 64L) one)) u)

  let narrow_of_u_opt u =
    let lsb = lsb_of_u u in
    match U.(u = lsb) with
    | false -> None
    | true -> Some (T.init ~f:(fun i -> U.get i u))

  let narrow_of_u_hlt u =
    match narrow_of_u_opt u with
    | None -> halt "Lossy conversion"
    | Some t -> t

  let trunc_of_u u =
    T.init ~f:(fun i -> U.get i u)

  let extend_to_u t =
    U.init ~f:(fun i ->
      match Stdlib.((Int64.compare i T.word_length) < 0) with
      | true -> T.get i t
      | false -> 0L
    )
end

module Make_wU_wI (T : IntwIntf.SFU) (X : IntwIntf.SFU) : UI with type t := T.t with type x := X.t
= struct
  let bits_of_x x =
    T.init ~f:(fun i -> X.get i x)

  let bits_to_x t =
    X.init ~f:(fun i -> T.get i t)

  let like_of_x_opt x =
    match X.(x < zero) with
    | true -> None
    | false -> Some (bits_of_x x)

  let like_to_x_opt t =
    let x = bits_to_x t in
    match X.(x < zero) with
    | true -> None
    | false -> Some x

  let like_of_x_hlt x =
    match like_of_x_opt x with
    | None -> halt "Lossy conversion"
    | Some t -> t

  let like_to_x_hlt t =
    match like_to_x_opt t with
    | None -> halt "Lossy conversion"
    | Some x -> x
end

module Make_nbI (T : IntnbIntf.SI) : Nb with type t := T.t = struct
  let trunc_of_sint x =
    T.narrow_of_unsigned x

  let extend_to_sint t =
    T.widen t

  let narrow_of_sint_opt x =
    let t = trunc_of_sint x in
    let x' = extend_to_sint t in
    match Stdlib.(Int64.(compare x' x) = 0) with
    | false -> None
    | true -> Some t

  let narrow_of_sint_hlt x =
    match narrow_of_sint_opt x with
    | None -> halt "Lossy conversion"
    | Some t -> t

  let kv x =
    T.narrow_of_signed x

  let trunc_of_uns x =
    T.narrow_of_unsigned x

  let extend_to_uns t =
    extend_to_sint t

  let narrow_of_uns_opt x =
    let t = trunc_of_uns x in
    let x' = extend_to_uns t in
    match Stdlib.(Int64.(unsigned_compare x' x) = 0) with
    | false -> None
    | true -> Some t

  let narrow_of_uns_hlt x =
    match narrow_of_uns_opt x with
    | None -> halt "Lossy conversion"
    | Some t -> t
end

module Make_nbU (T : IntnbIntf.SU) : Nb with type t := T.t = struct
  let trunc_of_sint x =
    T.narrow_of_unsigned x

  let extend_to_sint t =
    T.widen t

  let narrow_of_sint_opt x =
    let t = trunc_of_sint x in
    let x' = extend_to_sint t in
    match Stdlib.(Int64.(compare x' x) = 0) with
    | false -> None
    | true -> Some t

  let narrow_of_sint_hlt x =
    match narrow_of_sint_opt x with
    | None -> halt "Lossy conversion"
    | Some t -> t

  let kv x =
    T.narrow_of_unsigned x

  let trunc_of_uns x =
    T.narrow_of_unsigned x

  let extend_to_uns t =
    extend_to_sint t

  let narrow_of_uns_opt x =
    let t = trunc_of_uns x in
    let x' = extend_to_uns t in
    match Stdlib.(Int64.(unsigned_compare x' x) = 0) with
    | false -> None
    | true -> Some t

  let narrow_of_uns_hlt x =
    match narrow_of_uns_opt x with
    | None -> halt "Lossy conversion"
    | Some t -> t
end

module Make_nbI_wX (T : IntnbIntf.SI) (X : IntwIntf.SFI) : IX with type t := T.t with type x := X.t
= struct
  module TI = struct
    include Make_nbI(T)
  end

  let trunc_of_x x =
    TI.trunc_of_sint (X.to_u64 x)

  let extend_to_x t =
    let ws = match T.is_negative t with
      | true -> 0xffff_ffff_ffff_ffffL
      | false -> 0L
    in
    X.init ~f:(fun i ->
      match i with
      | 0L -> TI.extend_to_sint t
      | _ -> ws
    )

  let narrow_of_x_opt x =
    let t = trunc_of_x x in
    match X.(x = (extend_to_x t)) with
    | false -> None
    | true -> Some t

  let narrow_of_x_hlt x =
    match narrow_of_x_opt x with
    | None -> halt "Lossy conversion"
    | Some t -> t
end

module Make_nbI_wU (T : IntnbIntf.SI) (U : IntwIntf.SFU) : IU with type t := T.t with type u := U.t
= struct
  module TI = struct
    include Make_nbI(T)
  end

  let trunc_of_u u =
    TI.trunc_of_uns (U.to_u64 u)

  let narrow_of_u_opt u =
    let lsb = U.(bit_and (pred (bit_sl ~shift:Int64.(pred T.(bit_pop (bit_not zero))) one)) u) in
    match U.(u = lsb) with
    | false -> None
    | true -> Some (trunc_of_u u)

  let widen_to_u_opt t =
    match T.(t < zero) with
    | true -> None
    | false -> Some (U.of_u64 (TI.extend_to_uns t))

  let narrow_of_u_hlt u =
    match narrow_of_u_opt u with
    | None -> halt "Lossy conversion"
    | Some t -> t

  let widen_to_u_hlt t =
    match widen_to_u_opt t with
    | None -> halt "Lossy conversion"
    | Some u -> u
end

module Make_nbU_wX (T : IntnbIntf.SU) (X : IntwIntf.SFI) : UX with type t := T.t with type x := X.t
= struct
  module TU = struct
    include Make_nbU(T)
  end

  let trunc_of_x x =
    TU.trunc_of_uns (X.to_u64 x)

  let extend_to_x t =
    X.init ~f:(fun i ->
      match i with
      | 0L -> TU.extend_to_uns t
      | _ -> 0L
    )

  let narrow_of_x_opt x =
    let t = trunc_of_x x in
    match X.(x = (extend_to_x t)) with
    | false -> None
    | true -> Some (trunc_of_x x)

  let narrow_of_x_hlt x =
    match narrow_of_x_opt x with
    | None -> halt "Lossy conversion"
    | Some t -> t
end

module Make_nbU_wU (T : IntnbIntf.SU) (U : IntwIntf.SFU) : UU with type t := T.t with type u := U.t
= struct
  module TU = struct
    include Make_nbU(T)
  end

  let lsb_of_u u =
    U.(bit_and (pred (bit_sl ~shift:T.(bit_pop max_value) one)) u)

  let narrow_of_u_opt u =
    let lsb = lsb_of_u u in
    match U.(u = lsb) with
    | false -> None
    | true -> Some (TU.trunc_of_uns (U.to_u64 u))

  let narrow_of_u_hlt u =
    match narrow_of_u_opt u with
    | None -> halt "Lossy conversion"
    | Some t -> t

  let trunc_of_u u =
    TU.trunc_of_uns (U.to_u64 u)

  let extend_to_u t =
    U.init ~f:(fun i ->
      match i with
      | 0L -> TU.extend_to_uns t
      | _ -> 0L
    )
end

module Make_nbI_nbX (T : IntnbIntf.SI) (X : IntnbIntf.SI) : IX with type t := T.t with type x := X.t
= struct
  let trunc_of_x x =
    T.narrow_of_signed (X.widen x)

  let extend_to_x t =
    X.narrow_of_signed (T.widen t)

  let narrow_of_x_opt x =
    let t = trunc_of_x x in
    let x' = (extend_to_x t) in
    match X.(x = x') with
    | false -> None
    | true -> Some t

  let narrow_of_x_hlt x =
    match narrow_of_x_opt x with
    | None -> halt "Lossy conversion"
    | Some t -> t
end

module Make_nbI_nbU (T : IntnbIntf.SI) (U : IntnbIntf.SU) : IU with type t := T.t with type u := U.t
= struct
  let trunc_of_u u =
    T.narrow_of_unsigned (U.widen u)

  let narrow_of_u_opt u =
    match Stdlib.(Int64.(compare (U.widen u) T.(widen max_value)) > 0) with
    | true -> None
    | false -> Some (trunc_of_u u)

  let widen_to_u_opt t =
    match T.(t < zero) with
    | true -> None
    | false -> Some (U.narrow_of_signed (T.widen t))

  let narrow_of_u_hlt u =
    match narrow_of_u_opt u with
    | None -> halt "Lossy conversion"
    | Some t -> t

  let widen_to_u_hlt t =
    match widen_to_u_opt t with
    | None -> halt "Lossy conversion"
    | Some u -> u
end

module Make_nbU_nbX (T : IntnbIntf.SU) (X : IntnbIntf.SI) : UX with type t := T.t with type x := X.t
= struct
  let trunc_of_x x =
    T.narrow_of_signed (X.widen x)

  let extend_to_x t =
    X.narrow_of_unsigned (T.widen t)

  let narrow_of_x_opt x =
    let t = trunc_of_x x in
    let x' = (extend_to_x t) in
    match X.(x = x') with
    | false -> None
    | true -> Some t

  let narrow_of_x_hlt x =
    match narrow_of_x_opt x with
    | None -> halt "Lossy conversion"
    | Some t -> t
end

module Make_nbU_nbU (T : IntnbIntf.SU) (U : IntnbIntf.SU) : UU with type t := T.t with type u := U.t
= struct
  let trunc_of_u u =
    T.narrow_of_unsigned (U.widen u)

  let extend_to_u t =
    U.narrow_of_unsigned (T.widen t)

  let narrow_of_u_opt u =
    let t = trunc_of_u u in
    match U.(u = (extend_to_u t)) with
    | false -> None
    | true -> Some t

  let narrow_of_u_hlt u =
    match narrow_of_u_opt u with
    | None -> halt "Lossy conversion"
    | Some t -> t
end

module Make_nbU_nbI (T : IntnbIntf.SU) (X : IntnbIntf.SU) : UI with type t := T.t with type x := X.t
= struct
  let bits_of_x x =
    T.narrow_of_signed (X.widen x)

  let bits_to_x t =
    X.narrow_of_unsigned (T.widen t)

  let like_of_x_opt x =
    match X.(x < zero) with
    | true -> None
    | false -> Some (bits_of_x x)

  let like_to_x_opt t =
    let x = bits_to_x t in
    match X.(x < zero) with
    | true -> None
    | false -> Some x

  let like_of_x_hlt x =
    match like_of_x_opt x with
    | None -> halt "Lossy conversion"
    | Some t -> t

  let like_to_x_hlt t =
    match like_to_x_opt t with
    | None -> halt "Lossy conversion"
    | Some x -> x
end
