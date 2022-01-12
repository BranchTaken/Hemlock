open ConvertIntf

(* Nat/Zint are taken as parameters to avoid cyclic module dependencies. *)
module Make_wI_wZ (T : IntwIntf.SFI) (Z : IntwIntf.SVI) : IX with type t := T.t with type x := Z.t
module Make_wI_wN (T : IntwIntf.SFI) (N : IntwIntf.SVU) : IU with type t := T.t with type u := N.t
module Make_wU_wZ (T : IntwIntf.SFU) (Z : IntwIntf.SVI) : UX with type t := T.t with type x := Z.t
module Make_wU_wN (T : IntwIntf.SFU) (N : IntwIntf.SVU) : UU with type t := T.t with type u := N.t

module Make_wI_wX (T : IntwIntf.SFI) (X : IntwIntf.SFI) : IX with type t := T.t with type x := X.t
module Make_wI_wU (T : IntwIntf.SFI) (U : IntwIntf.SFU) : IU with type t := T.t with type u := U.t
module Make_wU_wX (T : IntwIntf.SFU) (X : IntwIntf.SFI) : UX with type t := T.t with type x := X.t
module Make_wU_wU (T : IntwIntf.SFU) (U : IntwIntf.SFU) : UU with type t := T.t with type u := U.t
module Make_wU_wI (T : IntwIntf.SFU) (X : IntwIntf.SFU) : UI with type t := T.t with type x := X.t

module Make_nbI (T : IntnbIntf.SI) : Nb with type t := T.t
module Make_nbU (T : IntnbIntf.SU) : Nb with type t := T.t

module Make_nbI_wZ (T : IntnbIntf.SI) (Z : IntwIntf.SVI) : IX with type t := T.t with type x := Z.t
module Make_nbI_wN (T : IntnbIntf.SI) (N : IntwIntf.SVU) : IU with type t := T.t with type u := N.t
module Make_nbU_wZ (T : IntnbIntf.SU) (Z : IntwIntf.SVI) : UX with type t := T.t with type x := Z.t
module Make_nbU_wN (T : IntnbIntf.SU) (N : IntwIntf.SVU) : UU with type t := T.t with type u := N.t

module Make_nbI_wX (T : IntnbIntf.SI) (X : IntwIntf.SFI) : IX with type t := T.t with type x := X.t
module Make_nbI_wU (T : IntnbIntf.SI) (U : IntwIntf.SFU) : IU with type t := T.t with type u := U.t
module Make_nbU_wX (T : IntnbIntf.SU) (X : IntwIntf.SFI) : UX with type t := T.t with type x := X.t
module Make_nbU_wU (T : IntnbIntf.SU) (U : IntwIntf.SFU) : UU with type t := T.t with type u := U.t

module Make_nbI_nbX (T : IntnbIntf.SI) (X : IntnbIntf.SI) : IX with type t := T.t with type x := X.t
module Make_nbI_nbU (T : IntnbIntf.SI) (U : IntnbIntf.SU) : IU with type t := T.t with type u := U.t
module Make_nbU_nbX (T : IntnbIntf.SU) (X : IntnbIntf.SI) : UX with type t := T.t with type x := X.t
module Make_nbU_nbU (T : IntnbIntf.SU) (U : IntnbIntf.SU) : UU with type t := T.t with type u := U.t
module Make_nbU_nbI (T : IntnbIntf.SU) (X : IntnbIntf.SU) : UI with type t := T.t with type x := X.t
