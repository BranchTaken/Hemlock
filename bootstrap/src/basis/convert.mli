open ConvertIntf

module Make_wI_wX (T : IntwIntf.SFI) (X : IntwIntf.SFI) : IX with type t := T.t with type x := X.t
module Make_wI_wU (T : IntwIntf.SFI) (U : IntwIntf.SFU) : IU with type t := T.t with type u := U.t
module Make_wU_wX (T : IntwIntf.SFU) (X : IntwIntf.SFI) : UX with type t := T.t with type x := X.t
module Make_wU_wU (T : IntwIntf.SFU) (U : IntwIntf.SFU) : UU with type t := T.t with type u := U.t
module Make_wU_wI (T : IntwIntf.SFU) (X : IntwIntf.SFU) : UI with type t := T.t with type x := X.t

module Make_nbI (T : IntnbIntf.SI) : Nb with type t := T.t
module Make_nbU (T : IntnbIntf.SU) : Nb with type t := T.t

module Make_nbI_wX (T : IntnbIntf.SI) (X : IntwIntf.SFI) : IX with type t := T.t with type x := X.t
module Make_nbI_wU (T : IntnbIntf.SI) (U : IntwIntf.SFU) : IU with type t := T.t with type u := U.t
module Make_nbU_wX (T : IntnbIntf.SU) (X : IntwIntf.SFI) : UX with type t := T.t with type x := X.t
module Make_nbU_wU (T : IntnbIntf.SU) (U : IntwIntf.SFU) : UU with type t := T.t with type u := U.t

module Make_nbI_nbX (T : IntnbIntf.SI) (X : IntnbIntf.SI) : IX with type t := T.t with type x := X.t
module Make_nbI_nbU (T : IntnbIntf.SI) (U : IntnbIntf.SU) : IU with type t := T.t with type u := U.t
module Make_nbU_nbX (T : IntnbIntf.SU) (X : IntnbIntf.SI) : UX with type t := T.t with type x := X.t
module Make_nbU_nbU (T : IntnbIntf.SU) (U : IntnbIntf.SU) : UU with type t := T.t with type u := U.t
module Make_nbU_nbI (T : IntnbIntf.SU) (X : IntnbIntf.SU) : UI with type t := T.t with type x := X.t
