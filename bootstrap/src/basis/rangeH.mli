(** Type-specific half-open ranges. *)

open RudimentsInt0
open RangeIntf

module I8 : SRangeH with type elm := I8.t with type l := U8.t
module I16 : SRangeH with type elm := I16.t with type l := U16.t
module I32 : SRangeH with type elm := I32.t with type l := U32.t
module I64 : SRangeH with type elm := I64.t with type l := U64.t
module Sint : SRangeH with type elm := sint with type l := uns
module I128 : SRangeH with type elm := I128.t with type l := U128.t
module I256 : SRangeH with type elm := I256.t with type l := U256.t
module I512 : SRangeH with type elm := I512.t with type l := U512.t

module U8 : SRangeH with type elm := U8.t with type l := U8.t
module U16 : SRangeH with type elm := U16.t with type l := U16.t
module U32 : SRangeH with type elm := U32.t with type l := U32.t
module U64 : SRangeH with type elm := U64.t with type l := U64.t
module Uns : SRangeH with type elm := uns with type l := uns
module U128 : SRangeH with type elm := U128.t with type l := U128.t
module U256 : SRangeH with type elm := U256.t with type l := U256.t
module U512 : SRangeH with type elm := U512.t with type l := U512.t
