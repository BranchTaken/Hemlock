(** Type-specific full-open ranges. *)

open RudimentsInt0
open RangeIntf

module I8 : SRangeF with type elm := I8.t with type l := U8.t l
module I16 : SRangeF with type elm := I16.t with type l := U16.t l
module I32 : SRangeF with type elm := I32.t with type l := U32.t l
module I64 : SRangeF with type elm := I64.t with type l := U64.t l
module Sint : SRangeF with type elm := sint with type l := uns l
module I128 : SRangeF with type elm := I128.t with type l := U128.t l
module I256 : SRangeF with type elm := I256.t with type l := U256.t l
module I512 : SRangeF with type elm := I512.t with type l := U512.t l

module U8 : SRangeF with type elm := U8.t with type l := U8.t l
module U16 : SRangeF with type elm := U16.t with type l := U16.t l
module U32 : SRangeF with type elm := U32.t with type l := U32.t l
module U64 : SRangeF with type elm := U64.t with type l := U64.t l
module Uns : SRangeF with type elm := uns with type l := uns l
module U128 : SRangeF with type elm := U128.t with type l := U128.t l
module U256 : SRangeF with type elm := U256.t with type l := U256.t l
module U512 : SRangeF with type elm := U512.t with type l := U512.t l
