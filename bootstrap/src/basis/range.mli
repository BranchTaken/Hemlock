(** Type-specific ranges. *)

open RudimentsInt0
open RangeIntf

val pp_limit: ('a -> (module Fmt.Formatter) -> (module Fmt.Formatter)) -> 'a RangeIntf.limit
  -> (module Fmt.Formatter) -> (module Fmt.Formatter)
(** [pp_limit limit formatter] applies a formatted representation of [limit] to the [formatter]
    using [pp_a] for the parametric type value [a]. *)

val pp_length: ('a -> (module Fmt.Formatter) -> (module Fmt.Formatter)) -> 'a RangeIntf.length
  -> (module Fmt.Formatter) -> (module Fmt.Formatter)
(** [pp_length length formatter] applies a formatted representation of [length] to the [formatter]
    using [pp_a] for the parametric type value [a]. *)

module I8 : SRange
  with type elm := I8.t
  with type limit := I8.t limit
  with type length := U8.t length
module I16 : SRange
  with type elm := I16.t
  with type limit := I16.t limit
  with type length := U16.t length
module I32 : SRange
  with type elm := I32.t
  with type limit := I32.t limit
  with type length := U32.t length
module I64 : SRange
  with type elm := I64.t
  with type limit := I64.t limit
  with type length := U64.t length
module Sint : SRange
  with type elm := sint
  with type limit := sint limit
  with type length := uns length
module I128 : SRange
  with type elm := I128.t
  with type limit := I128.t limit
  with type length := U128.t length
module I256 : SRange
  with type elm := I256.t
  with type limit := I256.t limit
  with type length := U256.t length
module I512 : SRange
  with type elm := I512.t
  with type limit := I512.t limit
  with type length := U512.t length

module U8 : SRange
  with type elm := U8.t
  with type limit := U8.t limit
  with type length := U8.t length
module U16 : SRange
  with type elm := U16.t
  with type limit := U16.t limit
  with type length := U16.t length
module U32 : SRange
  with type elm := U32.t
  with type limit := U32.t limit
  with type length := U32.t length
module U64 : SRange
  with type elm := U64.t
  with type limit := U64.t limit
  with type length := U64.t length
module Uns : SRange
  with type elm := uns
  with type limit := uns limit
  with type length := uns length
module U128 : SRange
  with type elm := U128.t
  with type limit := U128.t limit
  with type length := U128.t length
module U256 : SRange
  with type elm := U256.t
  with type limit := U256.t limit
  with type length := U256.t length
module U512 : SRange
  with type elm := U512.t
  with type limit := U512.t limit
  with type length := U512.t length

type range = Uns.t
(** Type alias for the overwhelmingly most common range type. *)
