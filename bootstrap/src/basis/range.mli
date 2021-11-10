(** Alias for {!module:RangeH.Uns}, used ubiquitously. *)

open RudimentsInt0

type range
include RangeIntf.SRangeH with type elm := uns with type l := uns with type t = range

val pp_l: ('a -> (module Fmt.Formatter) -> (module Fmt.Formatter)) -> 'a RangeIntf.l
  -> (module Fmt.Formatter) -> (module Fmt.Formatter)
(** [pp_l l formatter] applies a formatted representation of [l] to the [formatter] using [pp_a] for
    the parametric type value [a]. *)
