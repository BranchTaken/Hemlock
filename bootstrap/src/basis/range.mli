(** Alias for {!module:RangeH.Uns}, used ubiquitously. *)

open RudimentsInt0

type range
include RangeIntf.SRangeH with type elm := uns with type l := uns with type t = range
