(** Entropy generation. *)

open Rudiments_int0

val get: unit -> u128
(** Get entropy bits from the operating system.  Prefer to use [state] unless
    there is a specific need for independent intropy. *)

val seed: u128
(** Seed entropy for this execution of the application, set prior to application
    entry.  The seed is typically initialized using entropy bits provided by the
    operating system, but it can be explicitly initialized via the
    [HEMLOCK_ENTROPY] environment variable, which is interpreted as a
    {!type:u128} constant. *)
