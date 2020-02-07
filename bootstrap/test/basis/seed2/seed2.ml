open Hemlock

let () = Format.printf "HEMLOCK_ENTROPY=%a -> seed=%a\n"
  String.pp (Sys.getenv "HEMLOCK_ENTROPY")
  Hash.State.pp Hash.State.seed
