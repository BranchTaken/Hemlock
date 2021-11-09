open Basis

let () = Format.printf "HEMLOCK_ENTROPY=%a -> seed=%a\n"
    String.xpp (Sys.getenv "HEMLOCK_ENTROPY")
    Hash.State.xpp Hash.State.seed
