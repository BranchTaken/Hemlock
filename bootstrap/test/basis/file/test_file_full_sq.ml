open Basis.Rudiments
open Basis

external nop_submit: unit -> (sint * uns) = "hemlock_basis_executor_nop_submit_inner"
external sqring_pp: File.t -> unit = "hemlock_basis_executor_sqring_pp"
external user_data_decref: uns -> unit = "hemlock_basis_executor_user_data_decref"

let () =
  let submit_nop () = begin
    let _, user_data = nop_submit () in
    user_data_decref (user_data)
  end in
  sqring_pp File.stdout;
  let () = Range.Uns.iter ~f:(fun _ -> submit_nop ()) (0L=:<32L) in
  sqring_pp File.stdout;
  submit_nop ();
  sqring_pp File.stdout
