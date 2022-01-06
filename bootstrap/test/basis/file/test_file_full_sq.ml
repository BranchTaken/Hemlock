open Basis.Rudiments
open Basis

external nop_submit: unit -> (sint * uns) = "hm_basis_file_nop_submit_inner"
external setup_inner: unit -> sint = "hm_basis_file_setup_inner"
external sqring_pp: File.t -> unit = "hm_basis_file_sqring_pp"
external teardown_inner: unit -> unit = "hm_basis_file_teardown_inner"
external user_data_decref: uns -> unit = "hm_basis_file_user_data_decref"

let () =
  let () = match setup_inner () = 0L with
  | false -> halt "Setup failure"
  | true -> ()
  in
  let submit_nop () = begin
    let _, user_data = nop_submit () in
    user_data_decref (user_data)
  end in
  sqring_pp File.stdout;
  let () = Range.iter ~f:(fun _ -> submit_nop ()) (0L=:<32L) in
  sqring_pp File.stdout;
  submit_nop ();
  sqring_pp File.stdout;
  teardown_inner ()
