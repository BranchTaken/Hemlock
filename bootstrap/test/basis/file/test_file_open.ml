open Basis

external user_data_pp: File.t -> File.Open.t -> unit = "hm_basis_file_user_data_pp"
external sqring_pp: File.t -> unit = "hm_basis_file_sqring_pp"

let () =
  let s = File.Open.submit_hlt ~flag:File.Flag.RW (Path.of_string "file") in
  user_data_pp File.stdout s;
  sqring_pp File.stdout;
  let _ = File.Open.complete_hlt s in
  user_data_pp File.stdout s;
  sqring_pp File.stdout;
  ()
