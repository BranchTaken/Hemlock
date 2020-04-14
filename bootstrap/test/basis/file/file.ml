open Basis
open Basis.Rudiments

let () =
  let file = File.of_path_hlt (File.Buffer.of_string "foo") in
  let _ = File.write_hlt (File.Buffer.of_string "these are the file contents") file in
  let _ = File.seek_hd_hlt (Sint.kv 0) file in
  let buffer = File.read_hlt file in
  let _ = File.close_hlt file in
  File.write_hlt buffer File.stdout
