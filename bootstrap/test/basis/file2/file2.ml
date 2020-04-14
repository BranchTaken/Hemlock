open Basis
open Basis.Rudiments

let buffer = File.Buffer.of_string "
Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor
incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis
nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat.
Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu
fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident, sunt in
culpa qui officia deserunt mollit anim id est laborum.
"

let () =
  let file = File.of_path_hlt (File.Buffer.of_string "./foo") in
  let _ = File.write_hlt buffer file in
  let _ = File.seek_hd_hlt (Sint.kv 0) file in
  let file_stream = File.Stream.of_file file in
  File.Stream.write_hlt File.stdout file_stream
