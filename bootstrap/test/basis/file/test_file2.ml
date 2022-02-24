open Basis

let buffer = Bytes.Slice.of_string_slice (String.C.Slice.of_string "
Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor
incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis
nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat.
Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu
fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident, sunt in
culpa qui officia deserunt mollit anim id est laborum.
")

let () =
  let file = File.of_path_hlt ~flag:File.Flag.RW (Path.of_string "./file2") in
  let _ = File.write_hlt buffer file in
  let _ = File.seek_hd_hlt (Sint.kv 0L) file in
  let file_stream = File.Stream.of_file file in
  File.Stream.write_hlt File.stdout file_stream
