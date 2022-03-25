let argv = Array.map Stdlib.Sys.argv ~f:(fun arg ->
  Bytes.of_string_slice (String.C.Slice.of_string arg))
