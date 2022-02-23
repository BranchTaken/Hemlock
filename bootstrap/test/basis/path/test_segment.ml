open! Basis.Rudiments
open! Basis
open Path

let test_string () =
  List.iter [
    "/";
    ".";
    "..";
    "basename";
    "basename.suf";
    "basename.suf.ext";
    ".suf";
    ".suf.ext";
  ] ~f:(fun path_str ->
    let path = of_string path_str in
    let segment = Option.value_hlt (basename path) in
    File.Fmt.stdout
    |> String.pp path_str |> Fmt.fmt " -> " |> Segment.pp segment |> Fmt.fmt "\n"
    |> Fmt.fmt "    is_empty -> " |> Bool.pp (Segment.is_empty segment) |> Fmt.fmt "\n"
    |> Fmt.fmt "    is_current -> " |> Bool.pp (Segment.is_current segment) |> Fmt.fmt "\n"
    |> Fmt.fmt "    is_parent -> " |> Bool.pp (Segment.is_parent segment) |> Fmt.fmt "\n"

    |> Fmt.fmt "    split -> "
    |> (List.pp String.pp) (List.map (Segment.split segment) ~f:Segment.to_string_hlt)
    |> Fmt.fmt "\n"

    |> Fmt.fmt "    stem -> " |> String.pp Segment.(to_string_hlt (stem segment)) |> Fmt.fmt "\n"

    |> Fmt.fmt "    suffixes -> "
    |> (List.pp String.pp) (List.map (Segment.suffixes segment) ~f:Segment.to_string_hlt)
    |> Fmt.fmt "\n"

    |> Fmt.fmt "    suffix -> " |> String.pp Segment.(to_string_hlt (suffix segment))
    |> Fmt.fmt "\n"

    |> Fmt.fmt "    join -> " |> Segment.pp Segment.(join (split segment)) |> Fmt.fmt "\n"
    |> ignore
  )

let test_bytes () =
  List.iter [
    Bytes.Slice.of_string_slice (String.C.Slice.of_string "/");
    Bytes.Slice.of_string_slice (String.C.Slice.of_string ".");
    Bytes.Slice.of_string_slice (String.C.Slice.of_string "..");
    Bytes.Slice.of_string_slice (String.C.Slice.of_string "basename");
    Bytes.Slice.of_string_slice (String.C.Slice.of_string "basename.suf");
    Bytes.Slice.of_string_slice (String.C.Slice.of_string "basename.suf.ext");
    Bytes.Slice.of_string_slice (String.C.Slice.of_string ".suf");
    Bytes.Slice.of_string_slice (String.C.Slice.of_string ".suf.ext");
    Bytes.Slice.init [|
      U8.kv 0x5fL; U8.kv 0x61L;
      U8.kv 0x2eL; U8.kv 0x5fL; U8.kv 0x62L;
      U8.kv 0x2eL; U8.kv 0x5fL; U8.kv 0x63L
    |];
    Bytes.Slice.init [|
      U8.kv 0x5fL; U8.kv 0x61L;
      U8.kv 0x2eL; U8.kv 0x5fL; U8.kv 0x62L;
      U8.kv 0x2eL; U8.kv 0xffL; U8.kv 0x63L
    |];
    Bytes.Slice.init [|
      U8.kv 0x5fL; U8.kv 0x61L;
      U8.kv 0x2eL; U8.kv 0xffL; U8.kv 0x62L;
      U8.kv 0x2eL; U8.kv 0x5fL; U8.kv 0x63L
    |];
    Bytes.Slice.init [|
      U8.kv 0x5fL; U8.kv 0x61L;
      U8.kv 0x2eL; U8.kv 0xffL; U8.kv 0x62L;
      U8.kv 0x2eL; U8.kv 0xffL; U8.kv 0x63L
    |];
    Bytes.Slice.init [|
      U8.kv 0xffL; U8.kv 0x61L;
      U8.kv 0x2eL; U8.kv 0x5fL; U8.kv 0x62L;
      U8.kv 0x2eL; U8.kv 0x5fL; U8.kv 0x63L
    |];
    Bytes.Slice.init [|
      U8.kv 0xffL; U8.kv 0x61L;
      U8.kv 0x2eL; U8.kv 0x5fL; U8.kv 0x62L;
      U8.kv 0x2eL; U8.kv 0xffL; U8.kv 0x63L
    |];
    Bytes.Slice.init [|
      U8.kv 0xffL; U8.kv 0x61L;
      U8.kv 0x2eL; U8.kv 0xffL; U8.kv 0x62L;
      U8.kv 0x2eL; U8.kv 0x5fL; U8.kv 0x63L
    |];
    Bytes.Slice.init [|
      U8.kv 0xffL; U8.kv 0x61L;
      U8.kv 0x2eL; U8.kv 0xffL; U8.kv 0x62L;
      U8.kv 0x2eL; U8.kv 0xffL; U8.kv 0x63L
    |]
  ] ~f:(fun path_bytes ->
    let path = of_bytes path_bytes in
    let segment = Option.value_hlt (basename path) in
    File.Fmt.stdout
    |> Bytes.Slice.pp path_bytes |> Fmt.fmt " ~ " |> String.pp (Segment.to_string_replace segment)
    |> Fmt.fmt " -> " |> Segment.pp segment |> Fmt.fmt "\n"

    |> Fmt.fmt "    is_empty -> " |> Bool.pp (Segment.is_empty segment) |> Fmt.fmt "\n"
    |> Fmt.fmt "    is_current -> " |> Bool.pp (Segment.is_current segment) |> Fmt.fmt "\n"
    |> Fmt.fmt "    is_parent -> " |> Bool.pp (Segment.is_parent segment) |> Fmt.fmt "\n"

    |> Fmt.fmt "    split -> "
    |> (List.pp String.pp) (List.map (Segment.split segment) ~f:Segment.to_string_replace)
    |> Fmt.fmt "\n"

    |> Fmt.fmt "    stem -> " |> String.pp Segment.(to_string_replace (stem segment)) |> Fmt.fmt "\n"

    |> Fmt.fmt "    suffixes -> "
    |> (List.pp String.pp) (List.map (Segment.suffixes segment) ~f:Segment.to_string_replace)
    |> Fmt.fmt "\n"

    |> Fmt.fmt "    suffix -> " |> String.pp Segment.(to_string_replace (suffix segment))
    |> Fmt.fmt "\n"

    |> Fmt.fmt "    join -> " |> Segment.pp Segment.(join (split segment)) |> Fmt.fmt "\n"
    |> ignore
  )

let _ = test_string ()
let _ = test_bytes ()
