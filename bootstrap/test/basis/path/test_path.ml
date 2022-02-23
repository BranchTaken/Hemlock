open! Basis.Rudiments
open! Basis
open Path

let test_string () =
  List.iter [
    "";
    ".";
    "..";

    "a";
    "a/";
    "a/b";
    "a/b/";
    "a/b/c";

    "/";
    "/a";
    "/a/";
    "/a/b";
    "/a/b/";
    "/a/b/c";

    "//";
    "//a";

    "///";
    "///a";

    "a//";
    "a//b";

    "a///";
    "a///b";

    "a/.";
    "a/./";
    "a/./b";

    "a/..";
    "a/../";
    "a/../b";
  ] ~f:(fun path_str ->
    let path = of_string path_str in
    File.Fmt.stdout
    |> String.pp path_str |> Fmt.fmt " -> " |> pp path |> Fmt.fmt "\n"
    |> Fmt.fmt "    is_abs -> " |> Bool.pp (is_abs path) |> Fmt.fmt "\n"
    |> Fmt.fmt "    is_empty -> " |> Bool.pp (is_empty path) |> Fmt.fmt "\n"

    |> Fmt.fmt "    dirname -> " |> pp (dirname path) |> Fmt.fmt "\n"
    |> Fmt.fmt "    basename -> " |> (Option.pp Segment.pp) (basename path) |> Fmt.fmt "\n"
    |> Fmt.fmt "    split -> " |> (fun formatter ->
      let dirname, basename_opt = split path in
      formatter
      |> pp dirname
      |> Fmt.fmt ", "
      |> (Option.pp Segment.pp) basename_opt
    ) |> Fmt.fmt "\n"

    |> Fmt.fmt "    normalize -> " |> String.pp (to_string_hlt (normalize path)) |> Fmt.fmt "\n"

    |> Fmt.fmt "    join -> " |> (fun formatter ->
      let dirname, basename_opt = split path in
      let path' =  match basename_opt with
        | None -> path
        | Some basename -> join [dirname; of_segment basename]
      in
      formatter |> pp path'
    ) |> Fmt.fmt "\n"

    |> Fmt.fmt "    to_string -> " |> (Option.pp String.pp) (to_string path) |> Fmt.fmt "\n"
    |> Fmt.fmt "    to_string_replace -> " |> String.pp (to_string_replace path) |> Fmt.fmt "\n"
    |> Fmt.fmt "    to_string_hlt -> " |> String.pp (to_string_hlt path) |> Fmt.fmt "\n"
    |> Fmt.fmt "    to_bytes -> ~ " |> String.pp (Bytes.Slice.to_string_hlt (to_bytes path))
    |> Fmt.fmt "\n"
    |> ignore
  )

let test_bytes () =
  List.iter [
    Bytes.Slice.of_string_slice (String.C.Slice.of_string "a/./../b");
    Bytes.Slice.init [|
      U8.kv 0xffL; U8.kv 0x61L
    |];
    Bytes.Slice.init [|
      U8.kv 0xffL; U8.kv 0x61L;
      U8.kv 0x2fL;
      U8.kv 0xffL; U8.kv 0x62L
    |];
    Bytes.Slice.init [|
      U8.kv 0xffL; U8.kv 0x61L;
      U8.kv 0x2fL;
      U8.kv 0x2eL;
      U8.kv 0x2fL;
      U8.kv 0x2eL; U8.kv 0x2eL;
      U8.kv 0x2fL;
      U8.kv 0xffL; U8.kv 0x62L
    |];
  ] ~f:(fun path_bytes ->
    let path = of_bytes path_bytes in
    File.Fmt.stdout
    |> Bytes.Slice.pp path_bytes |> Fmt.fmt " ~ " |> String.pp (Path.to_string_replace path)
    |> Fmt.fmt " -> " |> pp path |> Fmt.fmt "\n"

    |> Fmt.fmt "    is_abs -> " |> Bool.pp (is_abs path) |> Fmt.fmt "\n"
    |> Fmt.fmt "    is_empty -> " |> Bool.pp (is_empty path) |> Fmt.fmt "\n"

    |> Fmt.fmt "    dirname -> " |> pp (dirname path) |> Fmt.fmt "\n"
    |> Fmt.fmt "    basename -> " |> (Option.pp Segment.pp) (basename path) |> Fmt.fmt "\n"
    |> Fmt.fmt "    split -> " |> (fun formatter ->
      let dirname, basename_opt = split path in
      formatter
      |> pp dirname
      |> Fmt.fmt ", "
      |> (Option.pp Segment.pp) basename_opt
    ) |> Fmt.fmt "\n"

    |> Fmt.fmt "    normalize -> " |> String.pp (to_string_replace (normalize path)) |> Fmt.fmt "\n"

    |> Fmt.fmt "    join -> " |> (fun formatter ->
      let dirname, basename_opt = split path in
      let path' =  match basename_opt with
        | None -> path
        | Some basename -> join [dirname; of_segment basename]
      in
      formatter |> pp path'
      |> Fmt.fmt "\n          ~ " |> String.pp (to_string_replace path')
    ) |> Fmt.fmt "\n"

    |> Fmt.fmt "    to_string -> " |> (Option.pp String.pp) (to_string path) |> Fmt.fmt "\n"
    |> Fmt.fmt "    to_string_replace -> " |> String.pp (to_string_replace path) |> Fmt.fmt "\n"
    |> Fmt.fmt "    to_bytes -> " |> Bytes.Slice.pp (to_bytes path)
    |> Fmt.fmt "\n"
    |> ignore
  )

let _ = test_string ()
let _ = test_bytes ()
