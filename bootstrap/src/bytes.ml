(* Partial Rudiments. *)
module Int = U63
module Codepoint = U21
module Byte = U8
type 'a array = 'a Array.t
type string = String.t
type cursor = String.Cursor.t
type int = Int.t
type codepoint = Codepoint.t
type byte = Byte.t
open Rudiments_functions

let of_codepoint cp =
  Array.of_list (Utf8.to_bytes (Utf8.of_codepoint cp))

module Array_seq = struct
  module T = struct
    type t = {
      string: string;
      cursor: cursor;
      bindex: int;
      rem_bytes: byte list;
    }
    type elm = codepoint

    let init t =
      {
          string=t;
          cursor=(String.Cursor.at t ~bindex:0);
          bindex=0;
          rem_bytes=[];
      }

    let length t =
      (String.blength t.string) - t.bindex

    let next t =
      assert (Int.(length t > 0));
      match t.rem_bytes with
      | b :: rem_bytes' -> begin
          let t' = {t with
                    bindex=(succ t.bindex);
                    rem_bytes=rem_bytes'
                   } in
          b, t'
        end
      | [] -> begin
          let codepoint = String.Cursor.rget t.cursor in
          let bytes = Utf8.(to_bytes (of_codepoint codepoint)) in
          let b, rem_bytes = match bytes with
            | b :: bytes' -> b, bytes'
            | [] -> not_reached ()
          in
          let t' = {t with
                    cursor=(String.Cursor.succ t.cursor);
                    bindex=(succ t.bindex);
                    rem_bytes} in
          b, t'
        end
  end
  include T
  include Array.Seq.Make(T)
end

let of_string s =
  Array_seq.to_array (Array_seq.init s)

module Utf8_seq = struct
  module T = struct
    type t = {
      bytes: byte array;
      bindex: int;
    }

    let init t =
      {bytes=t; bindex=0}

    let length t =
      Int.((Array.length t.bytes) - t.bindex)

    let next t =
      match Int.((length t) = 0) with
      | true -> None
      | false -> begin
          let b = Array.get t.bytes t.bindex in
          let t' = {t with bindex=(succ t.bindex)} in
          Some (b, t')
        end
  end
  include T
  include Utf8.Seq.Make(T)
end

module String_seq = struct
  module U = struct
    include Utf8_seq

    let next t =
      match to_utf8_hlt t with
      | Some (utf8, t') -> (Utf8.to_codepoint utf8), t'
      | None -> not_reached ()
  end
  include U
  include String.Seq.Codepoint.Make(U)
end

let to_string bytes =
  let rec validate seq = begin
    match Utf8_seq.to_utf8 seq with
    | None -> false
    | Some (Ok _, seq') -> validate seq'
    | Some (Error _, _) -> true
  end in
  let invalid = validate (Utf8_seq.init bytes) in
  match invalid with
  | true -> None
  | false -> Some (String_seq.(to_string (init bytes)))

let to_string_hlt bytes =
  match to_string bytes with
  | None -> halt "Invalid utf8 sequence"
  | Some s -> s

(*******************************************************************************
 * Begin tests.
 *)

let%expect_test "of_codepoint" =
  let open Printf in
  let strs = [
    "<";
    "«";
    "‡";
    "𐆗";
  ] in
  let cps = List.fold_right (fun s cps ->
    String.Cursor.(rget (hd s)) :: cps
  ) strs [] in
  List.iter (fun cp ->
    printf "'%s' -> [|" (String.of_codepoint cp);
    let bytes = of_codepoint cp in
    Array.iteri bytes ~f:(fun i b ->
      printf "%s%#02x" (if Int.(i = 0) then "" else "; ") b);
    printf "|] -> \"%s\"\n" (to_string_hlt bytes)
  ) cps;

  [%expect{|
    '<' -> [|0x3c|] -> "<"
    '«' -> [|0xc2; 0xab|] -> "«"
    '‡' -> [|0xe2; 0x80; 0xa1|] -> "‡"
    '𐆗' -> [|0xf0; 0x90; 0x86; 0x97|] -> "𐆗"
    |}]

let%expect_test "of_string" =
  let open Printf in
  let strs = [
    "";
    "<_>«‡𐆗»[_]";
  ] in
  List.iter (fun s ->
    printf "\"%s\" -> [|" s;
    let bytes = of_string s in
    Array.iteri bytes ~f:(fun i b ->
      printf "%s%#02x" (if Int.(i = 0) then "" else "; ") b);
    printf "|] -> \"%s\"\n" (to_string_hlt bytes)
  ) strs;

  [%expect{|
    "" -> [||] -> ""
    "<_>«‡𐆗»[_]" -> [|0x3c; 0x5f; 0x3e; 0xc2; 0xab; 0xe2; 0x80; 0xa1; 0xf0; 0x90; 0x86; 0x97; 0xc2; 0xbb; 0x5b; 0x5f; 0x5d|] -> "<_>«‡𐆗»[_]"
    |}]

let%expect_test "to_string" =
  let open Printf in
  let test_to_string bytes_list = begin
    let bytes = Array.of_list bytes_list in
    printf "to_string [|";
    Array.iteri bytes ~f:(fun i b ->
      printf "%s%#02x" (if Int.(i = 0) then "" else "; ") b);
    printf "|] -> %s\n" (match to_string bytes with
      | None -> "None"
      | Some s -> "\"" ^ s ^ "\""
    );
  end in
  test_to_string [0x61];
  test_to_string [0xf0; 0x80; 0x80];
  test_to_string [0xe0; 0x80];
  test_to_string [0xc0];
  test_to_string [0xf0; 0x80; 0x80; 0xf0];
  test_to_string [0xe0; 0x80; 0xe0];
  test_to_string [0xc0; 0xc0];
  test_to_string [0x80];
  test_to_string [0x80; 0x80; 0x80; 0x80];

  [%expect{|
    to_string [|0x61|] -> "a"
    to_string [|0xf0; 0x80; 0x80|] -> None
    to_string [|0xe0; 0x80|] -> None
    to_string [|0xc0|] -> None
    to_string [|0xf0; 0x80; 0x80; 0xf0|] -> None
    to_string [|0xe0; 0x80; 0xe0|] -> None
    to_string [|0xc0; 0xc0|] -> None
    to_string [|0x80|] -> None
    to_string [|0x80; 0x80; 0x80; 0x80|] -> None
    |}]
