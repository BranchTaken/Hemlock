open! Basis.Rudiments
open! Basis
open Codepoint
open Format

let xpp_x xppf cp =
  Format.fprintf xppf "%a" Uns.xpp_x (extend_to_uns cp)

let test () =
  let open Utf8 in
  let rec test_codepoints = function
    | [] -> ()
    | codepoint :: codepoints' -> begin
        let utf8 = of_codepoint codepoint in
        let codepoint' = to_codepoint utf8 in
        let bytes = to_bytes utf8 in
        let length = length utf8 in
        printf "codepoint=%a, codepoint'=%a, bytes=["
          xpp_x codepoint xpp_x codepoint';
        let rec bytes_iteri i = function
          | [] -> ()
          | b :: bytes' -> begin
              let space = if Uns.(i = 0L) then "" else " " in
              let sep = if Uns.(succ i < length) then ";" else "" in
              printf "%s%a%s" space Byte.xpp_x b sep;
              bytes_iteri (succ i) bytes'
            end
        in
        bytes_iteri 0L bytes;
        printf "], length=%a\n" Uns.xpp length;
        test_codepoints codepoints'
      end
  in
  let codepoints =
    [
      (kv 0x3cL); (* < *)
      (kv 0xabL); (* « *)
      (kv 0x2021L); (* ‡ *)
      (kv 0x10197L); (* 𐆗 *)
    ]
  in
  test_codepoints codepoints

let _ = test ()
