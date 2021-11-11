open! Basis.Rudiments
open! Basis
open Codepoint

let pp_uns_x u formatter =
  formatter |> Uns.fmt ~alt:true ~zpad:true ~width:16L ~base:Fmt.Hex u

let pp_x cp formatter =
  formatter |> pp_uns_x (extend_to_uns cp)

let test () =
  let open Utf8 in
  let rec test_codepoints = function
    | [] -> ()
    | codepoint :: codepoints' -> begin
        let utf8 = of_codepoint codepoint in
        let codepoint' = to_codepoint utf8 in
        let bytes = to_bytes utf8 in
        let length = length utf8 in
        File.Fmt.stdout
        |> Fmt.fmt "codepoint="
        |> pp_x codepoint
        |> Fmt.fmt ", codepoint'="
        |> pp_x codepoint'
        |> Fmt.fmt ", bytes="
        |> List.pp (Byte.fmt ~alt:true ~zpad:true ~width:2L ~base:Fmt.Hex ~pretty:true) bytes
        |> Fmt.fmt ", length="
        |> Uns.pp length
        |> Fmt.fmt "\n"
        |> ignore;
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
