open! Basis.Rudiments
open! Basis
open String
open Format

let test () =
  let s = "abcde" in
  printf "map: %a -> %a\n" xpp s xpp (map s ~f:(fun cp ->
    Codepoint.trunc_of_uns ((Codepoint.extend_to_uns cp) - 32L)));
  printf "mapi: %a -> %a\n" xpp s xpp (mapi s ~f:(fun i cp ->
    match (bit_and i 0x1L) with
    | 0L -> cp
    | 1L -> Codepoint.trunc_of_uns ((Codepoint.extend_to_uns cp) - 32L)
    | _ -> not_reached ()
  ));
  let s = "a:b:cd:e" in
  printf "tr: %a -> %a\n" xpp s xpp (tr s ~target:Codepoint.(of_char ':')
    ~replacement:Codepoint.(of_char ' '));
  printf "filter: %a -> %a\n" xpp s xpp (filter s ~f:(fun codepoint ->
    Codepoint.(codepoint <> (of_char ':'))
  ))

let _ = test ()
