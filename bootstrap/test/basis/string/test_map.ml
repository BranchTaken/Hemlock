open! Basis.Rudiments
open! Basis
open String
open Format

let test () =
  let s = "abcde" in
  printf "map: %a -> %a\n" pp s pp (map s ~f:(fun cp ->
    Codepoint.of_uns ((Codepoint.to_uns cp) - 32L)));
  printf "mapi: %a -> %a\n" pp s pp (mapi s ~f:(fun i cp ->
    match (bit_and i 0x1L) with
    | 0L -> cp
    | 1L -> Codepoint.of_uns ((Codepoint.to_uns cp) - 32L)
    | _ -> not_reached ()
  ));
  let s = "a:b:cd:e" in
  printf "tr: %a -> %a\n" pp s pp (tr s ~target:Codepoint.(of_char ':')
    ~replacement:Codepoint.(of_char ' '));
  printf "filter: %a -> %a\n" pp s pp (filter s ~f:(fun codepoint ->
    Codepoint.(codepoint <> (of_char ':'))
  ))

let _ = test ()
