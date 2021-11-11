open! Basis.Rudiments
open! Basis
open Ordset

let fmt ?(alt=Fmt.alt_default) ?(width=Fmt.width_default) t formatter =
  let cmper = cmper t in
  let t_sorted = Array.sort ~cmp:cmper.cmp (to_array t) in
  let indent = 4L in
  let width' = width + indent in
  formatter
  |> Fmt.fmt "Ordset "
  |> (Array.fmt ~alt ~width:width' cmper.pp) t_sorted
