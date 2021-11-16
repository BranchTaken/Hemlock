open! Basis.Rudiments
open! Basis
open Set

let fmt ?(alt=Fmt.alt_default) ?(width=Fmt.width_default) t formatter =
  let cmper = cmper t in
  let t_sorted = Array.sort ~cmp:cmper.cmp (to_array t) in
  let indent = 4L in
  let width' = width + indent in
  formatter
  |> Fmt.fmt "Set "
  |> (Array.fmt ~alt ~width:width' cmper.pp) t_sorted
