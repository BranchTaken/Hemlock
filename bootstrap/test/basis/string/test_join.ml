open! Basis.Rudiments
open! Basis
open String

let pp_s_nl s formatter =
  formatter
  |> fmt s
  |> Basis.Fmt.fmt "\n"

let test () =
  File.Fmt.stdout
  |> pp_s_nl (join [""])
  |> pp_s_nl (join [""; ""])
  |> pp_s_nl (join [""; ""; ""])

  |> pp_s_nl (join ~sep:":" [""])
  |> pp_s_nl (join ~sep:":" [""; ""])
  |> pp_s_nl (join ~sep:":" [""; ""; ""])

  |> pp_s_nl (join ["a"])
  |> pp_s_nl (join ["a"; ""])
  |> pp_s_nl (join ["a"; "b"])
  |> pp_s_nl (join ["a"; "b"; "c"])

  |> pp_s_nl (join ~sep:":" ["a"; "b"; "c"])
  |> pp_s_nl (join ~sep:".." ["a"; "b"; "c"])
  |> pp_s_nl (join ~sep:":" ["ab"; "cd"; "ef"])

  |> pp_s_nl (join ~sep:":" ["a"; ""; ""])
  |> pp_s_nl (join ~sep:":" ["a"; "b"; ""])
  |> pp_s_nl (join ~sep:":" ["a"; ""; "c"])
  |> pp_s_nl (join ~sep:":" [""; "b"; "c"])
  |> pp_s_nl (join ~sep:":" [""; ""; "c"])

  |> pp_s_nl (join_rev ~sep:":" ["a"; "b"; "c"])
  |> pp_s_nl ("a" ^ "b" ^ "c")

  |> ignore

let _ = test ()
