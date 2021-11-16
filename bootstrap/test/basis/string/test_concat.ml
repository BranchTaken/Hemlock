open! Basis.Rudiments
open! Basis
open String

let pp_s_nl s formatter =
  formatter
  |> fmt s
  |> Basis.Fmt.fmt "\n"

let test () =
  File.Fmt.stdout
  |> pp_s_nl (concat [""])
  |> pp_s_nl (concat [""; ""])
  |> pp_s_nl (concat [""; ""; ""])

  |> pp_s_nl (concat ~sep:":" [""])
  |> pp_s_nl (concat ~sep:":" [""; ""])
  |> pp_s_nl (concat ~sep:":" [""; ""; ""])

  |> pp_s_nl (concat ["a"])
  |> pp_s_nl (concat ["a"; ""])
  |> pp_s_nl (concat ["a"; "b"])
  |> pp_s_nl (concat ["a"; "b"; "c"])

  |> pp_s_nl (concat ~sep:":" ["a"; "b"; "c"])
  |> pp_s_nl (concat ~sep:".." ["a"; "b"; "c"])
  |> pp_s_nl (concat ~sep:":" ["ab"; "cd"; "ef"])

  |> pp_s_nl (concat ~sep:":" ["a"; ""; ""])
  |> pp_s_nl (concat ~sep:":" ["a"; "b"; ""])
  |> pp_s_nl (concat ~sep:":" ["a"; ""; "c"])
  |> pp_s_nl (concat ~sep:":" [""; "b"; "c"])
  |> pp_s_nl (concat ~sep:":" [""; ""; "c"])

  |> pp_s_nl (concat_rev ~sep:":" ["a"; "b"; "c"])
  |> pp_s_nl ("a" ^ "b" ^ "c")

  |> ignore

let _ = test ()
