open! Basis.Rudiments
open! Basis
open Real

(* XXX Remove verbose? *)
(* The test output is quite large, so only its hash is printed by default. Set verbose to true
 * while modifying the test or diagnosing regressions. *)
let verbose = true

let test () =
  let rec fn xs formatter =
    match xs with
    | [] -> formatter
    | x :: xs' ->
      List.fold ~init:(
        formatter
        |> xfmt ~alt:true ~precision:13L ~notation:Fmt.Normalized ~base:Fmt.Hex x
        |> Fmt.fmt "\n"
      ) [Fmt.Implicit; Fmt.Explicit; Fmt.Space] ~f:(fun formatter sign ->
        formatter
        |> Fmt.fmt "["
        |> Fmt.xfmt ~pad:"_" ~width:5L (
          String.Fmt.empty
          |> xfmt ~sign ~notation:Fmt.Normalized ~base:Fmt.Hex x (* XXX Test all notations/bases. *)
          |> Fmt.to_string
        )
        |> Fmt.fmt "] %"
        |> Fmt.fmt (
          match sign with
          | Fmt.Implicit -> ""
          | Fmt.Explicit -> "+"
          | Fmt.Space -> "_"
        )
        |> Fmt.fmt "r\n"
      )
      |> fn xs'
  in
  let output =
    String.Fmt.empty |>
    fn [
      neg_inf;
      inf;
      nan;
    ]
    |> Fmt.to_string
  in
  File.Fmt.stdout
  |> Fmt.fmt (match verbose with true -> output | false -> "")
  |> Fmt.fmt (U128.to_string ~alt:true ~zpad:true ~width:32L ~base:Fmt.Hex
      (Hash.State.empty |> String.hash_fold output |> Hash.t_of_state))

let _ = test ()
