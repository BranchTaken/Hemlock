open! Basis.Rudiments
open! Basis
open Real

(* The test output is quite large, so only its hash is printed by default. Set verbose to true
 * while modifying the test or diagnosing regressions. *)
let verbose = false

let test () =
  let rec fn xs formatter =
    match xs with
    | [] -> formatter
    | x :: xs' ->
      List.fold ~init:(
        formatter
        |> fmt ~alt:true ~radix:Radix.Hex x
        |> Fmt.fmt "\n"
      ) [Radix.Bin; Radix.Oct; Radix.Dec; Radix.Hex] ~f:(fun formatter radix ->
        List.fold ~init:formatter [Fmt.Implicit; Fmt.Explicit; Fmt.Space] ~f:(fun formatter sign ->
          List.fold ~init:formatter [false; true] ~f:(fun formatter alt ->
            List.fold ~init:formatter [Fmt.Normalized; Fmt.RadixPoint; Fmt.Compact]
              ~f:(fun formatter notation ->
                formatter
                |> Fmt.fmt "["
                |> Fmt.fmt ~pad:"_" ~width:5L (
                  String.Fmt.empty
                  |> fmt ~alt ~sign ~notation ~radix x
                  |> Fmt.to_string
                )
                |> Fmt.fmt "] %"
                |> Fmt.fmt (
                  match sign with
                  | Fmt.Implicit -> ""
                  | Fmt.Explicit -> "+"
                  | Fmt.Space -> "_"
                )
                |> Fmt.fmt (match alt with false -> "" | true -> "#")
                |> Fmt.fmt (
                  match radix with
                  | Radix.Bin -> "b"
                  | Radix.Oct -> "o"
                  | Radix.Dec -> "d"
                  | Radix.Hex -> "h"
                )
                |> Fmt.fmt (
                  match notation with
                  | Fmt.Normalized -> "m"
                  | Fmt.RadixPoint -> "a"
                  | Fmt.Compact -> "c"
                )
                |> Fmt.fmt "r\n"
              )
          )
        )
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
  |> Fmt.fmt (U128.to_string ~alt:true ~zpad:true ~width:32L ~radix:Radix.Hex ~pretty:true
      (Hash.State.empty |> String.hash_fold output |> Hash.t_of_state))

let _ = test ()
