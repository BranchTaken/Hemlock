open! Basis.Rudiments
open! Basis
open U64

(* The test output is quite large, so only its hash is printed by default. Set verbose to true
 * while modifying the test or diagnosing regressions. *)
let verbose = false

let test () =
  let rec fn xs formatter =
    match xs with
    | [] -> formatter
    | x :: xs' -> begin
        List.fold ~init:(
          formatter
          |> fmt ~alt:true ~zpad:true ~width:16L ~base:Fmt.Hex x
          |> String.fmt "\n"
        ) [Fmt.Bin; Fmt.Oct; Fmt.Dec; Fmt.Hex] ~f:(fun formatter base ->
          List.fold ~init:formatter [Fmt.Implicit; Fmt.Explicit; Fmt.Space]
            ~f:(fun formatter sign ->
              List.fold ~init:formatter [false; true] ~f:(fun formatter alt ->
                List.fold ~init:formatter [false; true] ~f:(fun formatter zpad ->
                  List.fold ~init:formatter [0L; 20L] ~f:(fun formatter width ->
                    List.fold ~init:formatter [Fmt.Left; Fmt.Center; Fmt.Right]
                      ~f:(fun formatter just ->
                        formatter
                        |> String.fmt "["
                        |> String.fmt ~pad:(Codepoint.of_char '_') ~width:74L (
                          String.Fmt.empty
                          |> fmt ~pad:"·" ~just ~sign ~alt ~zpad ~width ~base x
                          |> Fmt.to_string
                        )
                        |> String.fmt "] %'·'"
                        |> String.fmt (
                          match just with
                          | Fmt.Left -> "["
                          | Fmt.Center -> "]["
                          | Fmt.Right -> "]"
                        )
                        |> String.fmt (
                          match sign with
                          | Fmt.Implicit -> ""
                          | Fmt.Explicit -> "+"
                          | Fmt.Space -> "_"
                        )
                        |> String.fmt (match alt with false -> "" | true -> "#")
                        |> String.fmt (match zpad with false -> "" | true -> "0")
                        |> (match width with 0L -> String.fmt "" | _ -> fmt width)
                        |> String.fmt (
                          match base with
                          | Fmt.Bin -> "b"
                          | Fmt.Oct -> "o"
                          | Fmt.Dec -> "d"
                          | Fmt.Hex -> "h"
                        )
                        |> String.fmt "\n"
                      )
                  )
                )
              )
            )
        )
        |> fn xs'
      end
  in
  let output =
    String.Fmt.empty |>
    fn [
      zero;
      one;
      42L;
      0xfedc_ba98_7654_3210L;
      max_value;
    ]
    |> Fmt.to_string
  in
  File.Fmt.stdout
  |> Fmt.fmt (match verbose with true -> output | false -> "")
  |> String.fmt (U128.to_string ~alt:true ~zpad:true ~width:32L ~base:Fmt.Hex
      (Hash.State.empty |> String.hash_fold output |> Hash.t_of_state))

let _ = test ()
