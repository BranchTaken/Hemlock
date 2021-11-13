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
          |> Fmt.fmt "\n"
        ) [Fmt.Bin; Fmt.Oct; Fmt.Dec; Fmt.Hex] ~f:(fun formatter base ->
          List.fold ~init:formatter [Fmt.Implicit; Fmt.Explicit; Fmt.Space]
            ~f:(fun formatter sign ->
              List.fold ~init:formatter [false; true] ~f:(fun formatter alt ->
                List.fold ~init:formatter [false; true] ~f:(fun formatter zpad ->
                  List.fold ~init:formatter [0L; 20L] ~f:(fun formatter width ->
                    List.fold ~init:formatter [Fmt.Left; Fmt.Center; Fmt.Right]
                      ~f:(fun formatter just ->
                        formatter
                        |> Fmt.fmt "["
                        |> Fmt.fmt ~pad:"_" ~width:74L (
                          String.Fmt.empty
                          |> fmt ~pad:"·" ~just ~sign ~alt ~zpad ~width ~base x
                          |> Fmt.to_string
                        )
                        |> Fmt.fmt "] %'·'"
                        |> Fmt.fmt (
                          match just with
                          | Fmt.Left -> "<"
                          | Fmt.Center -> "^"
                          | Fmt.Right -> ">"
                        )
                        |> Fmt.fmt (
                          match sign with
                          | Fmt.Implicit -> ""
                          | Fmt.Explicit -> "+"
                          | Fmt.Space -> "_"
                        )
                        |> Fmt.fmt (match alt with false -> "" | true -> "#")
                        |> Fmt.fmt (match zpad with false -> "" | true -> "0")
                        |> (match width with 0L -> Fmt.fmt "" | _ -> fmt width)
                        |> Fmt.fmt (
                          match base with
                          | Fmt.Bin -> "b"
                          | Fmt.Oct -> "o"
                          | Fmt.Dec -> "d"
                          | Fmt.Hex -> "h"
                        )
                        |> Fmt.fmt "\n"
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
  |> Fmt.fmt (U128.to_string ~alt:true ~zpad:true ~width:32L ~base:Fmt.Hex ~pretty:true
      (Hash.State.empty |> String.hash_fold output |> Hash.t_of_state))

let _ = test ()
