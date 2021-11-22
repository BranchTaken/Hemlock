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
        |> fmt ~alt:true ~precision:13L ~notation:Fmt.Normalized ~base:Fmt.Hex x
        |> String.fmt "\n"
      ) [Fmt.Dec] ~f:(fun formatter base ->
        List.fold ~init:formatter [Fmt.Implicit; Fmt.Explicit; Fmt.Space]
          ~f:(fun formatter sign ->
            List.fold ~init:formatter [false; true] ~f:(fun formatter alt ->
              List.fold ~init:formatter [false; true] ~f:(fun formatter zpad ->
                List.fold ~init:formatter [0L; 40L] ~f:(fun formatter width ->
                  List.fold ~init:formatter [2L; 12L; 13L] ~f:(fun formatter precision ->
                    List.fold ~init:formatter [Fmt.Normalized; Fmt.RadixPoint; Fmt.Compact]
                      ~f:(fun formatter notation ->
                        List.fold ~init:formatter [Fmt.Left; Fmt.Center; Fmt.Right]
                          ~f:(fun formatter just ->
                            formatter
                            |> String.fmt "["
                            |> String.fmt ~pad:(Codepoint.of_char '_') ~width:41L (
                              String.Fmt.empty
                              |> fmt ~pad:"·" ~just ~sign ~alt ~zpad ~width ~precision ~notation
                                ~base x
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
                            |> (match width with 0L -> String.fmt "" | _ -> Uns.fmt width)
                            |> (match precision with
                              | 2L -> String.fmt ""
                              | _ -> (fun formatter ->
                                formatter
                                |> Fmt.fmt "."
                                |> Uns.fmt precision
                              )
                            )
                            |> String.fmt (
                              match base with
                              | Fmt.Bin -> "b"
                              | Fmt.Oct -> "o"
                              | Fmt.Dec -> "d"
                              | Fmt.Hex -> "h"
                            )
                            |> String.fmt (
                              match notation with
                              | Fmt.Normalized -> "m"
                              | Fmt.RadixPoint -> "a"
                              | Fmt.Compact -> "c"
                            )
                            |> String.fmt "r\n"
                          )
                      )
                  )
                )
              )
            )
          )
      )
      |> fn xs'
  in
  let output =
    String.Fmt.empty |>
    fn [
      -1.;
      -0.;
      0.;
      0x1p-1022;
      (0x1p-1022 * 0x1p-52);
      (0x1p-1022 * (1. - 0x1p-52));
      (8. / 9.);
      1.;
      0x1.0000_0000_0000_1;
      2.875;
      9.999;
      (0x1p1023 * (1. + (1. - 0x1p-52)));
    ]
    |> Fmt.to_string
  in
  File.Fmt.stdout
  |> Fmt.fmt (match verbose with true -> output | false -> "")
  |> String.fmt (U128.to_string ~alt:true ~zpad:true ~width:32L ~base:Fmt.Hex
      (Hash.State.empty |> String.hash_fold output |> Hash.t_of_state))

let _ = test ()
