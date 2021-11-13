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
        |> Fmt.fmt "\n"
      ) [Fmt.Bin; Fmt.Oct; Fmt.Hex] ~f:(fun formatter base ->
        List.fold ~init:formatter [Fmt.Implicit; Fmt.Explicit; Fmt.Space]
          ~f:(fun formatter sign ->
            List.fold ~init:formatter [false; true] ~f:(fun formatter alt ->
              List.fold ~init:formatter [false; true] ~f:(fun formatter zpad ->
                List.fold ~init:formatter [0L; 20L] ~f:(fun formatter width ->
                  List.fold ~init:formatter [2L; 3L] ~f:(fun formatter precision ->
                    List.fold ~init:formatter [Fmt.Normalized; Fmt.RadixPoint; Fmt.Compact]
                      ~f:(fun formatter notation ->
                        List.fold ~init:formatter [Fmt.Left; Fmt.Center; Fmt.Right]
                          ~f:(fun formatter just ->
                            formatter
                            |> Fmt.fmt "["
                            |> Fmt.fmt ~pad:"_" ~width:21L (
                              String.Fmt.empty
                              |> fmt ~pad:"·" ~just ~sign ~alt ~zpad ~width ~precision ~notation
                                ~base x
                              |> Fmt.to_string
                            )
                            |> Fmt.fmt "] %'·'"
                            |> Fmt.fmt (
                              match just with
                              | Fmt.Left -> "["
                              | Fmt.Center -> "]["
                              | Fmt.Right -> "]"
                            )
                            |> Fmt.fmt (
                              match sign with
                              | Fmt.Implicit -> ""
                              | Fmt.Explicit -> "+"
                              | Fmt.Space -> "_"
                            )
                            |> Fmt.fmt (match alt with false -> "" | true -> "#")
                            |> Fmt.fmt (match zpad with false -> "" | true -> "0")
                            |> (match width with 0L -> Fmt.fmt "" | _ -> Uns.fmt width)
                            |> (match precision with
                              | 2L -> Fmt.fmt ""
                              | _ -> (fun formatter ->
                                formatter
                                |> Fmt.fmt "."
                                |> Uns.fmt precision
                              )
                            )
                            |> Fmt.fmt (
                              match base with
                              | Fmt.Bin -> "b"
                              | Fmt.Oct -> "o"
                              | Fmt.Dec -> "d"
                              | Fmt.Hex -> "h"
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
      0.;
      0x1p-1022;
      (0x1p-1022 *. 0x1p-52);
      (0x1p-1022 *. (1. - 0x1p-52));
      0x1.fedc_ba98_7654_3p16;
      (0x1p1023 * (1. + (1. - 0x1p-52)));
    ]
    |> Fmt.to_string
  in
  File.Fmt.stdout
  |> Fmt.fmt (match verbose with true -> output | false -> "")
  |> Fmt.fmt (U128.to_string ~alt:true ~zpad:true ~width:32L ~base:Fmt.Hex ~pretty:true
      (Hash.State.empty |> String.hash_fold output |> Hash.t_of_state))

let _ = test ()
