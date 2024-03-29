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
      ) [Radix.Dec] ~f:(fun formatter radix ->
        List.fold ~init:formatter [Fmt.Implicit; Fmt.Explicit; Fmt.Space] ~f:(fun formatter sign ->
          List.fold ~init:formatter [false; true] ~f:(fun formatter alt ->
            List.fold ~init:formatter [false; true] ~f:(fun formatter zpad ->
              List.fold ~init:formatter [0L; 40L] ~f:(fun formatter width ->
                List.fold ~init:formatter [Fmt.Limited; Fmt.Fixed] ~f:(fun formatter pmode ->
                  List.fold ~init:formatter [2L; 12L; 13L] ~f:(fun formatter precision ->
                    List.fold ~init:formatter [Fmt.Normalized; Fmt.RadixPoint; Fmt.Compact]
                      ~f:(fun formatter notation ->
                        List.fold ~init:formatter [Fmt.Left; Fmt.Center; Fmt.Right]
                          ~f:(fun formatter just ->
                            formatter
                            |> Fmt.fmt "["
                            |> Fmt.fmt ~pad:"_" ~width:41L (
                              String.Fmt.empty
                              |> fmt ~pad:"·" ~just ~sign ~alt ~zpad ~width ~pmode ~precision
                                ~notation ~radix x
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
                            |> (match width with 0L -> Fmt.fmt "" | _ -> Uns.fmt width)
                            |> (match pmode, precision with
                              | Fmt.Limited, 3L -> Fmt.fmt ""
                              | _ -> (fun formatter ->
                                formatter
                                |> Fmt.fmt "."
                                |> Fmt.fmt (match pmode with
                                  | Fmt.Limited -> ""
                                  | Fmt.Fixed -> "="
                                )
                                |> Uns.fmt precision
                              )
                            )
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
  |> Fmt.fmt (U128.to_string ~alt:true ~zpad:true ~width:32L ~radix:Radix.Hex ~pretty:true
      (Hash.State.empty |> String.hash_fold output |> Hash.t_of_state))

let _ = test ()
