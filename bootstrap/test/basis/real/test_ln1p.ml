open! Basis.Rudiments
open! Basis
open Real

let test () =
  let norm_nan t = if (is_nan t) then nan else t in
  let rec fn ts = begin
    match ts with
    | [] -> ()
    | t :: ts' -> begin
        File.Fmt.stdout
        |> Fmt.fmt "ln,ln1p "
        |> fmt ~alt:true ~base:Fmt.Hex t
        |> Fmt.fmt " -> "
        |> fmt ~alt:true ~base:Fmt.Hex (norm_nan (ln (1. + t)))
        |> Fmt.fmt " "
        |> fmt ~alt:true ~base:Fmt.Hex (norm_nan (ln1p t))
        |> Fmt.fmt "\n"
        |> ignore;
        fn ts'
      end
  end in
  fn [
    inf; -inf;
    nan;
    -2.; -1.; 1.;
    (ex 1.); (ex 2.);
    0x0.0000000000001p-1022; 0x0.fffffffffffffp-1022;
    -0.; 0.;
  ]

let _ = test ()
