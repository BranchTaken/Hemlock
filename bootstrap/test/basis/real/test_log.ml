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
        |> Fmt.fmt "log "
        |> fmt ~alt:true ~base:Fmt.Hex t
        |> Fmt.fmt " -> "
        |> fmt ~alt:true ~base:Fmt.Hex (norm_nan (log t))
        |> Fmt.fmt "\n"
        |> ignore;
        fn ts'
      end
  end in
  fn [
    inf; -inf;
    nan;
    -1.; 1.;
    10.; 100.;
    -0.; 0.;
  ]

let _ = test ()
