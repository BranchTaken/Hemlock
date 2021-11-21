open! Basis.Rudiments
open! Basis
open Real

let test () =
  let rec fn ts = begin
    match ts with
    | [] -> ()
    | t :: ts' -> begin
        File.Fmt.stdout
        |> Fmt.fmt "ex "
        |> fmt ~alt:true ~base:Fmt.Hex t
        |> Fmt.fmt " -> "
        |> fmt ~alt:true ~base:Fmt.Hex (ex t)
        |> Fmt.fmt "\n"
        |> ignore;
        fn ts'
      end
  end in
  fn [
    inf; -inf;
    nan;
    -1.; 1.;
    -0.; 0.;
  ]

let _ = test ()
