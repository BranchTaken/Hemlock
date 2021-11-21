open! Basis.Rudiments
open! Basis
open Real

let test () =
  let rec fn ts = begin
    match ts with
    | [] -> ()
    | t :: ts' -> begin
        File.Fmt.stdout
        |> fmt ~alt:true ~base:Fmt.Hex t
        |> Fmt.fmt " -> "
        |> Class.pp (classify t)
        |> Fmt.fmt "\n"
        |> ignore;
        fn ts'
      end
  end in
  fn [
    inf; -inf;
    nan;
    -1.; 1.;
    0x0.0000000000001p-1022; 0x0.fffffffffffffp-1022;
    -0.; 0.;
  ]

let _ = test ()
