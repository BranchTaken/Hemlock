open! Basis.Rudiments
open! Basis
open Real

let test () =
  let rec fn xys = begin
    match xys with
    | [] -> ()
    | (x, y) :: xys' -> begin
        File.Fmt.stdout
        |> Fmt.fmt "x="
        |> pp x
        |> Fmt.fmt " y="
        |> pp y
        |> Fmt.fmt ": hypot="
        |> pp (hypot x y)
        |> Fmt.fmt "\n"
        |> ignore;
        fn xys'
      end
  end in
  fn [(3., 4.); (4., 3.); (-3., -4.);
    (-0., -3.); (0., -3.);
    (3., inf); (nan, inf); (neg_inf, nan);
  ]

let _ = test ()
