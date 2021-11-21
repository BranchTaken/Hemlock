open! Basis.Rudiments
open! Basis
open Real

let test () =
  let norm_nan t = if (is_nan t) then nan else t in
  let rec fn xs = begin
    match xs with
    | [] -> ()
    | x :: xs' -> begin
        File.Fmt.stdout
        |> fmt ~pmode:Fmt.Fixed ~precision:3L ~notation:Fmt.RadixPoint x
        |> Fmt.fmt ": sqrt="
        |> fmt ~pmode:Fmt.Fixed ~precision:6L ~notation:Fmt.RadixPoint (norm_nan (sqrt x))
        |> Fmt.fmt " cbrt="
        |> fmt ~pmode:Fmt.Fixed ~precision:6L ~notation:Fmt.RadixPoint (norm_nan (cbrt x))
        |> Fmt.fmt "\n"
        |> ignore;
        fn xs'
      end
  end in
  fn [-0.125; 0.; inf; 1.; 4.; 27.; 64.; 65.; 729.]

let _ = test ()
