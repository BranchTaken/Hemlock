open! Basis.Rudiments
open! Basis
open Real

let test () =
  let rec fn xs = begin
    match xs with
    | [] -> ()
    | x :: xs' -> begin
        File.Fmt.stdout
        |> Fmt.fmt "gamma "
        |> fmt ~pmode:Fmt.Fixed ~precision:1L ~notation:Fmt.RadixPoint x
        |> Fmt.fmt " -> "
        |> fmt ~pmode:Fmt.Fixed ~precision:5L ~notation:Fmt.Normalized (gamma x)
        |> Fmt.fmt "\n"
        |> ignore;
        fn xs'
      end
  end in
  fn [-1.; -0.; 0.; 0.5; 10.; 171.6; 171.7; inf; nan]

let _ = test ()
