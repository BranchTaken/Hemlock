open! Basis.Rudiments
open! Basis
open Real

let test () =
  let rec fn = function
    | [] -> ()
    | t :: ts' -> begin
        let sin_t = sin t in
        let cos_t = cos t in
        let tan_t = tan t in
        File.Fmt.stdout
        |> Fmt.fmt "sin cos tan "
        |> fmt ~pmode:Fmt.Fixed ~precision:5L ~notation:Fmt.RadixPoint t
        |> Fmt.fmt " -> "
        |> fmt ~pmode:Fmt.Fixed ~precision:5L ~notation:Fmt.RadixPoint sin_t
        |> Fmt.fmt " "
        |> fmt ~pmode:Fmt.Fixed ~precision:5L ~notation:Fmt.RadixPoint cos_t
        |> Fmt.fmt " "
        |> fmt ~pmode:Fmt.Fixed ~precision:5L ~notation:Fmt.RadixPoint tan_t
        |> Fmt.fmt "\n"
        |> Fmt.fmt "asin acos atan atan2 -> "
        |> fmt ~pmode:Fmt.Fixed ~precision:5L ~notation:Fmt.RadixPoint (asin sin_t)
        |> Fmt.fmt " "
        |> fmt ~pmode:Fmt.Fixed ~precision:5L ~notation:Fmt.RadixPoint (acos cos_t)
        |> Fmt.fmt " "
        |> fmt ~pmode:Fmt.Fixed ~precision:5L ~notation:Fmt.RadixPoint (atan tan_t)
        |> Fmt.fmt " "
        |> fmt ~pmode:Fmt.Fixed ~precision:5L ~notation:Fmt.RadixPoint (atan2 sin_t cos_t)
        |> Fmt.fmt "\n"
        |> ignore;
        fn ts'
      end
  in
  fn [
    0.; (pi / 6.); (pi / 4.); (2./3. * pi); pi; (4./3. * pi); (2. * pi);
  ]

let _ = test ()
