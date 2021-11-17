open! Basis.Rudiments
open! Basis
open I8

let test () =
  let rec fn = function
    | [] -> ()
    | x :: xs' -> begin
        let i = x in
        let t = trunc_of_sint i in
        let i' = extend_to_sint t in
        let t' = trunc_of_sint i' in
        File.Fmt.stdout
        |> Fmt.fmt "trunc_of_sint "
        |> Sint.fmt ~alt:true ~zpad:true ~width:16L ~base:Fmt.Hex ~pretty:true i
        |> Fmt.fmt " -> extend_to_sint "
        |> fmt ~alt:true ~zpad:true ~width:2L ~base:Fmt.Hex ~pretty:true t
        |> Fmt.fmt " -> trunc_of_sint "
        |> Sint.fmt ~pretty:true i'
        |> Fmt.fmt " -> "
        |> fmt ~alt:true ~zpad:true ~width:2L ~base:Fmt.Hex ~pretty:true t'
        |> Fmt.fmt "\n"
        |> ignore;
        let t = trunc_of_uns (Uns.bits_of_sint i) in
        let u = extend_to_uns t in
        let t' = trunc_of_uns u in
        File.Fmt.stdout
        |> Fmt.fmt "trunc_of_uns "
        |> Sint.fmt ~alt:true ~zpad:true ~width:16L ~base:Fmt.Hex ~pretty:true i
        |> Fmt.fmt " -> extend_to_uns "
        |> fmt ~alt:true ~zpad:true ~width:2L ~base:Fmt.Hex ~pretty:true t
        |> Fmt.fmt " -> trunc_of_uns "
        |> Uns.fmt ~alt:true ~zpad:true ~width:16L ~base:Fmt.Hex u
        |> Fmt.fmt " -> "
        |> fmt ~alt:true ~zpad:true ~width:2L ~base:Fmt.Hex ~pretty:true t'
        |> Fmt.fmt "\n"
        |> ignore;
        fn xs'
      end
  in
  fn [Uns.max_value; (-2L); (-1L); 0L; 1L; 2L; Uns.bits_of_sint Sint.max_value]

let _ = test ()
