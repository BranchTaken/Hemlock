open! Basis.Rudiments
open! Basis
open U32

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
        |> fmt ~alt:true ~zpad:true ~width:8L ~base:Fmt.Hex ~pretty:true t
        |> Fmt.fmt " -> trunc_of_sint "
        |> Sint.fmt ~alt:true ~zpad:true ~width:16L ~base:Fmt.Hex ~pretty:true i'
        |> Fmt.fmt " -> "
        |> fmt ~alt:true ~zpad:true ~width:8L ~base:Fmt.Hex ~pretty:true t'
        |> Fmt.fmt "\n"
        |> ignore;
        let t = trunc_of_uns (Uns.bits_of_sint i) in
        let u = extend_to_uns t in
        let t' = trunc_of_uns u in
        File.Fmt.stdout
        |> Fmt.fmt "trunc_of_uns "
        |> Uns.fmt ~alt:true ~zpad:true ~width:16L ~base:Fmt.Hex x
        |> Fmt.fmt " -> extend_to_uns "
        |> fmt ~alt:true ~zpad:true ~width:8L ~base:Fmt.Hex ~pretty:true t
        |> Fmt.fmt " -> trunc_of_uns "
        |> Uns.fmt ~alt:true ~zpad:true ~width:16L ~base:Fmt.Hex u
        |> Fmt.fmt " -> "
        |> fmt ~alt:true ~zpad:true ~width:8L ~base:Fmt.Hex ~pretty:true t'
        |> Fmt.fmt "\n"
        |> ignore;
        fn xs'
      end
  in
  fn [Uns.max_value; 0L; 42L; 0x1f_ffffL; 0x20_0000L; 0x20_0001L; Uns.bits_of_sint Sint.max_value]

let _ = test ()
