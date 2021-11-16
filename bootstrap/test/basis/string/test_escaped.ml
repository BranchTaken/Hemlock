open! Basis.Rudiments
open! Basis
open String

let test () =
  let rec fn i = begin
    match i with
    | 0x80L -> ()
    | _ -> begin
        File.Fmt.stdout
        |> Uns.fmt ~alt:true ~zpad:true ~width:2L ~base:Basis.Fmt.Hex i
        |> Basis.Fmt.fmt " -> \""
        |> fmt (escaped (of_codepoint Codepoint.(trunc_of_uns i)))
        |> Basis.Fmt.fmt "\"\n"
        |> ignore;
        fn (Uns.succ i)
      end
  end in
  fn 0L

let _ = test ()
