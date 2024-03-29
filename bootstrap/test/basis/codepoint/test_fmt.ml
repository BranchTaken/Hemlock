open! Basis.Rudiments
open! Basis
open Codepoint

let test () =
  let rec fn i = begin
    match i with
    | 0x80L -> ()
    | _ -> begin
        let cp = trunc_of_uns i in
        File.Fmt.stdout
        |> Uns.fmt ~alt:true ~zpad:true ~width:2L ~radix:Radix.Hex i
        |> Fmt.fmt " -> "
        |> pp cp
        |> Fmt.fmt " "
        |> Fmt.fmt (match Uns.(i > 0x1fL && i < 0x7fL) with
          | true -> (String.Fmt.empty |> Fmt.fmt "'" |> fmt cp |> Fmt.fmt "'" |> Fmt.to_string)
          | false -> "_"
        )
        |> Fmt.fmt "\n"
        |> ignore;
        fn (Uns.succ i)
      end
  end in
  fn 0L

let _ = test ()
