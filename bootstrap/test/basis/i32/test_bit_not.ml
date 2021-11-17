open! Basis.Rudiments
open! Basis
open I32

let test () =
  let rec test = function
    | [] -> ()
    | x :: xs' -> begin
        File.Fmt.stdout
        |> Fmt.fmt "bit_not "
        |> fmt ~alt:true ~zpad:true ~width:8L ~base:Fmt.Hex ~pretty:true x
        |> Fmt.fmt " -> "
        |> fmt ~alt:true ~zpad:true ~width:8L ~base:Fmt.Hex ~pretty:true (bit_not x)
        |> Fmt.fmt "\n"
        |> ignore;
        test xs'
      end
  in
  let xs = [
    kv 0L;
    kv 0xffff_ffffL
  ] in
  test xs

let _ = test ()
