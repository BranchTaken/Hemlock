open! Basis.Rudiments
open! Basis
open I16

let test () =
  let rec test = function
    | [] -> ()
    | x :: xs' -> begin
        File.Fmt.stdout
        |> Fmt.fmt "bit_{pop,clz,ctz} "
        |> fmt ~alt:true ~zpad:true ~width:4L ~radix:Radix.Hex ~pretty:true x
        |> Fmt.fmt " -> "
        |> Uns.pp (bit_pop x)
        |> Fmt.fmt ", "
        |> Uns.pp (bit_clz x)
        |> Fmt.fmt ", "
        |> Uns.pp (bit_ctz x)
        |> Fmt.fmt "\n"
        |> ignore;
        test xs'
      end
  in
  let xs = [
    kv (-0x8000L);
    kv (-1L);
    kv 0L;
    kv 1L;
  ] in
  test xs

let _ = test ()
