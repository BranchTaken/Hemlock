open! Basis.Rudiments
open! Basis
open I64

let test () =
  let rec test_xs xs = begin
    match xs with
    | [] -> ()
    | x :: xs' -> begin
        let sx = x in
        File.Fmt.stdout
        |> Fmt.fmt "to_sint "
        |> fmt ~alt:true ~zpad:true ~width:16L ~base:Fmt.Hex ~pretty:true x
        |> Fmt.fmt " -> "
        |> Sint.fmt ~alt:true ~zpad:true ~width:16L ~base:Fmt.Hex ~pretty:true x
        |> Fmt.fmt "; of_sint -> "
        |> fmt ~alt:true ~zpad:true ~width:16L ~base:Fmt.Hex ~pretty:true sx
        |> Fmt.fmt "\n"
        |> ignore;
        test_xs xs'
      end
  end in
  let xs = [
    zero;
    one;
    neg_one;
    Sint.min_value;
    Sint.max_value;
    min_value;
    max_value;
  ] in
  test_xs xs

let _ = test ()
