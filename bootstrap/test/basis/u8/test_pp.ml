open! Basis.Rudiments
open! Basis
open U8

let test () =
  let rec print_xs xs = begin
    match xs with
    | [] -> ()
    | x :: xs' -> begin
        File.Fmt.stdout
        |> fmt ~alt:true ~zpad:true ~width:8L ~base:Fmt.Bin ~pretty:true x
        |> Fmt.fmt ", "
        |> fmt ~alt:true ~zpad:true ~width:3L ~base:Fmt.Oct ~pretty:true x
        |> Fmt.fmt ", "
        |> pp x
        |> Fmt.fmt ", "
        |> fmt ~alt:true ~zpad:true ~width:2L ~base:Fmt.Hex ~pretty:true x
        |> Fmt.fmt "\n"
        |> ignore;
        print_xs xs'
      end
  end in
  print_xs [
    min_value;
    kv 42L;
    max_value;
  ]

let test2 () =
  let rec fn = function
    | [] -> ()
    | x :: xs' -> begin
        File.Fmt.stdout
        |> pp x
        |> Fmt.fmt " "
        |> fmt ~alt:true ~zpad:true ~width:2L ~base:Fmt.Hex ~pretty:true x
        |> Fmt.fmt "\n"
        |> ignore;
        fn xs'
      end
  in
  fn [kv 0L; kv 1L; kv 42L; kv 255L]

let _ = test ()
let _ = test2 ()
