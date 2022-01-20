open! Basis.Rudiments
open! Basis
open U32

let test () =
  let rec fn = function
    | [] -> ()
    | x :: xs' -> begin
        File.Fmt.stdout
        |> pp x
        |> Fmt.fmt " "
        |> fmt ~alt:true ~zpad:true ~width:8L ~radix:Radix.Hex ~pretty:true x
        |> Fmt.fmt "\n"
        |> ignore;
        fn xs'
      end
  in
  fn [kv 0L; kv 1L; kv 42L; kv 0x1_ffff_ffffL]

let _ = test ()
