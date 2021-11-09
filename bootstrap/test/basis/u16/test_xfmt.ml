open! Basis.Rudiments
open! Basis
open U16
let test () =
  let rec fn = function
    | [] -> ()
    | x :: xs' -> begin
        let _ =
          File.Fmt.stdout
          |> fmt x
          |> Fmt.fmt " "
          |> xfmt ~alt:true ~zpad:true ~width:4L ~base:Fmt.Hex x
          |> Fmt.fmt "\n"
        in
        fn xs'
      end
  in
  fn [kv 0L; kv 1L; kv 42L; kv 0x1fffL]

let _ = test ()
