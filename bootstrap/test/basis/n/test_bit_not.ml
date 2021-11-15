open! Basis.Rudiments
open! Basis
open N

let test () =
  let rec test = function
    | [] -> ()
    | x :: xs' -> begin
        File.Fmt.stdout
        |> Fmt.fmt "bit_not "
        |> fmt ~alt:true ~base:Fmt.Hex x
        |> Fmt.fmt " -> "
        |> fmt ~alt:true ~base:Fmt.Hex (bit_not x)
        |> Fmt.fmt "\n"
        |> ignore;
        test xs'
      end
  in
  let xs = [
    zero; (* No bits, therefore the bitwise not is also no bits. *)
    one;
    of_u64 U64.max_value
  ] in
  test xs

let _ = test ()
