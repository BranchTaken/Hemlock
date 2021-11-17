open! Basis.Rudiments
open! Basis
open Stream

let test () =
  let rec test_push_up_to t i n = begin
    match i <= n with
    | false -> ()
    | true -> begin
        let t' = push i t in
        File.Fmt.stdout
        |> Fmt.fmt "push "
        |> Uns.pp i
        |> Fmt.fmt " "
        |> (pp Uns.pp) t
        |> Fmt.fmt " = "
        |> (pp Uns.pp) t'
        |> Fmt.fmt "\n"
        |> ignore;
        test_push_up_to t' (succ i) n
      end
  end in
  test_push_up_to empty 0L 3L

let _ = test ()
