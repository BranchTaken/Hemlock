open! Basis.Rudiments
open! Basis
open Deq

let test () =
  let rec fn i n t = begin
    match i <= n with
    | false -> ()
    | true -> begin
        let t' = push_back i t in
        File.Fmt.stdout
        |> Fmt.fmt "push_back "
        |> Uns.pp i
        |> Fmt.fmt " "
        |> (pp Uns.pp) t
        |> Fmt.fmt " = "
        |> (pp Uns.pp) t'
        |> Fmt.fmt "\n"
        |> ignore;
        fn (succ i) n t'
      end
  end in
  fn 0L 4L empty

let _ = test ()
