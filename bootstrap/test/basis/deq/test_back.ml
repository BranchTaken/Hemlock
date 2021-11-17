open! Basis.Rudiments
open! Basis
open Deq

let test () =
  let rec fn i n t = begin
    match i <= n with
    | false -> ()
    | true -> begin
        let t' = push i t in
        let elm = back t' in
        File.Fmt.stdout
        |> Fmt.fmt "back "
        |> (pp Uns.pp) t'
        |> Fmt.fmt " = "
        |> Uns.pp elm
        |> Fmt.fmt "\n"
        |> ignore;
        fn (succ i) n t'
      end
  end in
  fn 0L 4L empty

let _ = test ()
