open! Basis.Rudiments
open! Basis
open Deq

let test () =
  let rec fn i n t = begin
    match i <= n with
    | false -> ()
    | true -> begin
        let t' = push_back i t in
        let elm = hd t' in
        File.Fmt.stdout
        |> Fmt.fmt "hd "
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
