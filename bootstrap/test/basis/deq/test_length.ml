open! Basis.Rudiments
open! Basis
open Deq

let test () =
  let rec fn i n t = begin
    match i <= n with
    | false -> ()
    | true -> begin
        let l = length t in
        File.Fmt.stdout
        |> Fmt.fmt "length "
        |> (pp Uns.pp) t
        |> Fmt.fmt " = "
        |> Uns.pp l
        |> Fmt.fmt "\n"
        |> ignore;
        fn (succ i) n (push i t)
      end
  end in
  fn 0L 3L empty

let _ = test ()
