open! Basis.Rudiments
open! Basis
open Q

let test () =
  let rec fn i n t = begin
    match i <= n with
    | false -> ()
    | true -> begin
        let e = is_empty t in
        File.Fmt.stdout
        |> Fmt.fmt "is_empty "
        |> (pp Uns.pp) t
        |> Fmt.fmt " = "
        |> Bool.pp e
        |> Fmt.fmt "\n"
        |> ignore;
        fn (succ i) n (push_back i t)
      end
  end in
  fn 0L 3L empty

let _ = test ()
