open! Basis.Rudiments
open! Basis
open Q

let test () =
  let rec fn i n t = begin
    match i <= n with
    | false -> ()
    | true -> begin
        let elm, t' = pop t in
        File.Fmt.stdout
        |> Fmt.fmt "pop "
        |> (pp Uns.pp) t
        |> Fmt.fmt " = "
        |> Uns.pp elm
        |> Fmt.fmt " "
        |> (pp Uns.pp) t'
        |> Fmt.fmt "\n"
        |> ignore;
        fn (succ i) n (push_back i t)
      end
  end in
  (* halts if we start with empty *)
  fn 1L 4L (push_back 0L empty)

let _ = test ()
