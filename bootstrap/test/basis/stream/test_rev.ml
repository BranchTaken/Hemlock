open! Basis.Rudiments
open! Basis
open Stream

let test () =
  let rec test_rev_up_to i n = begin
    match i <= n with
    | false -> ()
    | true -> begin
        let t = init (0L =:< i) ~f:(fun i -> i) in
        let t' = rev t in
        File.Fmt.stdout
        |> Fmt.fmt "rev "
        |> (pp Uns.pp) t
        |> Fmt.fmt " = "
        |> (pp Uns.pp) t'
        |> Fmt.fmt "\n"
        |> ignore;
        test_rev_up_to (succ i) n
      end
  end in
  test_rev_up_to 0L 3L

let _ = test ()
