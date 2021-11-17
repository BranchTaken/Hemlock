open! Basis.Rudiments
open! Basis
open Stream

let test () =
  let rec test_rev_split_up_to i l n =
    match i <= l, l <= n with
    | _, false -> ()
    | false, _ -> test_rev_split_up_to 0L (succ l) n
    | true, true -> begin
        let t = init (0L =:< l) ~f:(fun i -> i) in
        let t0, t1 = rev_split i t in
        File.Fmt.stdout
        |> Fmt.fmt "rev_split "
        |> Uns.pp i
        |> Fmt.fmt " "
        |> (pp Uns.pp) t
        |> Fmt.fmt " = "
        |> (pp Uns.pp) t0
        |> Fmt.fmt " "
        |> (pp Uns.pp) t1
        |> Fmt.fmt "\n"
        |> ignore;
        test_rev_split_up_to (succ i) l n
      end in
  test_rev_split_up_to 0L 0L 3L

let _ = test ()
