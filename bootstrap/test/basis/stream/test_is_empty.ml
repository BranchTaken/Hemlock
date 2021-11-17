open! Basis.Rudiments
open! Basis
open Stream

let test () =
  let rec test_is_empty_up_to i n = begin
    match i <= n with
    | false -> ()
    | true -> begin
        let t = init (0L =:< i) ~f:(fun i -> i) in
        let e = is_empty t in
        File.Fmt.stdout
        |> Fmt.fmt "is_empty "
        |> (pp Uns.pp) t
        |> Fmt.fmt " = "
        |> Bool.pp e
        |> Fmt.fmt "\n"
        |> ignore;
        test_is_empty_up_to (succ i) n
      end
  end in
  test_is_empty_up_to 0L 3L

let _ = test ()
