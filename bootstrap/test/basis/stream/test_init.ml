open! Basis.Rudiments
open! Basis
open Stream

let test () =
  let rec test_init_up_to i n = begin
    match i <= n with
    | false -> ()
    | true -> begin
        let t = init (0L =:< i) ~f:(fun i -> i) in
        File.Fmt.stdout
        |> Fmt.fmt "init "
        |> Uns.pp i
        |> Fmt.fmt " ~f:(fun i -> i) = "
        |> (pp Uns.pp) t
        |> Fmt.fmt "\n"
        |> ignore;
        test_init_up_to (succ i) n
      end
  end in
  test_init_up_to 0L 3L

let _ = test ()
