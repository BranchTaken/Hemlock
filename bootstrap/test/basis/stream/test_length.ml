open! Basis.Rudiments
open! Basis
open Stream

let test () =
  let rec test_length_up_to i n = begin
    match i <= n with
    | false -> ()
    | true -> begin
        let t = init (0L =:< i) ~f:(fun i -> i) in
        let l = length t in
        File.Fmt.stdout
        |> Fmt.fmt "length "
        |> (pp Uns.pp) t
        |> Fmt.fmt " = "
        |> Uns.pp l
        |> Fmt.fmt "\n"
        |> ignore;
        test_length_up_to (succ i) n
      end
  end in
  test_length_up_to 0L 3L

let _ = test ()
