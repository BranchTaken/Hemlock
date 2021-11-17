open! Basis.Rudiments
open! Basis
open Stream

let test () =
  let rec test_tl_up_to i n = begin
    match i <= n with
    | false -> ()
    | true -> begin
        let t = init (0L =:< i) ~f:(fun i -> i) in
        let t' = tl t in
        File.Fmt.stdout
        |> Fmt.fmt "tl "
        |> (pp Uns.pp) t
        |> Fmt.fmt " = "
        |> (pp Uns.pp) t'
        |> Fmt.fmt "\n"
        |> ignore;
        test_tl_up_to (succ i) n
      end
  end in
  (* halts if we start at 0 *)
  test_tl_up_to 1L 4L

let _ = test ()
