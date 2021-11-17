open! Basis.Rudiments
open! Basis
open Stream

let test () =
  let rec test_hd_up_to i n = begin
    match i <= n with
    | false -> ()
    | true -> begin
        let t = init (0L =:< i) ~f:(fun i -> i) in
        let elm = hd t in
        File.Fmt.stdout
        |> Fmt.fmt "hd "
        |> (pp Uns.pp) t
        |> Fmt.fmt " = "
        |> Uns.pp elm
        |> Fmt.fmt "\n"
        |> ignore;
        test_hd_up_to (succ i) n
      end
  end in
  (* halts if we start at 0 *)
  test_hd_up_to 1L 4L

let _ = test ()
