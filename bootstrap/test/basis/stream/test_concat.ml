open! Basis.Rudiments
open! Basis
open Stream

let test () =
  let rec test_concat_up_to i l n = begin
    match i <= l, l <= n with
    | _, false -> ()
    | false, _ -> test_concat_up_to 0L (succ l) n
    | true, true -> begin
        let t0 = init (0L =:< i) ~f:(fun i -> i) in
        let t1 = init (i =:< l) ~f:(fun j -> j) in
        let t = concat t0 t1 in
        File.Fmt.stdout
        |> Fmt.fmt "concat "
        |> (pp Uns.pp) t0
        |> Fmt.fmt " "
        |> (pp Uns.pp) t1
        |> Fmt.fmt " = "
        |> (pp Uns.pp) t
        |> Fmt.fmt "\n"
        |> ignore;
        test_concat_up_to (succ i) l n
      end
  end in
  test_concat_up_to 0L 0L 3L

let _ = test ()
