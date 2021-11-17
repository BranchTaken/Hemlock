open! Basis.Rudiments
open! Basis
open Stream

let test () =
  let rec test_init_indef_up_to i n = begin
    match i <= n with
    | false -> ()
    | true -> begin
        let f state = begin
          match state < i with
          | false -> None
          | true -> Some(state, succ state)
        end in
        let t = init_indef ~f 0L in
        File.Fmt.stdout
        |> Fmt.fmt "init_indef until "
        |> Uns.pp i
        |> Fmt.fmt " = "
        |> (pp Uns.pp) t
        |> Fmt.fmt "\n"
        |> ignore;
        test_init_indef_up_to (succ i) n
      end
  end in
  test_init_indef_up_to 0L 3L

let _ = test ()
