open! Basis.Rudiments
open! Basis
open Stream

let test () =
  let rec test_pop t = begin
    match t with
    | lazy Nil -> ()
    | _ -> begin
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
        test_pop t'
      end
  end in
  test_pop (init (0L =:< 3L) ~f:(fun i -> i))

let _ = test ()
