open! Basis.Rudiments
open! Basis
open Array

let test () =
  let test_set len = begin
    let rec fn i n = begin
      match i < n with
      | false -> ()
      | true -> begin
          let arr = init (0L =:< len) ~f:(fun _ -> 0L) in
          let arr' = set i 1L arr in
          File.Fmt.stdout
          |> Fmt.fmt "set "
          |> Uns.pp i
          |> Fmt.fmt ": "
          |> (pp Uns.pp) arr
          |> Fmt.fmt " -> "
          |> (pp Uns.pp) arr'
          |> ignore;
          set_inplace i 1L arr;
          File.Fmt.stdout
          |> Fmt.fmt " -> set_inplace: "
          |> (pp Uns.pp) arr
          |> ignore;
          let arr'' = copy arr in
          File.Fmt.stdout
          |> Fmt.fmt " -> copy,set_inplace: "
          |> (pp Uns.pp) arr''
          |> ignore;
          set_inplace i 2L arr'';
          File.Fmt.stdout
          |> Fmt.fmt " -> "
          |> (pp Uns.pp) arr''
          |> Fmt.fmt "\n"
          |> ignore;
          fn (succ i) n
        end
    end in
    fn 0L len
  end in
  test_set 1L;
  test_set 2L;
  test_set 3L

let _ = test ()
