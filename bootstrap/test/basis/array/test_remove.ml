open! Basis.Rudiments
open! Basis
open Array

let test () =
  let test arr = begin
    File.Fmt.stdout
    |> Fmt.fmt "remove "
    |> (pp Uns.pp) arr
    |> Fmt.fmt " ->"
    |> ignore;
    let rec fn i n = begin
      match i >= n with
      | true -> ()
      | false -> begin
          let arr' = remove i arr in
          File.Fmt.stdout
          |> Fmt.fmt " "
          |> (pp Uns.pp) arr'
          |> ignore;
          fn (succ i) n
        end
    end in
    fn 0L (length arr);
    File.Fmt.stdout |> Fmt.fmt "\n" |> ignore
  end in
  test [|0L|];
  test [|0L; 1L|];
  test [|0L; 1L; 2L|]

let _ = test ()
