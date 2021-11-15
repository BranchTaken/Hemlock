open! Basis.Rudiments
open! Basis
open Array

let test () =
  let test arr = begin
    File.Fmt.stdout
    |> Fmt.fmt "pare "
    |> (pp Uns.pp) arr
    |> Fmt.fmt " ->"
    |> ignore;
    let rec fni i n = begin
      match i > n with
      | true -> ()
      | false -> begin
          let rec fnj j n = begin
            match j > n with
            | true -> ()
            | false -> begin
                let arr' = pare (i =:< j) arr in
                File.Fmt.stdout
                |> Fmt.fmt " ["
                |> Uns.pp i
                |> Fmt.fmt ","
                |> Uns.pp j
                |> Fmt.fmt ")="
                |> (pp Uns.pp) arr'
                |> ignore;
                fnj (succ j) n
              end
          end in
          fnj i n;
          fni (succ i) n
        end
    end in
    fni 0L (length arr);
    File.Fmt.stdout |> Fmt.fmt "\n" |> ignore
  end in
  test [||];
  test [|0L|];
  test [|0L; 1L|];
  test [|0L; 1L; 2L|]

let _ = test ()
