open! Basis.Rudiments
open! Basis
open Array
open Format

let test () =
  let test arr = begin
    printf "remove %a ->" (xpp Uns.xpp) arr;
    let rec fn i n = begin
      match i >= n with
      | true -> ()
      | false -> begin
          let arr' = remove i arr in
          printf " %a" (xpp Uns.xpp) arr';
          fn (succ i) n
        end
    end in
    fn 0L (length arr);
    printf "\n"
  end in
  printf "@[<h>";
  test [|0L|];
  test [|0L; 1L|];
  test [|0L; 1L; 2L|];
  printf "@]"

let _ = test ()
