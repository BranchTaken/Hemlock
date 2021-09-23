open! Basis.Rudiments
open! Basis
open Array
open Format

let test () =
  let test arr x = begin
    printf "insert %a %a ->"
      (pp Uns.pp) arr
      Uns.pp x
    ;
    let rec fn i n = begin
      match i > n with
      | true -> ()
      | false -> begin
          let arr' = insert i x arr in
          printf " %a" (pp Uns.pp) arr';
          fn (succ i) n
        end
    end in
    fn 0L (length arr);
    printf "\n"
  end in
  printf "@[<h>";
  test [||] 0L;
  test [|0L|] 1L;
  test [|0L; 1L|] 2L;
  test [|0L; 1L; 2L|] 3L;
  printf "@]"

let _ = test ()
