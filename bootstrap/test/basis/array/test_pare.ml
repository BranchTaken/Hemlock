open! Basis.Rudiments
open! Basis
open Array
open Format

let test () =
  let test arr = begin
    printf "pare %a ->" (pp Uns.pp) arr;
    let rec fni i n = begin
      match i > n with
      | true -> ()
      | false -> begin
          let rec fnj j n = begin
            match j > n with
            | true -> ()
            | false -> begin
                let arr' = pare (i =:< j) arr in
                printf " [%a,%a)=%a"
                  Uns.pp i
                  Uns.pp j
                  (pp Uns.pp) arr'
                ;
                fnj (succ j) n
              end
          end in
          fnj i n;
          fni (succ i) n
        end
    end in
    fni 0L (length arr);
    printf "\n"
  end in
  printf "@[<h>";
  test [||];
  test [|0L|];
  test [|0L; 1L|];
  test [|0L; 1L; 2L|];
  printf "@]"

let _ = test ()
