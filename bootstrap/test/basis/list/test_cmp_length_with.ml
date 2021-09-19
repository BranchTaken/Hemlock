open! Basis.Rudiments
open! Basis
open List
open Format

let test () =
  let test_cmp_length_with lst limit = begin
    printf " (limit=%a -> %s)"
      Uns.pp limit (match cmp_length_with lst limit with
      | Cmp.Lt -> "Lt"
      | Cmp.Eq -> "Eq"
      | Cmp.Gt -> "Gt"
    )
  end in
  let rec test_with_lists lists = begin
    match lists with
    | [] -> ()
    | list :: lists' -> begin
        printf "cmp_length_with %a" (pp Uns.pp) list;
        for limit = 0 to 3 do
          printf "%s" (if limit = 0 then ": " else ", ");
          test_cmp_length_with list limit;
        done;
        printf "\n";
        test_with_lists lists'
      end
  end in
  let lists = [
    [];
    [0];
    [0; 1];
    [0; 1; 2]
  ] in
  printf "@[<h>";
  test_with_lists lists;
  printf "@]"

let _ = test ()
