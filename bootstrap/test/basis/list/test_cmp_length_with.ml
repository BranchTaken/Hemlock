open! Basis.Rudiments
open! Basis
open List
open Format

let test () =
  let test_cmp_length_with lst limit = begin
    printf " (limit=%a -> %s)"
      Uns.xpp limit (match cmp_length_with lst limit with
      | Cmp.Lt -> "Lt"
      | Cmp.Eq -> "Eq"
      | Cmp.Gt -> "Gt"
    )
  end in
  let rec test_with_lists lists = begin
    match lists with
    | [] -> ()
    | list :: lists' -> begin
        printf "cmp_length_with %a" (xpp Uns.xpp) list;
        Range.iter (0L =:< 4L) ~f:(fun limit ->
          printf "%s" (if limit = 0L then ": " else ", ");
          test_cmp_length_with list limit;
        );
        printf "\n";
        test_with_lists lists'
      end
  end in
  let lists = [
    [];
    [0L];
    [0L; 1L];
    [0L; 1L; 2L]
  ] in
  printf "@[<h>";
  test_with_lists lists;
  printf "@]"

let _ = test ()
