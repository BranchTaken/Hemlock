open! Basis.Rudiments
open! Basis
open Array
open Format

let test () =
  let test_search arr key_max = begin
    printf "%a\n" (pp Uns.pp) arr;
    for probe = 0 to key_max do
      printf "  %a -> %s, %s, %s\n" Uns.pp probe
        (match psearch probe ~cmp:Uns.cmp arr with
          | None -> "<"
          | Some (Cmp.Lt, i) -> asprintf "<[%a]=%a" Uns.pp i Uns.pp (get i arr)
          | Some (Cmp.Eq, i) -> asprintf "=[%a]=%a" Uns.pp i Uns.pp (get i arr)
          | Some (Cmp.Gt, i) -> asprintf ">[%a]=%a" Uns.pp i Uns.pp (get i arr)
        )
        (match search probe ~cmp:Uns.cmp arr with
          | None -> "<>"
          | Some i -> asprintf "=%a" Uns.pp (get i arr)
        )
        (match nsearch probe ~cmp:Uns.cmp arr with
          | Some (Cmp.Lt, i) -> asprintf "<[%a]=%a" Uns.pp i Uns.pp (get i arr)
          | Some (Cmp.Eq, i) -> asprintf "=[%a]=%a" Uns.pp i Uns.pp (get i arr)
          | Some (Cmp.Gt, i) -> asprintf ">[%a]=%a" Uns.pp i Uns.pp (get i arr)
          | None -> ">"
        );
    done
  end in
  printf "@[<h>";
  for len = 0 to 3 do
    let arr = init len ~f:(fun i -> i * 2 + 1) in
    let key_max = len * 2 in
    test_search arr key_max
  done;
  for hlen = 1 to 3 do
    let len = hlen * 2 in
    let arr = init len ~f:(fun i -> i + ((i + 1) % 2)) in
    let key_max = len in
    test_search arr key_max
  done;
  printf "@]"

let _ = test ()
