open! Basis.Rudiments
open! Basis
open Array
open Format

let test () =
  let test_search arr key_max = begin
    printf "%a\n" (pp Uns.pp) arr;
    Range.iter (0L =:< (succ key_max)) ~f:(fun probe ->
      let open Cmp in
      printf "  %a -> %s, %s, %s\n" Uns.pp probe
        (match psearch probe ~cmp:Uns.cmp arr with
          | None -> "<"
          | Some (Lt, i) -> asprintf "<[%a]=%a" Uns.pp i Uns.pp (get i arr)
          | Some (Eq, i) -> asprintf "=[%a]=%a" Uns.pp i Uns.pp (get i arr)
          | Some (Gt, i) -> asprintf ">[%a]=%a" Uns.pp i Uns.pp (get i arr)
        )
        (match search probe ~cmp:Uns.cmp arr with
          | None -> "<>"
          | Some i -> asprintf "=%a" Uns.pp (get i arr)
        )
        (match nsearch probe ~cmp:Uns.cmp arr with
          | Some (Lt, i) -> asprintf "<[%a]=%a" Uns.pp i Uns.pp (get i arr)
          | Some (Eq, i) -> asprintf "=[%a]=%a" Uns.pp i Uns.pp (get i arr)
          | Some (Gt, i) -> asprintf ">[%a]=%a" Uns.pp i Uns.pp (get i arr)
          | None -> ">"
        )
    )
  end in
  printf "@[<h>";
  Range.iter (0L =:< 4L) ~f:(fun len ->
    let arr = init (0L =:< len) ~f:(fun i -> i * 2L + 1L) in
    let key_max = len * 2L in
    test_search arr key_max
  );
  Range.iter (1L =:< 4L) ~f:(fun hlen ->
    let len = hlen * 2L in
    let arr = init (0L =:< len) ~f:(fun i -> i + ((i + 1L) % 2L)) in
    let key_max = len in
    test_search arr key_max
  );
  printf "@]"

let _ = test ()
