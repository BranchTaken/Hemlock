open! Basis.Rudiments
open! Basis
open Array
open Format

let test () =
  let test_set len = begin
    let rec fn i n = begin
      match i < n with
      | false -> ()
      | true -> begin
          let arr = init (0L =:< len) ~f:(fun _ -> 0L) in
          let arr' = set i 1L arr in
          printf "set %a: %a -> %a"
            Uns.xpp i
            (xpp Uns.xpp) arr
            (xpp Uns.xpp) arr'
          ;
          set_inplace i 1L arr;
          printf " -> set_inplace: %a" (xpp Uns.xpp) arr;
          let arr'' = copy arr in
          printf " -> copy,set_inplace: %a" (xpp Uns.xpp) arr'';
          set_inplace i 2L arr'';
          printf " -> %a\n"
            (xpp Uns.xpp) arr'';
          fn (succ i) n
        end
    end in
    fn 0L len
  end in
  printf "@[<h>";
  test_set 1L;
  test_set 2L;
  test_set 3L;
  printf "@]"

let _ = test ()
