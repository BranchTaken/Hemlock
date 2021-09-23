open! Basis.Rudiments
open! Basis
open List
open Format

let test () =
  let test_pop_push lst = begin
    printf "%a -> " (pp Uns.pp) lst;
    let hd_, tl_ = pop lst in
    assert (hd_ = (hd lst));
    let () = match cmp Uns.cmp tl_ (tl lst) with
      | Cmp.Eq -> ()
      | _ -> assert false
    in
    printf "%a %a -> %a = %a\n"
      Uns.pp hd_
      (pp Uns.pp) tl_
      (pp Uns.pp) (push hd_ tl_)
      (pp Uns.pp) (hd_ :: tl_)
  end in
  let lists = [
    [0L];
    [0L; 1L];
    [0L; 1L; 2L];
  ] in
  printf "@[<h>";
  iter lists ~f:(fun lst -> test_pop_push lst);
  printf "@]"

let _ = test ()
