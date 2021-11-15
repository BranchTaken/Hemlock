open! Basis.Rudiments
open! Basis
open List

let test () =
  let test_pop_push lst = begin
    File.Fmt.stdout
    |> (pp Uns.pp) lst
    |> Fmt.fmt " -> "
    |> ignore;
    let hd_, tl_ = pop lst in
    assert (hd_ = (hd lst));
    let () = match cmp Uns.cmp tl_ (tl lst) with
      | Cmp.Eq -> ()
      | _ -> assert false
    in
    File.Fmt.stdout
    |> Uns.pp hd_
    |> Fmt.fmt " "
    |> (pp Uns.pp) tl_
    |> Fmt.fmt " -> "
    |> (pp Uns.pp) (push hd_ tl_)
    |> Fmt.fmt " = "
    |> (pp Uns.pp) (hd_ :: tl_)
    |> Fmt.fmt "\n"
    |> ignore
  end in
  let lists = [
    [0L];
    [0L; 1L];
    [0L; 1L; 2L];
  ] in
  iter lists ~f:(fun lst -> test_pop_push lst)

let _ = test ()
