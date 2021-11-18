open! Basis.Rudiments
open! Basis
open U16

let test () =
  let rec test_pairs = function
    | [] -> ()
    | (x, y) :: pairs' -> begin
        File.Fmt.stdout
        |> fmt ~alt:true ~zpad:true ~width:4L ~base:Fmt.Hex ~pretty:true x
        |> Fmt.fmt " ** "
        |> fmt ~alt:true ~zpad:true ~width:4L ~base:Fmt.Hex ~pretty:true y
        |> Fmt.fmt " -> "
        |> fmt ~alt:true ~zpad:true ~width:4L ~base:Fmt.Hex ~pretty:true (x ** y)
        |> Fmt.fmt "\n"
        |> ignore;
        test_pairs pairs'
      end
  in
  let pairs = [
    (kv 0L, kv 0L);
    (kv 0L, kv 1L);

    (kv 0xffffL, kv 0L);
    (kv 0xffffL, kv 1L);

    (kv 2L, kv 15L);
    (kv 2L, kv 16L);
    (kv 2L, kv 31L);
    (kv 2L, kv 32L);

    (kv 0xfL, kv 0xfL);
    (kv 0xffL, kv 0xffL);

    (kv 1L, kv 0xffffL);

    (kv 0xffffL, kv 0xffffL);
  ] in
  test_pairs pairs

let _ = test ()
