open! Basis.Rudiments
open! Basis
open U16

let pp_x x formatter =
  formatter |> fmt ~alt:true ~zpad:true ~width:4L ~radix:Radix.Hex ~pretty:true x

let test () =
  let fn x y formatter = begin
    let f_cmp cmp_s x y cmp formatter = begin
      formatter |> Fmt.fmt cmp_s |> Fmt.fmt " " |> pp_x x |> Fmt.fmt " " |> pp_x y |> Fmt.fmt " -> "
      |> Cmp.pp cmp |> Fmt.fmt "\n"
    end in
    let f_rel x rel_s y rel formatter = begin
      formatter |> pp_x x |> Fmt.fmt " " |> Fmt.fmt rel_s |> Fmt.fmt " " |> pp_x y |> Fmt.fmt " -> "
      |> Bool.pp rel |> Fmt.fmt "\n"
    end in
    formatter
    |> f_cmp "cmp" x y (cmp x y)
    |> f_rel x ">=" y (x >= y)
    |> f_rel x "<=" y (x <= y)
    |> f_rel x "=" y (x = y)
    |> f_rel x ">" y (x > y)
    |> f_rel x "<" y (x < y)
    |> f_rel x "<>" y (x <> y)
    |> f_cmp "ascending" x y (ascending x y)
    |> f_cmp "descending" x y (descending x y)
  end in
  let fn2 t min max formatter = begin
    formatter
    |> Fmt.fmt "\nclamp ~min:"
    |> pp_x min
    |> Fmt.fmt " ~max:"
    |> pp_x max
    |> Fmt.fmt " "
    |> pp_x t
    |> Fmt.fmt " -> "
    |> pp_x (clamp ~min ~max t)
    |> Fmt.fmt "\nbetween ~low:"
    |> pp_x min
    |> Fmt.fmt " ~high:"
    |> pp_x max
    |> Fmt.fmt " "
    |> pp_x t
    |> Fmt.fmt " -> "
    |> Bool.pp (between ~low:min ~high:max t)
    |> Fmt.fmt "\n"
  end in
  File.Fmt.stdout
  |> fn (kv 0L) (kv 0x8000L)
  |> Fmt.fmt "\n"
  |> fn (kv 0L) (kv 0xffffL)
  |> Fmt.fmt "\n"
  |> fn (kv 0x8000L) (kv 0xffffL)
  |> fn2 (kv 0x7ffeL) (kv 0x7fffL) (kv 0x8001L)
  |> fn2 (kv 0x7fffL) (kv 0x7fffL) (kv 0x8001L)
  |> fn2 (kv 0x8000L) (kv 0x7fffL) (kv 0x8001L)
  |> fn2 (kv 0x8001L) (kv 0x7fffL) (kv 0x8001L)
  |> fn2 (kv 0x8002L) (kv 0x7fffL) (kv 0x8001L)

let _ = test ()
