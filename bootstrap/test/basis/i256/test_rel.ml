open! Basis.Rudiments
open! Basis
open I256

let pp_x x formatter =
  formatter |> fmt ~alt:true ~zpad:true ~width:64L ~radix:Radix.Hex ~pretty:true x

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
    |> pp min
    |> Fmt.fmt " ~max:"
    |> pp max
    |> Fmt.fmt " "
    |> pp t
    |> Fmt.fmt " -> "
    |> pp (clamp ~min ~max t)
    |> Fmt.fmt "\nbetween ~low:"
    |> pp min
    |> Fmt.fmt " ~high:"
    |> pp max
    |> Fmt.fmt " "
    |> pp t
    |> Fmt.fmt " -> "
    |> Bool.pp (between ~low:min ~high:max t)
    |> Fmt.fmt "\n"
  end in
  File.Fmt.stdout
  |> fn zero (of_string
      "0x8000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000")
  |> Fmt.fmt "\n"
  |> fn zero (of_string
      "0xffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff")
  |> Fmt.fmt "\n"
  |> fn (of_string
      "0x8000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000")
    (of_string
        "0x7fff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff")
  |> fn2 (of_string "-2") (of_string "-1") (of_string "1")
  |> fn2 (of_string "1") (of_string "1") (of_string "1")
  |> fn2 (of_string "0") (of_string "-1") (of_string "1")
  |> fn2 (of_string "1") (of_string "-1") (of_string "1")
  |> fn2 (of_string "2") (of_string "-1") (of_string "1")

let _ = test ()
