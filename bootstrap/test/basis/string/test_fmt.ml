open! Basis.Rudiments
open! Basis
open String

let test () =
  File.Fmt.stdout
  |> fmt "fmt" |> Basis.Fmt.fmt "\n"
  |> pp "pp" |> Basis.Fmt.fmt "\n"
  |> pp "pp \n escaped" |> Basis.Fmt.fmt "\n"
  |> fmt ~pretty:true "fmt pretty" |> Basis.Fmt.fmt "\n"
  |> fmt ~alt:true ~pretty:true "fmt alt pretty" |> Basis.Fmt.fmt "\n"
  |> fmt ~alt:true ~pretty:true "fmt alt pretty\n not escaped" |> Basis.Fmt.fmt "\n"
  |> fmt ~alt:true ~pretty:true "\nfmt alt pretty multi-line\n" |> Basis.Fmt.fmt "\n"
  |> Basis.Fmt.fmt "fmt alt pretty \"\\n\":\n"
  |> fmt ~alt:true ~pretty:true "\n" |> Basis.Fmt.fmt "\n"
  |> Basis.Fmt.fmt "fmt alt pretty \"\\n\\n\":\n"
  |> fmt ~alt:true ~pretty:true "\n\n" |> Basis.Fmt.fmt "\n"
  |> fmt ~alt:true ~pretty:true "..." |> Basis.Fmt.fmt "\n"
  |> fmt ~alt:true ~pretty:true "...`..." |> Basis.Fmt.fmt "\n"
  |> fmt ~alt:true ~pretty:true "...``..." |> Basis.Fmt.fmt "\n"
  |> fmt ~alt:true ~pretty:true "`..." |> Basis.Fmt.fmt "\n"
  |> fmt ~alt:true ~pretty:true "...`" |> Basis.Fmt.fmt "\n"
  |> fmt ~alt:true ~pretty:true "...``...`_`..." |> Basis.Fmt.fmt "\n"
  |> fmt ~alt:true ~pretty:true "_`...``..." |> Basis.Fmt.fmt "\n"
  |> fmt ~alt:true ~pretty:true "...``...`_" |> Basis.Fmt.fmt "\n"
  |> fmt ~alt:true ~pretty:true "...``...`_`...`__`..." |> Basis.Fmt.fmt "\n"
  |> fmt ~alt:true ~pretty:true "__`...``...`_`..." |> Basis.Fmt.fmt "\n"
  |> fmt ~alt:true ~pretty:true "...``...`_`...`__" |> Basis.Fmt.fmt "\n"
  |> fmt ~alt:true ~pretty:true
    "...``...`_`...`__`...`__`_0_`_1_`_2_`_3_`_4_`_5_`_6_`_7_`_8_`_9_`_a_`_b_`_c_`_d_`_e_`_f_..."
  |> Basis.Fmt.fmt "\n"
  |> ignore

let _ = test ()
