open Basis
open! Basis.Rudiments

let parse_hmhi (Io.{hmhi; _} as io) =
  match hmhi with
  | Some text -> begin
      let scanner = Scan.init text in
      let io =
        io.log
        |> Fmt.fmt "hocc: Parsing " |> Path.pp (Option.value_hlt (Text.path text)) |> Fmt.fmt "\n"
        |> Io.with_log io in
      let _scanner', hmhi = Parse.hmhi scanner in
      match hmhi with
      | Error errors -> begin
          List.iter (List.sort errors ~cmp:Parse.Error.cmp) ~f:(fun error ->
            File.Fmt.stderr |> Parse.Error.fmt ~alt:true error |> ignore
          );
          Stdlib.exit 1
        end
      | Ok hmhi -> io, Some hmhi
    end
  | None -> io, None

let parse_hmh (Io.{hmh; _} as io) =
  let scanner = Scan.init hmh in
  let io =
    io.log
    |> Fmt.fmt "hocc: Parsing " |> Path.pp (Option.value_hlt (Text.path hmh)) |> Fmt.fmt "\n"
    |> Io.with_log io in
  let _scanner', hmh = Parse.hmh scanner in
  match hmh with
  | Error errors -> begin
      List.iter (List.sort errors ~cmp:Parse.Error.cmp) ~f:(fun error ->
        File.Fmt.stderr |> Parse.Error.fmt ~alt:true error |> ignore
      );
      Stdlib.exit 1
    end
  | Ok hmh -> io, hmh

let _ =
  let conf = Conf.of_argv Os.argv in
  let io = Io.init conf in
  let io, hmhi_opt = parse_hmhi io in
  let io, hmh = parse_hmh io in
  let io, spec = Spec.init (Conf.algorithm conf) ~resolve:(Conf.resolve conf) io hmh in
  let io = match Conf.text conf with
    | false -> io
    | true -> Spec.to_txt conf io spec
  in
  let io = match Conf.html conf with
    | false -> io
    | true -> Spec.to_html conf io spec
  in
  let io = match Conf.hocc conf with
    | false -> io
    | true -> Spec.to_hocc io spec
  in
  let io = match Conf.hemlock conf with
    | false -> io
    | true -> begin
        let io = match hmhi_opt with
          | None -> io
          | Some hmhi -> Spec.to_hmi conf hmhi io spec
        in
        Spec.to_hm conf hmh io spec
      end
  in
  let io = match Conf.ocaml conf with
    | false -> io
    | true -> begin
        let io = match hmhi_opt with
          | None -> io
          | Some hmhi -> Spec.to_mli conf hmhi io spec
        in
        Spec.to_ml conf hmh io spec
      end
  in
  let _io = Io.fini conf io in
  ()
