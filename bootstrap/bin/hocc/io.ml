open Basis
include Basis.Rudiments

type t = {
  err: (module Fmt.Formatter);
  hmhi: Text.t option;
  hmh: Text.t;
  log: (module Fmt.Formatter);
  txt: (module Fmt.Formatter);
  hocc: (module Fmt.Formatter);
  hmi: (module Fmt.Formatter);
  hm: (module Fmt.Formatter);
  mli: (module Fmt.Formatter);
  ml: (module Fmt.Formatter);
}

let init_err _conf =
  File.Fmt.stderr

let path_with_suffix ?(is_report=false) conf suffix =
  Path.join [
    (Conf.srcdir conf);
    Path.of_string (match is_report with false -> "" | true -> "hocc");
    Path.of_segment (Path.Segment.join [
      (Conf.module_ conf);
      Option.value_hlt Path.(basename (of_string suffix))
    ]);
  ]

let open_infile_as_text path =
  match File.of_path path with
  | Ok f -> begin
      let stream = File.Stream.of_file f in
      let text = Text.of_bytes_stream ~path stream in
      Ok text
    end
  | Error _ as error -> error

let init_hmhi conf =
  let path = path_with_suffix conf ".hmhi" in
  match open_infile_as_text path with
  | Ok text -> Some text
  | Error _ -> None

let open_error ~err path error =
  let _err =
    err
    |> Fmt.fmt "hocc: File.of_path " |> Path.pp path |> Fmt.fmt ": ["
    |> Errno.pp error
    |> Fmt.fmt "] "
    |> Fmt.fmt (Errno.to_string error)
    |> Fmt.fmt "\n"
  in
  Stdlib.exit 1

let init_hmh conf ~err =
  let path = path_with_suffix conf ".hmh" in
  match open_infile_as_text path with
  | Ok text -> text
  | Error error -> open_error ~err path error

let init_log conf =
  match Conf.verbose conf with
  | false -> File.Fmt.sink
  | true -> File.Fmt.stdout

let init_txt conf =
  match Conf.text conf with
  | false -> File.Fmt.sink
  | true -> String.Fmt.empty

let init_hocc conf =
  match Conf.hocc conf with
  | false -> File.Fmt.sink
  | true -> String.Fmt.empty

let init_hmi conf hmhi =
  match Conf.hemlock conf, hmhi with
  | false, _
  | _, None -> File.Fmt.sink
  | true, Some _ -> String.Fmt.empty

let init_hm conf =
  match Conf.hemlock conf with
  | false -> File.Fmt.sink
  | true -> String.Fmt.empty

let init_mli conf hmhi =
  match Conf.ocaml conf, hmhi with
  | false, _
  | _, None -> File.Fmt.sink
  | true, Some _ -> String.Fmt.empty

let init_ml conf =
  match Conf.ocaml conf with
  | false -> File.Fmt.sink
  | true -> String.Fmt.empty

let init conf =
  let err = init_err conf in
  let hmhi = init_hmhi conf in
  let hmh = init_hmh conf ~err in
  let log = init_log conf in
  let txt = init_txt conf in
  let hocc = init_hocc conf in
  let hmi = init_hmi conf hmhi in
  let hm = init_hm conf in
  let mli = init_mli conf hmhi in
  let ml = init_ml conf in

  {err; hmhi; hmh; log; txt; hocc; hmi; hm; mli; ml}

let open_outfile_as_formatter ~is_report ~err path =
  let _ = match is_report with
    | false -> ()
    | true -> Os.mkdirat (Path.dirname path) |> ignore
  in
  match File.of_path ~flag:File.Flag.W path with
  | Ok f -> File.Fmt.of_t f
  | Error error -> open_error ~err path error

let fini_formatter ?(is_report=false) conf conflicts ~err ~log formatter suffix =
  match is_report || (not conflicts) with
  | true -> begin
      match Fmt.sync formatter with
      | To_string s -> begin
          let path = path_with_suffix ~is_report conf suffix in
          let log' = log |> Fmt.fmt "hocc: Writing " |> Path.pp path |> Fmt.fmt "\n" in
          let formatter = open_outfile_as_formatter ~is_report ~err path in
          let formatter' = formatter |> Fmt.fmt s |> Fmt.flush in
          log', formatter'
        end
      | Synced formatter' -> log, formatter'
    end
  | false -> log, formatter

let fini conf conflicts ({err; log; txt; hocc; hmi; hm; mli; ml; _} as t) =
  let log, txt = fini_formatter ~is_report:true conf conflicts ~err ~log txt ".txt" in
  let log, hocc = fini_formatter ~is_report:true conf conflicts ~err ~log hocc ".hmh" in

  let log, hmi = fini_formatter conf conflicts ~err ~log hmi ".hmi" in
  let log, hm = fini_formatter conf conflicts ~err ~log hm ".hm" in
  let log, mli = fini_formatter conf conflicts ~err ~log mli ".mli" in
  let log, ml = fini_formatter conf conflicts ~err ~log ml ".ml" in
  let log = Fmt.flush log in
  {t with log; txt; hocc; hmi; hm; mli; ml}

let fatal {err; _} =
  let _err = Fmt.flush err in
  Stdlib.exit 1

let with_err t err =
  {t with err}

let with_log t log =
  let log = Fmt.flush log in
  {t with log}

let with_txt t txt =
  {t with txt}

let with_hocc t hocc =
  {t with hocc}

let with_hmi t hmi =
  {t with hmi}

let with_hm t hm =
  {t with hm}

let with_mli t mli =
  {t with mli}

let with_ml t ml =
  {t with ml}
