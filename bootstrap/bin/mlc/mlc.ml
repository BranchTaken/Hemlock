open Basis
include Basis.Rudiments
open Mlc

let scan_file path =
  let rec fn scanner = begin
    let scanner', tok = Scan.next scanner in
    File.Fmt.stdout
    |> Fmt.fmt "  "
    |> Scan.Token.pp tok
    |> Fmt.fmt "\n"
    |> ignore;
    match tok with
    | Scan.Token.Tok_end_of_input _ -> ()
    | _ -> fn scanner'
  end in
  let () = match File.of_path path with
    | Ok f -> begin
        let stream = File.Stream.of_file f in
        let text = Text.of_bytes_stream ~path stream in
        let scanner = Scan.init text in
        fn scanner
      end
    | Error err -> halt (
      String.Fmt.empty
      |> Fmt.fmt "File.of_path error: "
      |> Fmt.fmt (Errno.to_string err)
      |> Fmt.fmt "\n"
      |> Fmt.to_string
    )
  in
  ()

let _ =
  match Array.length Os.argv with
  | 0L | 1L -> halt "mlc usage: mlc <path>"
  | _ -> begin
      let path = Path.of_bytes (Bytes.Slice.init (Array.get 1L Os.argv)) in
      scan_file path
    end
