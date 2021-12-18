open Basis
include Basis.Rudiments
open Hmc

let scan_file path =
  let rec fn scanner = begin
    let scanner', ctoken = Scan.next scanner in
    let atoken = Scan.ConcreteToken.atoken ctoken in
    let source = Scan.ConcreteToken.source ctoken in
    File.Fmt.stdout
    |> Fmt.fmt "  "
    |> Source.Slice.pp source
    |> Fmt.fmt " : "
    |> Scan.AbstractToken.pp atoken
    |> Fmt.fmt "\n"
    |> ignore;
    match atoken with
    | Scan.AbstractToken.Tok_end_of_input -> ()
    | _ -> fn scanner'
  end in
  let () = match File.of_path path with
    | Ok f -> begin
        let stream = File.Stream.of_file f in
        let path_str = Bytes.Slice.to_string_hlt path in
        let text = Text.of_bytes_stream ~path:path_str stream in
        let scanner = Scan.init text in
        fn scanner
      end
    | Error err -> halt (
      String.Fmt.empty
      |> Fmt.fmt "File.of_path error: "
      |> Fmt.fmt (File.Error.to_string err)
      |> Fmt.fmt "\n"
      |> Fmt.to_string
    )
  in
  ()

let _ =
  match Array.length Sys.argv with
  | 0L | 1L -> halt "hmc usage: hmc <path>"
  | _ -> begin
      let path_str = Array.get 1L Sys.argv in
      let path_slice = String.C.Slice.of_string path_str in
      let path = Bytes.Slice.of_string_slice path_slice in
      scan_file path
    end
