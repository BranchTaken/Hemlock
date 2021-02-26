open Basis
include Basis.Rudiments
open Hmc

let scan_file path =
  let open Format in
  let rec fn scanner = begin
    let scanner', ctoken = Scan.next scanner in
    let atoken = Scan.ConcreteToken.atoken ctoken in
    let source = Scan.ConcreteToken.source ctoken in
    printf "  %a : %s\n"
      Scan.Source.pp_loc source
      (Scan.AbstractToken.to_string atoken)
    ;
    match atoken with
    | Scan.AbstractToken.Tok_end_of_input -> ()
    | _ -> fn scanner'
  end in
  printf "@[<h>";
  let () = match File.of_path ~flag:File.Flag.R_O path with
    | Ok f -> begin
        let stream = File.Stream.of_file f in
        let path_str = Bytes.Slice.to_string_hlt path in
        let text = Text.of_bytes_stream ~path:path_str stream in
        let scanner = Scan.init text in
        fn scanner
      end
    | Error err -> begin
        halt (asprintf "File.of_path error: %s\n" (File.Error.to_string err))
      end
  in
  printf "@]"

let _ =
  match Array.length Sys.argv with
  | 0 | 1 -> halt "hmc usage: hmc <path>"
  | _ -> begin
      let path_str = Array.get 1 Sys.argv in
      let path_slice = String.Slice.of_string path_str in
      let path = Bytes.Slice.of_string_slice path_slice in
      scan_file path
    end
