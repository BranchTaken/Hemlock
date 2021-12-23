open! Basis.Rudiments
open! Basis
open! Hmc

let scan_str s =
  let rec fn t ctokens = begin
    let t', ctoken = Scan.next t in
    let atoken = Scan.ConcreteToken.atoken ctoken in
    match atoken with
    | Tok_end_of_input -> List.rev ctokens
    | _ -> fn t' (ctoken :: ctokens)
  end in
  let t = Scan.init (Text.of_string_slice (String.C.Slice.of_string s)) in
  fn t []

let contextualize source first last =
  let ctokens = scan_str source in
  let first_ctoken = List.nth first ctokens in
  let last_ctoken = List.nth last ctokens in
  let lookahead_ctoken_opt = (
    let rec f last lookahead tail =
      match tail with
      | [] -> lookahead
      | ctoken :: tail' -> begin
          let last_line = Text.Pos.line (Source.Cursor.pos (Source.Cursor.unbias (Source.Slice.past
              (Scan.ConcreteToken.source last)))) in
          let ctoken_line = Text.Pos.line (Source.Cursor.pos (Source.Cursor.unbias
              (Source.Slice.base (Scan.ConcreteToken.source ctoken)))) in
          match last_line < ctoken_line with
          | true -> Some ctoken
          | false -> f last (Some ctoken) tail'
        end
    in
    let tail = List.drop (succ last) ctokens in
    f last_ctoken None tail
  ) in
  let lookahead = match lookahead_ctoken_opt with
    | None -> None
    | Some ctoken -> Some (Source.Slice.past (Scan.ConcreteToken.source ctoken))
  in
  let source_slice = Source.Slice.of_cursors
      ~base:(Source.Slice.base (Scan.ConcreteToken.source first_ctoken))
      ~past:(Source.Slice.past (Scan.ConcreteToken.source last_ctoken)) in
  let context_lookahead = Source.Slice.line_context ?lookahead source_slice in
  let context_no_lookahead = Source.Slice.line_context source_slice in
  File.Fmt.stdout
  |> Fmt.fmt "---\nsource=" |> String.fmt ~alt:true ~pretty:true source
  |> Fmt.fmt "\nfirst=" |> Scan.ConcreteToken.pp (List.nth first ctokens)
  |> Fmt.fmt "\nlast=" |> Scan.ConcreteToken.pp (List.nth last ctokens)
  |> Fmt.fmt "\nlookahead=" |> (Option.pp Source.Cursor.pp) lookahead
  |> Fmt.fmt "\nctokens="
  |> (List.fmt ~alt:true Scan.ConcreteToken.pp) ctokens
  |> Fmt.fmt "\ncontext_lookahead="
  |> (List.fmt ~alt:true (fun slice formatter ->
    formatter
    |> Source.Slice.pp slice
    |> Fmt.fmt ": "
    |> String.pp (Source.Slice.to_string slice)
  )) context_lookahead
  |> Fmt.fmt "\ncontext_no_lookahead="
  |> (List.fmt ~alt:true (fun slice formatter ->
    formatter
    |> Source.Slice.pp slice
    |> Fmt.fmt ": "
    |> String.pp (Source.Slice.to_string slice)
  )) context_no_lookahead
  |> Fmt.fmt "\n"
  |> ignore

let test () =
  contextualize {|let x = 42|} 0L 0L;
  contextualize {|let x = 42|} 2L 2L;
  contextualize {|let x = 42|} 1L 2L;
  contextualize {|let x = 42|} 6L 6L;
  contextualize {|[:"Foo.hm"]let x = 42|} 3L 3L;
  contextualize {|[:"Foo.hm"]\
let x =
    42|} 4L 4L;
  contextualize {|[:"Foo.hm"]\
let x =
    42|} 4L 7L;
  contextualize {|[:"Foo.hm"]\
let x =[:"Bar.hm"]
    42 + 13|} 4L 11L

let _ = test ()
