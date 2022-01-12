open! Basis.Rudiments
open! Basis
open! Hmc

let scan_str s =
  let rec fn t ctoks = begin
    let t', ctok = Scan.next t in
    let atok = Scan.ConcreteToken.atok ctok in
    match atok with
    | Tok_end_of_input -> List.rev ctoks
    | _ -> fn t' (ctok :: ctoks)
  end in
  let t = Scan.init (Text.of_string_slice (String.C.Slice.of_string s)) in
  fn t []

let contextualize source first last =
  let ctoks = scan_str source in
  let first_ctok = List.nth first ctoks in
  let last_ctok = List.nth last ctoks in
  let lookahead_ctok_opt = (
    let rec f last lookahead tail =
      match tail with
      | [] -> lookahead
      | ctok :: tail' -> begin
          let last_line = Text.Pos.line (Source.Cursor.pos (Source.Cursor.unbias (Source.Slice.past
              (Scan.ConcreteToken.source last)))) in
          let ctok_line = Text.Pos.line (Source.Cursor.pos (Source.Cursor.unbias
              (Source.Slice.base (Scan.ConcreteToken.source ctok)))) in
          match last_line < ctok_line with
          | true -> Some ctok
          | false -> f last (Some ctok) tail'
        end
    in
    let tail = List.drop (succ last) ctoks in
    f last_ctok None tail
  ) in
  let lookahead = match lookahead_ctok_opt with
    | None -> None
    | Some ctok -> Some (Source.Slice.past (Scan.ConcreteToken.source ctok))
  in
  let source_slice = Source.Slice.of_cursors
      ~base:(Source.Slice.base (Scan.ConcreteToken.source first_ctok))
      ~past:(Source.Slice.past (Scan.ConcreteToken.source last_ctok)) in
  let context_lookahead = Source.Slice.line_context ?lookahead source_slice in
  let context_no_lookahead = Source.Slice.line_context source_slice in
  let pp_i_tok (i, ctok) formatter = begin
    formatter
    |> Fmt.fmt "("
    |> Uns.pp i
    |> Fmt.fmt ", "
    |> Scan.ConcreteToken.pp ctok
    |> Fmt.fmt ")"
  end in
  File.Fmt.stdout
  |> Fmt.fmt "---\nsource=" |> String.fmt ~alt:true ~pretty:true source
  |> Fmt.fmt "\nfirst=" |> Scan.ConcreteToken.pp (List.nth first ctoks)
  |> Fmt.fmt "\nlast=" |> Scan.ConcreteToken.pp (List.nth last ctoks)
  |> Fmt.fmt "\nlookahead=" |> (Option.pp Source.Cursor.pp) lookahead
  |> Fmt.fmt "\nctoks="
  |> (List.fmt ~alt:true pp_i_tok) (List.mapi ctoks ~f:(fun i ctok -> (i, ctok)))
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
  contextualize {|x = 42|} 0L 0L;
  contextualize {|x = 42|} 1L 1L;
  contextualize {|x = 42|} 1L 2L;
  contextualize {|x = 42|} 4L 4L;
  contextualize {|[:"Foo.hm"]x = 42|} 1L 1L;
  contextualize {|[:"Foo.hm"]
x =
    42|} 3L 3L;
  contextualize {|[:"Foo.hm"]
x =
    42|} 3L 6L;
  contextualize {|[:"Foo.hm"]
x =[:"Bar.hm"]
    42 + 13|} 3L 10L;

  contextualize {|accept parser =
    let node, parser' = [...]
    let () = (%accept_hook)
    node, parser'|} 0L 41L;
  contextualize {|%accept_hook = %(
    File.Fmt.stdout |> "%f(^Node.pp^)=(^node^)\n" |> ignore
  )%|} 0L 27L;
  contextualize {|[:"Pgen.hm"]accept parser =
    let node, parser' = [...]
    let () = ([:"Foo.hmy":1:0+16](
    File.Fmt.stdout |> "%f(^Node.pp^)=(^node^)\n" |> ignore
  )[:"Pgen.hm":3:4+26])
    node, parser'|} 0L 63L

let _ = test ()
