open! Basis
include Basis.Rudiments

type algorithm =
  | Lr1
  | Ielr1
  | Pgm1
  | Lalr1

let pp_algorithm algorithm formatter =
  formatter |> Fmt.fmt (match algorithm with
    | Lr1 -> "Lr1"
    | Ielr1 -> "Ielr1"
    | Pgm1 -> "Pgm1"
    | Lalr1 -> "Lalr1"
  )

type t = {
  verbose: bool;
  text: bool;
  html: bool;
  hocc: bool;
  algorithm: algorithm;
  resolve: bool;
  remerge: bool;
  hemlock: bool;
  ocaml: bool;
  srcdir_opt: Path.t option;
  module_opt: Path.Segment.t option;
  dstdir_opt: Path.t option;
}

let pp {verbose; text; html; hocc; algorithm; resolve; remerge; hemlock; ocaml; srcdir_opt;
  module_opt; dstdir_opt} formatter =
  formatter
  |> Fmt.fmt "{verbose=" |> Bool.pp verbose
  |> Fmt.fmt "; text=" |> Bool.pp text
  |> Fmt.fmt "; html=" |> Bool.pp html
  |> Fmt.fmt "; hocc=" |> Bool.pp hocc
  |> Fmt.fmt "; algorithm=" |> pp_algorithm algorithm
  |> Fmt.fmt "; resolve=" |> Bool.pp resolve
  |> Fmt.fmt "; remerge=" |> Bool.pp remerge
  |> Fmt.fmt "; hemlock=" |> Bool.pp hemlock
  |> Fmt.fmt "; ocaml=" |> Bool.pp ocaml
  |> Fmt.fmt "; srcdir_opt=" |> (Option.pp Path.pp) srcdir_opt
  |> Fmt.fmt "; module_opt=" |> (Option.pp Path.Segment.pp) module_opt
  |> Fmt.fmt "; dstdir_opt=" |> (Option.pp Path.pp) dstdir_opt
  |> Fmt.fmt "}"

let default = {
  verbose=false;
  text=false;
  html=false;
  hocc=false;
  algorithm=Lr1;
  resolve=true;
  remerge=true;
  hemlock=false;
  ocaml=false;
  srcdir_opt=None;
  module_opt=None;
  dstdir_opt=None;
}

let usage error =
  let exit_code, formatter = match error with
    | false -> 0, File.Fmt.stdout
    | true -> 1, File.Fmt.stderr
  in
  formatter
  |> Fmt.fmt {|hocc usage: hocc <parameters>

Parameters:
              -h[elp] : Print command usage and exit.
           -v[erbose] : Print progress information during parser generation.
         -txt | -text : Write a detailed automaton description in plain text
                        format to "<dstdir>/hocc/<module>.txt".
                -html : Write a detailed automaton description in internally
                        hyperlinked HTML format to
                        "<dstdir>/hocc/<module>.html".
         -hmh | -hocc : Write a complete grammar specification in hocc format to
                        "<dstdir>/hocc/<module>.hmh", but with all non-terminal
                        types and reduction code omitted.
   -a[lgorithm] <alg> : Use the specified <alg>orithm for generating an
                        automaton. Defaults to lr1.
                        - lr1: Canonical LR(1) automaton.
                        - ielr1: Compact LR(1) automaton that recognizes valid
                          inputs identically to lr1 automatons, even in the
                          presence of precedence-resolved ambiguities.
                        - pgm1: Compact LR(1) automaton that recognizes valid
                          inputs identically to lr1 automatons, provided there
                          were no precedence-resolved ambiguities in the grammar
                          specification.
                        - lalr1: LALR(1) automaton.
  -r[esolve] (yes|no) : Control whether conflict resolution is enabled. Defaults
                        to yes.
-[re]m[erge] (yes|no) : Control whether remerging equivalent split states is
                        enabled. Defaults to yes.
       -hm | -hemlock : Generate a Hemlock-based parser implementation and write
                        it to "<dstdir>/<module>.hm[i]".
         -ml | -ocaml : Generate an OCaml-based parser implementation and write
                        it to "<dstdir>/<module>.ml[i]". This is brittle
                        functionality intended only for Hemlock bootstrapping.
         -s[rc] <src> : Path and module name of input source, where inputs match
                        "<src>.hmh[i]" and "<src>" comprises the source
                        directory and module name, "[<srcdir>/]<module>".
   -d[stdir] <dstdir> : Path to directory in which to place generated output,
                        such that output file paths match
                        "<dstdir>/[hocc/]<module>.*". Defaults to "<srcdir>".
|}
  |> ignore;
  Stdlib.exit exit_code

let is_segment_cident segment =
  let rec cont cursor past = begin
    match String.C.Cursor.(<) cursor past with
    | false -> true
    | true -> begin
        let cp, cursor' = String.C.Cursor.next cursor in
        match cp with
        | cp when Codepoint.(cp >= of_char 'A' && cp <= of_char 'Z') -> cont cursor' past
        | cp when Codepoint.(cp >= of_char 'a' && cp <= of_char 'z') -> cont cursor' past
        | cp when Codepoint.(cp >= of_char '0' && cp <= of_char '9') -> cont cursor' past
        | cp when Codepoint.(cp = of_char '_') -> cont cursor' past
        | cp when Codepoint.(cp = of_char '\'') -> cont cursor' past
        | _ -> false
      end
  end in
  let rec start cursor past = begin
    match String.C.Cursor.(<) cursor past with
    | false -> false
    | true -> begin
        let cp, cursor' = String.C.Cursor.next cursor in
        match cp with
        | cp when Codepoint.(cp = of_char '_') -> start cursor' past
        | cp when Codepoint.(cp >= of_char 'A' && cp <= of_char 'Z') -> cont cursor' past
        | _ -> false
      end
  end in
  match Path.Segment.to_string segment with
  | None -> false
  | Some s -> begin
      let sslice = String.C.Slice.of_string s in
      let base = String.C.Slice.base sslice in
      let past = String.C.Slice.past sslice in
      start base past
    end

let of_argv argv =
  let arg_arg argv i = begin
    let i' = succ i in
    match i' < Array.length argv with
    | false -> begin
        let arg = Bytes.to_string_replace (Array.get i argv) in
        File.Fmt.stderr |> Fmt.fmt "hocc: " |> Fmt.fmt arg |> Fmt.fmt " argument missing\n"
        |> ignore;
        usage true
      end
    | true -> Array.get i' argv
  end in
  let rec f t argv i = begin
    match i < Array.length argv with
    | false -> t
    | true -> begin
        let arg_bytes = Array.get i argv in
        let arg_string = Bytes.to_string_replace arg_bytes in
        match arg_string with
        | "-help" | "-h" -> usage false
        | "-verbose" | "-v" -> f {t with verbose=true} argv (succ i)
        | "-txt" | "-text" -> f {t with text=true} argv (succ i)
        | "-html" -> f {t with html=true} argv (succ i)
        | "-hmh" | "-hocc" -> f {t with hocc=true} argv (succ i)
        | "-algorithm" | "-a" -> begin
            let algorithm = match Bytes.to_string_replace (arg_arg argv i) with
              | "lr1" -> Lr1
              | "ielr1" -> Ielr1
              | "pgm1" -> Pgm1
              | "lalr1" -> Lalr1
              | s -> begin
                  File.Fmt.stderr |> Fmt.fmt "hocc: Invalid algorithm: " |> Fmt.fmt s
                  |> Fmt.fmt "\n" |> ignore;
                  usage true
                end
            in
            f {t with algorithm} argv (i + 2L)
          end
        | "-resolve" | "-r" -> begin
            let resolve = match Bytes.to_string_replace (arg_arg argv i) with
              | "yes" -> true
              | "no" -> false
              | s -> begin
                  File.Fmt.stderr |> Fmt.fmt "hocc: Invalid resolve parameter: "
                  |> Fmt.fmt s |> Fmt.fmt "\n" |> ignore;
                  usage true
                end
            in
            f {t with resolve} argv (i + 2L)
          end
        | "-remerge" | "-m" -> begin
            let remerge = match Bytes.to_string_replace (arg_arg argv i) with
              | "yes" -> true
              | "no" -> false
              | s -> begin
                  File.Fmt.stderr |> Fmt.fmt "hocc: Invalid remerge parameter: "
                  |> Fmt.fmt s |> Fmt.fmt "\n" |> ignore;
                  usage true
                end
            in
            f {t with remerge} argv (i + 2L)
          end
        | "-hm" | "-hemlock" -> f {t with hemlock=true} argv (succ i)
        | "-ml" | "-ocaml" -> f {t with ocaml=true} argv (succ i)
        | "-src" | "-s" -> begin
            let path = Path.of_bytes (Bytes.Slice.init (arg_arg argv i)) in
            let dirname, basename_opt = Path.split path in
            let srcdir_opt = match Path.is_empty dirname with
              | true -> None
              | false -> Some dirname
            in
            let module_opt = match basename_opt with
              | None -> begin
                  File.Fmt.stderr
                  |> Fmt.fmt "hocc: Invalid source: "
                  |> Path.pp path
                  |> Fmt.fmt "\n"
                  |> ignore;
                  usage true
                end
              | Some segment -> begin
                  match is_segment_cident segment with
                  | false -> begin
                      File.Fmt.stderr |> Fmt.fmt "hocc: Invalid source module name: "
                      |> Path.Segment.pp segment |> Fmt.fmt "\n" |> ignore;
                      usage true
                    end
                  | true -> Some segment
                end
            in
            f {t with srcdir_opt; module_opt} argv (i + 2L)
          end
        | "-dstdir" | "-d" -> begin
            let dstdir = Path.of_bytes (Bytes.Slice.init (arg_arg argv i)) in
            f {t with dstdir_opt=Some dstdir} argv (i + 2L)
          end
        | _ -> begin
            File.Fmt.stderr
            |> Fmt.fmt "hocc: Invalid command line parameter: "
            |> String.pp arg_string
            |> Fmt.fmt "\n"
            |> ignore;
            usage true
          end
      end
  end in
  let t = f default argv 1L in
  match t.module_opt with
  | None -> begin
      File.Fmt.stderr |> Fmt.fmt "hocc: Source unspecified\n" |> ignore;
      usage true
    end
  | Some _ -> t

let verbose {verbose; _} =
  verbose

let text {text; _} =
  text

let html {html; _} =
  html

let hocc {hocc; _} =
  hocc

let algorithm {algorithm; _} =
  algorithm

let resolve {resolve; _} =
  resolve

let remerge {remerge; _} =
  remerge

let hemlock {hemlock; _} =
  hemlock

let ocaml {ocaml; _} =
  ocaml

let srcdir {srcdir_opt; _} =
  match srcdir_opt with
  | None -> Path.of_string "."
  | Some srcdir -> srcdir

let module_ {module_opt; _} =
  match module_opt with
  | None -> not_reached ()
  | Some m -> m

let dstdir {dstdir_opt; _} =
  match dstdir_opt with
  | None -> Path.of_string "."
  | Some dstdir -> dstdir
