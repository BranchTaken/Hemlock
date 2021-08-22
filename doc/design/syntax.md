# Syntax

Hemlock's syntax derives from [OCaml](https://ocaml.org/), which in turn is loosely based on the
[lambda calculus](https://en.wikipedia.org/wiki/Lambda_calculus) syntax. Hemlock omits OCaml syntax
such as that for objects and polymorphic variants, and adds syntax for distinct features like
algebraic effects. But the biggest difference is that Hemlock uses indentation to rigidly define
block structure.

## Encoding

Hemlock uses [UTF-8](https://en.wikipedia.org/wiki/UTF-8) encoding. A [byte order
mark](https://en.wikipedia.org/wiki/Byte_order_mark) prefix will cause a syntax error and should
therefore always be omitted. Encoding errors result in one or more `�` replacement codepoint
substitutions. Such replacements of invalid encoding are silently ignored inside comments, but they
explicitly cause scanning errors if inside codepoint/string literals, and elsewhere they result in
error tokens which cause parsing errors.

## Indentation

Semantically meaningful indentation has multiple advantages:

- Programmers typically indent code to indicate block structure, in order to streamline human
  understanding, even if the compiler is oblivious to indentation. Making indentation meaningful
  reduces the gap between the programmer's mental model and the language semantics.
- Fewer symbols are required. The omission of lines consisting only of `end` or `}` tends to reduce
  total number of lines, even as compared with a two-column indentation style. (Hemlock enforces
  four-column indentation; more on that below.)
- Automated code formatting tools are brain-dead simple as compared with the near-sentience required
  to implement typical coding styles in many contemporary languages.
- One of the most effective general heuristics for parser error recovery is to heed indentation and
  restart parsing at the enclosing indentation level. By mandating proper indentation, error
  recovery is yet more effective, and it becomes feasible to provide code introspection services
  even for source files in intermediate editing states.

Several other contemporary languages have semantically meaningful indentation, but approaches vary
considerably. The following languages have particularly interesting takes on the problem.

- [Python](https://www.python.org/) is the most widely used language for which indentation is
  significant, and the rules for what constitutes acceptable indentation are quite flexible. The
  details changed a bit between Python 2 and 3 to reduce ways in which tab width could obscure code
  meaning.
- [Haskell](https://www.haskell.org/) requires continuation of an expressions to be indented more
  than its beginning token. The rules are simple and consistent; the biggest common pitfall is that
  if a multi-line expression starts on a line with preceding tokens that change length, the entire
  expression must be re-indented during refactoring.
- [F#](https://fsharp.org/) indentation is modeled as an "offside" rule similar to that used in
  soccer. Although elegant in concept, this approach motivates an unfortunate set of special cases
  which allow multi-line expressions to be indented "prettily", e.g. with math operators aligned.

Hemlock takes a comparatively simple approach to avoid some of the pitfalls mentioned above:

- Tabs are forbidden in whitespace. (Tabs can be embedded only in string literals and comments.)
- The first non-whitespace token establishes line indentation.
- Block indentation is four columns.
- Expression continuation indentation is two colums. One-column and three-column indentation are
  *always* invalid, which eliminates undetected off-by-one errors.
- The raw `'\n'` at the end of a line is treated as non-breaking whitespace if immediately preceded
  by a `'\\'`. This acts as an escape hatch which allows arbitrary continuation indentation.
- Unindented lines comprising only comments and/or whitespace are ignored.
- Consecutive lines at the same indentation are distinct expressions.

Automated formatting only needs to perform a few actions:

- Wrap lines that exceed 80 columns if at all possible without changing the token stream.
- Strip extraneous trailing whitespace, including any `'\n'` codepoints following the last token.
- Dense wrapping of expressions (as well as removal of `'\\'` continuation) can be used to enforce
  uniform coding style. This is Hemlock's answer to endless coding style debate, but it is a
  draconian approach, and therefore opt-in. In the absence of dense wrapping, code formatting style
  guidelines are pretty minimal.

  + Wrap expressions such that lines do not exceed 80 columns if possible. (Don't bother with
    heroics such as string literal splitting unless it makes the code more readable.)
  + Prefer to densely wrap long expressions unless sparser wrapping significantly improves
    readability.
  + Use additional inter-token alignment spacing only if it significantly improves readability.
  + Use `'\\'` continuation sparingly.

## Tokens

### Whitespace

Whitespace comprises space (`' '`) and newline (`'\n'`) codepoints. Tabs, carriage returns, form
feeds, etc. are not treated as whitespace.

### Comment

Comments have two syntaxes.
- Single-line `#...` comments are delimited by a leading `#` and a trailing `\n` (or end of input).
- `(*...*)` comments are delimited by symmetric `(*` and `*)` sequences, which allows `(* comments
  (* to be *) nested *)`.

### Punctuation

Hemlock uses various symbols as punctuation:

```hemlock
. , ; : :: :=
( ) [ ] [| |] { } {| |}
\ ' ^ &
~ ?
| ~> -> >->
```

### Operator

In addition to punctuation, Hemlock supports an extensible set of operators. Operators are either
prefix or infix, as determined by the leading codepoint.

- Prefix operator: `[~?][-+*/%@!$<=>|:.~?]+`
- Infix operator: `[-+*/%@!$<=>|:.][-+*/%@!$<=>|:.~?]*` excluding punctuation symbols

### Keyword

The following words are keywords which are used as syntactic elements, and cannot be used for other
purposes.

```hemlock
and      external  match   val
also     false     module  when
as       for       of      while
assert   fun       open    with
conceal  function  or
do       if        rec
downto   import    then
effect   include   to
else     lazy      true
expose   let       type
```

### Identifier

The first non-underscore codepoint of an identifier helps distinguish namespaces. Identifiers which
begin with `'_'` are exempt from compiler warnings regarding unused values. `'_'` by itself is
special in that it creates no lexical binding at all.

- Capitalized identifiers match `[_]*[A-Z][A-Za-z0-9_']*`, and are used for module names and variant
  type constructors.
- Uncapitalized identifiers match `[_]*[a-z][A-Za-z0-9_']*`, and are used for value names, parameter
  label names, type names, and record field names.

### Integer

Integers are either signed or unsigned, though a leading sign is always a separate token. Integer
literals may be specified in any of four bases, as determined by optional base prefix:

- `0b`: Binary, where digits are in `[01]`.
- `0o`: Octal, where digits are in `[0-7]`.
- Default: Decimal, where digits are in `[0-9]`.
- `0x`: Hexadecimal, where digits are in `[0-9a-f]`.

`_` codepoints may be arbitrarily placed before/after any digit as desired to make literals easier
to read.

Integers are unsigned 64-bit by default, but may be unsigned/signed and of different bitwidth,
depending on optional type suffix:

- Unsigned:
  + `u8`: Unsigned 8-bit
  + `u16`: Unsigned 16-bit
  + `u32`: Unsigned 32-bit
  + `u64`/`u`: Unsigned 64-bit (`uns` type)
  + `u128`: Unsigned 128-bit
  + `u256`: Unsigned 256-bit
  + `u512`: Unsigned 512-bit
- Signed:
  + `i8`: Signed 8-bit
  + `i16`: Signed 16-bit
  + `i32`: Signed 32-bit
  + `i64`/`i`: Signed 64-bit (`int` type)
  + `i128`: Signed 128-bit
  + `i256`: Signed 256-bit
  + `i512`: Signed 512-bit

Examples:

- `uns`
  ```hemlock
  0
  42
  15u
  17u64
  0x0123_4567_89ab_cdef
  0o660
  0b10_0001
  0b0100_0001
  1_000_000
  0x___1_fffd
  ```
- `int`
  ```hemlock
  0i
  42i
  17i64
  0x_ab__c_i
  0o777
  ```
- `byte`
  ```hemlock
  0u8
  0xffu8
  ```

### Real

Real numbers use the [IEEE 754](https://en.wikipedia.org/wiki/IEEE_754) binary floating point
format. Real number literals are distinct from integer literals in at least one of the following
ways:

- Decimal point
- Exponent
- Type suffix

Reals are signed, though a leading sign is always a separate token. Real number literal mantissas
may be specified in any of four bases, as determined by optional base prefix:

- `0b`: Binary, where digits are in `[01]`. All well formed binary literals have bit-precise machine
  representations.
- `0o`: Octal, where digits are in `[0-7]`. All well formed octal literals have bit-precise machine
  representations.
- Default: Decimal, where digits are in `[0-9]`. Not all decimal-format literals have bit-precise
  machine representations; favor the other bases if this is of significance to the application.
- `0x`: Hexadecimal, where digits are in `[0-9a-f]`. All well formed hexadecimal literals have
  bit-precise machine representations.

Optional signed exponents are separated from a binary/octal/hexadecimal mantissa by a `p` codepoint,
or from a decimal mantissa by an `e` codepoint. The optional exponent sign is in `[-+]`. The
exponent is always expressed as a decimal value, where digits are in `[0-9]`, but the implicit base
of the exponent depends on the mantissa's base prefix:

- Binary (`0b<mantissa>p<exponent>`): *mantissa*<sub>2</sub> × 2<sup>*exponent*<sub>10</sub></sup>
- Octal (`0o<mantissa>p<exponent>`): *mantissa<sub>8</sub>* × 2<sup>*exponent<sub>10</sub>*</sup>
- Decimal (`<mantissa>e<exponent>`): *mantissa<sub>10</sub>* × 10<sup>*exponent<sub>10</sub>*</sup>
- Hexadecimal (`0x<mantissa>p<exponent>`): *mantissa<sub>16</sub>* ×
  2<sup>*exponent<sub>10</sub>*</sup>

`_` codepoints may be arbitrarily placed before/after any mantissa digit or exponent sign/digit as
desired to make literals easier to read.

Reals are 64-bit by default, but may be of different bitwidth, depending on optional type suffix:

- `r32`: 32-bit ["single-precision
  float"](https://en.wikipedia.org/wiki/Single-precision_floating-point_format)
- `r64`/`r`: 64-bit ["double-precision
  float"](https://en.wikipedia.org/wiki/Double-precision_floating-point_format) (`real` type)

Examples:

```hemlock
0.
0e0
0r
0r64
1.0
1_000_000.0
42.e44
42.3e-78
0b1.101p42
0o7.406p42
0x4.a3
0x4a.3d2p+42
1.5r32
0x4a.3d2p+42_r32
1.234_567_e_+89_r32
```

### Codepoint

Codepoint tokens are delimited by `'` codepoints, and their contents are interpolated for a limited
set of codepoint sequences.
- `\u{...}`: Hexadecimal-encoded codepoint, e.g. `\u{fffd}` is the `�` replacement codepoint.
- `\t`: Tab (`\u{9}`).
- `\n`: Newline, aka line feed (`\u{a}`).
- `\r`: Return (`\u{d}`).
- `\'`: Single quote.
- `\\`: Backslash.

Examples:

```hemlock
'A'
'\u{10197}' # '𐆗'
'\t'
'\r'
'\n'
'\''
'\\'
```

Note that `'` is also used as the sigil for type parameters, e.g. in `'a t`. Type parameter sigils
and codepoint literal delimiters never cause grammar ambiguity, but invalid source code may result
in surprising syntax errors because type parameters and codepoint literals have overlapping valid
prefixes.

### String

String tokens have three distinct syntaxes, all of which are useful depending on contents and
context:

- **Interpolated** strings are delimited by `"` codepoints, and their contents are interpolated for
  a limited set of codepoint sequences.
  ```hemlock
  "Interpolated string without any interpolated sequences"
  ```
  The following codepoint sequences are interpolated as indicated:
  + `\u{...}`: Hexadecimal-encoded codepoint, e.g. `\u{fffd}` is the `�` replacement codepoint.
  + `\t`: Tab (`\u{9}`).
  + `\n`: Newline, aka line feed (`\u{a}`).
  + `\r`: Return (`\u{d}`).
  + `\"`: Double quote.
  + `\\`: Backslash.
  + `\␤`: Newline omitted. This allows a string literal to be broken across lines without the line
    breaks becoming part of the string.
    ```hemlock
    "Single-line \
    interpolated string"
    ```
- **Raw** strings are delimited by matching `` `([^|`][^`]*)?` `` sequences, where the optional tag
  between the `` ` `` codepoints can be used to distinguish the delimiters from string contents.
  ```hemlock
  ``Simple raw string``
  `_`String would ``end prematurely`` without a tag`_`
  ```
  If the raw string begins and/or ends with a `\n`, that codepoint is omitted. This allows raw
  string delimiters to be on separate lines from the string contents without changing the string.
  ```hemlock
  ``Single-line raw string``
  ``
  Single-line raw string
  ``
  ```

  ```hemlock
  ``

  Three-line raw string

  ``
  ```
- **Bar-margin [raw] strings** are delimited by `` `| `` and a codepoint sequence matching `` ^[ ]*`
  ``. Each line past the first one is prefixed by enough whitespace to align a `|` with the opening
  delimiter's `|`. The per line leading whitespace and `|` are omitted from the string; they provide
  a left margin for the string contents.
  ```hemlock
  `|First line
   |Second line
  `
  ```
  Note that the final `\n` preceding the closing delimiter is itself part of the delimiter and is
  omitted.
  ```hemlock
  `|Single-line bar-margin string
  `
  `|Two-line bar-margin string
   |
  `
  ```

## Line directives

Token path/line/column locations are ordinarily a simple function of the source stream from which
they derive, but if the source stream is generated from another source, e.g. using a parser
generator, it can be useful to associate tokens with the pre-generated source locations. Line
directives provide a mechanism for setting the line and path for subsequent source lines. Line
directives are consumed by the scanner and no tokens result unless there is a syntax error in the
line directive. As such, the line directive syntax is extremely rigid. Line directives begin with
`:` at column 0, followed by positive decimal line number (leading zero prohibited), followed by
optional single space and double-quoted string path, terminated by newline.

Examples:

```hemlock
:42␤
:42 "foo.hm"␤
```
