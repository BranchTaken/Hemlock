# Syntax

Hemlock's syntax derives from the [ML](https://en.wikipedia.org/wiki/ML_(programming_language))
language family, which in turn is loosely based on the [lambda
calculus](https://en.wikipedia.org/wiki/Lambda_calculus) syntax. Hemlock adds syntax for distinct
features like algebraic effects. But the most striking visual difference is that Hemlock uses
indentation to rigidly define block structure.

## Encoding

Hemlock uses [UTF-8](https://en.wikipedia.org/wiki/UTF-8) encoding. A [byte order
mark](https://en.wikipedia.org/wiki/Byte_order_mark) prefix will cause a syntax error and should
therefore always be omitted. Encoding errors result in one or more `�` replacement codepoint
substitutions. Such replacements of invalid encoding are silently ignored inside comments, but they
explicitly cause scanning errors if inside codepoint/string literals, and elsewhere they result in
error tokens which cause parsing errors.

## Dentation

Semantically meaningful {in,de}dentation, dentation for short, has multiple advantages:

- Programmers typically indent code to indicate block structure, in order to streamline human
  understanding, even if the compiler is oblivious to dentation. Making dentation meaningful reduces
  the gap between the programmer's mental model and the language semantics.
- Fewer symbols are required. The omission of lines consisting only of `end` or `}` tends to reduce
  total number of lines, even as compared with a two-column dentation style. (Hemlock enforces
  four-column dentation; more on that below.)
- Automated code formatting tools are brain-dead simple as compared with the near-sentience required
  to implement typical coding styles in many contemporary languages.
- One of the most effective general heuristics for parser error recovery is to heed dentation and
  restart parsing at the enclosing indentation level. By mandating proper dentation, error recovery
  is yet more effective, and it becomes feasible to provide code introspection services even for
  source files in intermediate editing states.

Several other contemporary languages have semantically meaningful dentation, but approaches vary
considerably. The following languages have particularly interesting takes on the problem.

- [Python](https://www.python.org/) is the most widely used language for which dentation is
  significant, and the rules for what constitutes acceptable dentation are quite flexible. The
  details changed a bit between Python 2 and 3 to reduce ways in which tab width could obscure code
  meaning.
- [Haskell](https://www.haskell.org/) requires continuation of an expression to be indented more
  than its beginning token. The rules are simple and consistent; the biggest common pitfall is that
  if a multi-line expression starts on a line with preceding tokens that change length, the entire
  expression must be re-indented during refactoring.
- [F#](https://fsharp.org/) dentation is modeled as an "offside" rule similar to that used in
  soccer. Although elegant in concept, this approach motivates an unfortunate set of special cases
  which allow multi-line expressions to be indented "prettily", e.g. with math operators aligned.

Hemlock takes a comparatively simple approach to avoid some of the pitfalls mentioned above:

- Tabs can be embedded only in raw string literals and comments. This prevents tabs from impacting
  code structure.
- The first non-space codepoint on each line establishes its indentation.
- Block indentation is four columns.
- Expression continuation indentation is two columns. One-column and three-column indentation are
  *always* invalid, which eliminates undetected off-by-one errors.
- Lines comprising only comments and/or whitespace are ignored.
- Consecutive expressions at the same indentation are distinct expressions.

Autoformatting only needs to remove excessive whitespace and densely rewrap. With that done, there
are limited significant formatting style considerations, e.g.:

- Use block indentation of subexpressions for long expressions rather than dense wrapping when doing
  so improves readability.
- Use block indentation when its absence would cause confusion, e.g. non-trivial pattern match
  actions.
- Prefer to omit `(...)` delimiters when they do not increase code clarity.
- Prefer to order named parameters such that non-trivial `fn` expressions come last and use block
  indentation for their bodies.

Additional notes:

- Tabs *can* affect column numbering for code in lines that intersperse raw strings and/or comments.
  For the purposes of column computation the compiler fixes the tabstops at multiples of eight.
  There are two useful rules for avoiding confusion:
  + Don't do that. Raw tabs are rarely necessary, and they *never* need to be used in combination
    with trailing code on the same line.
  + Configure the terminal and code editor to use tabstops at multiples of eight. For example, use
    the following [EditorConfig](https://editorconfig.org/) settings in `~/.editorconfig`.
    ```
    [*.{hm,hmi}]
    indent_style = space
    indent_size = 4
    tab_width = 8
    charset = utf-8
    end_of_line = lf
    trim_trailing_whitespace = true
    insert_final_newline = false
    ```
- There are documentation-specific attributes for documentation strings, which can be extracted
  during documentation generation. Such strings are ingested as
  [Markdown](https://github.github.com/gfm/), and documentation generation could well fail on
  malformed input. For the purposes of code compilation such strings are opaque, but autoformatting
  could in principle normalize indentation and wrapping. However the complexity is rather high for a
  limited payoff.

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
. .. , ; : :: :=
( ) (| |) [ ] [| |] { }
| \ ' ^ < <= = <> >= >
! &
~ ?
-> ~->
```

### Operator

In addition to punctuation, Hemlock supports an extensible set of operators. Operators are either
prefix or infix, as determined by the leading codepoint.

- Prefix operator: `[~?][-+*/%@^$<=>|:.~?]+`
- Infix operator: `[-+*/%@^$<=>|:.][-+*/%@$<=>|:.~?]*` excluding punctuation symbols

### Precedence and associativity

Precedence and associativity varies somewhat by use for some punctuation. For example, the
precedence of `>` is not important when used as a prefix for an effect name binding as in
`>(e:effect)`, because there are no nearby syntactic elements for which precedence can change how
valid code is parsed.

Type/binding construction:

| Operator      | Associativity |
| :-----------: | :-----------: |
| `,`           | —             |
| `->`, `~->`   | right         |
| `as`          | —             |
| `!`, `&`      | —             |
| `'`, `^`, `>` | —             |

Pattern construction:

| Operator            | Associativity |
| :-----------------: | :-----------: |
| `lazy`              | —             |
| Variant application | right         |
| `::`                | right         |
| `,`                 | —             |
| `\|`                | left          |
| `as`                | —             |

Expressions:

| Operator                                              | Associativity |
| :---------------------------------------------------: | :-----------: |
| `.`                                                   | —             |
| Function/variant application, `lazy`                  | left          |
| `-` (prefix), `~`..., `?`..., `!`                     | —             |
| `'` (prefix), `^` (prefix), `>` (prefix)              | —             |
| `**`...                                               | right         |
| `*`..., `/`..., `%`...                                | left          |
| `+`..., `-`...                                        | left          |
| `::`, `:`...                                          | right         |
| `@`..., `^`...                                        | right         |
| `=`..., `<`..., `>`..., `\|`..., `$`..., `.`...       | left          |
| `and`                                                 | right         |
| `or`                                                  | right         |
| `,`                                                   | —             |
| `:=`                                                  | right         |
| `if`                                                  | —             |
| `;`                                                   | right         |
| `..`                                                  | —             |
| `import`                                              | —             |
| `open`                                                | —             |
| `let`, `match`, `fn`, `function`, `expose`, `conceal` | —             |

### Keyword

The following words are keywords which are used as syntactic elements, and cannot be used for other
purposes.

| Keywords   |          |              |           |          |
| :--------- | :------- | :----------- | :-------- | :------- |
| `and`      | `also`   | `as`         | `conceal` | `effect`
| `else`     | `expose` | `external`   | `false`   | `fn`
| `function` | `if`     | `import`     | `include` | `lazy`
| `let`      | `match`  | `mutability` | `of`      | `open`
| `or`       | `rec`    | `then`       | `true`    | `type`
| `when`     | `with`

### Identifier

The first non-underscore codepoint of an identifier helps distinguish namespaces. Identifiers which
begin with `'_'` are exempt from compiler warnings regarding unused values. `'_'` by itself is
special in that it creates no lexical binding at all.

- Capitalized identifiers match `[_]*[A-Z][A-Za-z0-9_']*`, and are used for module names and variant
  type constructors.
- Uncapitalized identifiers match `[_]*[a-z][A-Za-z0-9_']*`, and are used for value names, parameter
  label names, type names, and record field names.
- The toolchain reserves identifiers with `__` suffix for internal use; using such identifiers in
  application code risks undefined behavior.

### Integer

Integers are either signed or unsigned, though a leading sign is always a separate token. Integer
literals may be specified in any of four radixes, as determined by optional radix prefix:

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
  + `n`: Arbitrary-precision 0-inclusive natural (ℕ₀, `nat` type)
- Signed:
  + `i8`: Signed 8-bit
  + `i16`: Signed 16-bit
  + `i32`: Signed 32-bit
  + `i64`/`i`: Signed 64-bit (`int` type)
  + `i128`: Signed 128-bit
  + `i256`: Signed 256-bit
  + `i512`: Signed 512-bit
  + `z`: Arbitrary-precision integer (ℤ, `zint` type)

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

- Radix point
- Exponent
- Type suffix

Reals are signed, though a leading sign is always a separate token. Real number literal mantissas
may be specified in any of four radixes, as determined by optional radix prefix:

- `0b`: Binary, where digits are in `[01]`. All well formed binary literals have bit-precise machine
  representations.
- `0o`: Octal, where digits are in `[0-7]`. All well formed octal literals have bit-precise machine
  representations.
- Default: Decimal, where digits are in `[0-9]`. Not all decimal-format literals have bit-precise
  machine representations; favor the other radixes if this is of significance to the application.
- `0x`: Hexadecimal, where digits are in `[0-9a-f]`. All well formed hexadecimal literals have
  bit-precise machine representations.

Optional signed exponents are separated from a binary/octal/hexadecimal mantissa by a `p` codepoint,
or from a decimal mantissa by an `e` codepoint. The optional exponent sign is in `[-+]`. The
exponent is always expressed as a decimal value, where digits are in `[0-9]`, but the implicit radix
of the exponent depends on the mantissa's radix prefix:

- Binary (`0b<mantissa>p<exponent>`): *mantissa<sub>2</sub>* × 2<sup>*exponent<sub>10</sub>*</sup>
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
- `\t`: Tab (`\u{9}`). Raw tab codepoints are prohibited.
- `\n`: Newline, aka line feed (`\u{a}`). Raw newline codepoints are not supported, because `'␤` is
  considered a whitespace-delimited implicit type parameter sigil.
- `\r`: Return (`\u{d}`). Raw return codepoints are prohibited.
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

Note that `'` is also used as the sigil for type parameters, e.g. in `type 'a t:t a`. Type parameter
sigils and codepoint literal delimiters never cause grammar ambiguity, but invalid source code may
result in surprising syntax errors because type parameters and codepoint literals have overlapping
valid prefixes.

### String

String tokens have three distinct syntaxes, all of which are useful depending on contents and
context:

- **Raw** strings are delimited by matching `` `[A-Za-z0-9_']*` `` sequences, where the optional tag
  between the `` ` `` codepoints can be used to distinguish the delimiters from string contents.
  ```hemlock
  ``Simple raw string``
  `_`String would ``end prematurely`` without a tag`_`
  ```
- **Interpolated** strings are delimited by `"` codepoints, and their contents are interpolated for
  a limited set of codepoint sequences.
  ```hemlock
  "Interpolated string without any interpolated sequences"
  "Interpolated string with \"embedded quotes\" and\na newline"
  ```
  The following codepoint sequences are interpolated as indicated:
  + `\u{...}`: Hexadecimal-encoded codepoint, e.g. `\u{fffd}` is the `�` replacement codepoint.
  + `\t`: Tab (`\u{9}`). Raw tab codepoints are prohibited.
  + `\n`: Newline, aka line feed (`\u{a}`).
  + `\r`: Return (`\u{d}`). Raw return codepoints are prohibited.
  + `\"`: Double quote.
  + `\\`: Backslash.
  + `\%`: Percent.
  + `\␤`: Newline omitted. This allows a string literal to be broken across lines without the line
    breaks becoming part of the string.
    ```hemlock
    "Single-line \
    interpolated string"
    ```

- **Formatted** strings provide syntactic sugar for formatting. Formatted strings support all the
  interpolation syntax of interpolated strings, but they additionally contain one or more format
  specifiers. Format specifiers support unrestricted embedded code expressions, which means that
  formatted strings can nest arbitrarily deeply. The delimiters for embedded expressions are
  unambiguous relative to all other language syntax, which enables tokenization without parser
  feedback. The format specifiers include enough explicit type information that desugaring requires
  no type inference. Nonetheless, the scanner requires considerable sophistication to track nesting
  and state transitions between the tokens which together logically comprise one formatted string.

  ##### Supported types

  The following types are supported by format specifiers:

  + `bool`
  + Numeric types (`uns`, `int`, `[ui](8|16|32|64|128|256|512)`, `nat`, `zint`, `real`, `r(32|64)`)
  + `codepoint`
  + `string`
  + Partially applied formatter functions of type `>(e:effect) -> t -> Fmt.Formatter e >e->
    Fmt.Formatter e`, where `t` is the type of the value expression

  Composite polymorphic types like `type List 'a: List a` provide formatters which can be composed
  to produce partially applied formatter functions, as shown later.

  ##### Desugaring

  The following example shows how a string containing format specifiers is desugared.

  ```hemlock
  answer = 42

  # Formatted string.
  "Hello %s(^"Fred"^), %u=(^answer^) and this is a list: %f(^List.fmt Uns.fmt^)(^[0; 1; 2]^)"

  # Desugared form.
  Basis.String.Fmt.empty
    |> Basis.Fmt.fmt "Hello "
    |> Basis.String.fmt ("Fred")
    |> Basis.Fmt.fmt ", answer="
    |> Basis.Uns.fmt (answer)
    |> Basis.Fmt.fmt " and this is a list: "
    |> (List.fmt Uns.fmt) ([0; 1; 2])
    |> Basis.Fmt.to_string
  ```

  Clearly a sufficiently compatible `Basis` module must be linked with the application for
  compilation to succeed in the presence of format specifiers. Additionally, the embedded code must
  be properly indented if broken across multiple lines. The outermost indentation level for embedded
  code is one level deeper than that of the enclosing string. Note that the seemingly superfluous
  parentheses surrounding the embedded expressions assure that invalid indentation within embedded
  code will cause a localized syntax error rather than allowing for potentially confusing syntax
  errors in code that follows.

  ```hemlock
  # Formatted string.
  s = "0-space indentation, %s(^
      "4-space indentation, %u(^
          8
        ^)-space indentation"
    ^)."

  # Desugared form.
  s = Basis.String.Fmt.empty
    |> Basis.Fmt.fmt "0-space indentation, "
    |> Basis.String.fmt (
      Basis.String.Fmt.empty
        |> Basis.Fmt.fmt "4-space indentation, "
        |> Basis.Uns.fmt (
          8
        )
        |> Basis.Fmt.fmt "-space indentation"
        |> Basis.Fmt.to_string
    )
    |> Basis.Fmt.fmt "."
    |> Basis.Fmt.to_string
  ```

  ##### Syntax

  A format specifier is embedded in a formatted string as `%<specifier>(^...^)`. Specifier
  options are desugared to optional function parameters, e.g. `#` becomes `~alt:true`. Some
  specifiers are only supported by a subset of types, e.g. zero padding will cause a compilation
  error if used with a string value (`"%0s(^"hello"^)"`). Formatter functions, whether those
  implemented in `Basis` or in the application, may implement parameter support for any desired
  combination of parameters; in all cases specifying parameters which are not supported by the
  formatter function being called will cause compilation failure.

  Format specifiers are of the form:
  ```
  %['<pad>'][<just>][<sign>][<alt>][<zpad>][<width>][.=?<precision>][<radix>][<notation>][<pretty>][<fmt>][<sep>](^...^)
  ```
  + `'<pad>'` (`?pad:codepoint`): Pad with specified codepoint (default: `' '`; complete codepoint
    literal syntax supported)
  + `<just>` (`?just:Fmt.just`): Justify (default: `Fmt.Right`)
    * `<`: Left
    * `^`: Center
    * `>`: Right
  + `<sign>` (`?sign:Fmt.sign`): Sign (default: `Fmt.Implicit`)
    * `+`: Explicit sign, even when positive
    * `_`: Space in place of sign if sign is non-negative
  + `<alt>` (`?alt:bool`): `#` enables alternate formatting (default: `false`)
    * Numeric types: Prefix with radix, separate digit groups with `_`
      - Binary: `0b` prefix, groups of 8
      - Octal: `0o` prefix, groups of 3
      - Decimal: No prefix, groups of 3
      - Hexadecimal: `0x` prefix, groups of 4
    * String (with `<pretty>`): ``` ``raw`` ```, with auto-generated ``` `tag`...`tag` ``` as needed
    * Container types (`list a`, `array a`): Multi-line block-based formatting, where `width` is
      interpreted as the starting indentation
  + `<zpad>` (`?zpad:bool`): `0` enables padding numeric type with leading zeros between sign/prefix
    and non-zero digits (default: `false`)
  + `<width>` (`?width:uns`): Minimum width (default: `0`)
    * `42`: Fixed width in codepoints
    * `*`: Parametric width in codepoints, e.g. `%*(^width^)u(^some_u^)`
  + `.=?<precision>` (`?pmode:Fmt.pmode`, `?precision:uns`): Number of digits past the radix point,
    where `=` enables `pmode=Fmt.Fixed` precision mode, i.e. the potential for trailing zeros (default:
    `pmode=Fmt.Limited`, `precision`: `52`/`18`/`15`/`13` or `53`/`18`/`3`/`14` for
    binary/octal/decimal/hexadecimal normalized or radix point form, respectively)
    * `.=3`: Fixed precision
    * `.3`: Limited precision
    * `.*`: Parametric limited precision in digits, e.g. `%*(^width^).*(^precision^)r(^some_r^)`
  + `<radix>` (`?radix:Radix.t`): Numerical radix (default: `Radix.Dec`)
    * `b`: Binary
    * `o`: Octal
    * `d`: Decimal
    * `x`: Hexadecimal
  + `<notation>` (`?notation:Fmt.notation`): Real-specific notation (default: `Fmt.Compact`)
    * `m`: Normalized scientific form, i.e. decimal exponential or binary floating point notation
      (mnemonic: Mantissa × radix <sup>exponent</sup>)
    * `a`: Radix point form (mnemonic: rAdix point)
    * `c`: The more compact of normalized vs radix point forms (mnemonic: Compact)
  + `<pretty>` (`?pretty:bool`): `p` enables pretty-printing as if a lexical token (default:
    `false`)
    * Numeric types: Append type suffix
    * Codepoint: `'c'`, special codepoints escaped
    * String: `"some string"`/``` ``some string`` ```, depending on `<alt>`
  + `<fmt>`: Formatter to use, designated via type abbreviation or nested expression
    * `b`: `bool` (`%b(^...^)` is unambiguous with respect to e.g. binary-formatted `uns` —
      `%bu(^...^)`)
    * `[ui](8|16|32|64|128|256|512)?`, `n`, `z`, `r(32|64)?`: Numeric type of corresponding literal
      suffix
    * `c`: `codepoint` (`%c(^...^)` is unambiguous with respect to e.g. compact-formatted `real` —
      `%cr(^...^)`)
    * `s`: `string`
    * `f(^...^)`: Partially applied formatter of type `>(e:effect) -> t -> Fmt.Formatter e >e->
      Fmt.Formatter e`, where `t` is the type of the value expression
  + `<sep>`: Separator between stringified representation of value and its formatted
    representation, i.e. `<stringified><sep><repr>`, where `<sep>` matches `[ ]*<infix_op>[ ]*` and
    `<infix_op>` matches an infix operator

  Examples:

  ```hemlock
  "%s(^name^)"                              # "Fred"
  "%#xu=(^age^)"                            # "age=0x2a"
  "%u(^succ age^)"                          # "43"
  "%pz=(^x^)"                               # "x=42z"
  "%016xu(^some_uns^)"                      # "000000000000002a"
  "%' '^*(^page_width^)s(^title^)\n"        # "  Some Book Title  "
  "(%'*'98s(^""^))"                         # Length-100 "(**...**)" string.
  "%#xu=(^x^) > %#xu=(^y^) -> %b(^x > y^)"  # "x=0x2a > y=0x2b -> false"
  "%#bu(^x^) %#ou(^x^) %#du(^x^) %#xu(^x^)" # "0b1111 0o17 15 0xf"
  "%f(^List.fmt String.pp^)=(^children^)"   # "children=[\"Alice\"; \"Bob\"]"
  "%^24f(^List.fmt String.pp^)(^children^)" # "    [\"Alice\"; \"Bob\"]    "
  "%^24f(^List.fmt String.pp^)(^
      List.mapi children ~f:fn i child ->
          "%u(^succ i^):%s(^child^)"
    ^)"                                     # "  [\"1:Alice\"; \"2:Bob\"]  "
  ```

## Source directives

Token path/line/column locations are ordinarily a simple function of the source stream from which
they derive, but if the primary source stream comprises embedded sources, e.g. as the product of a
parser generator, it can be useful to associate tokens with the unembedded source locations. Source
directives provide a mechanism for setting the path, line, block indentation, and column for
embedded source. Although the scanner accepts source directive tokens much as any other token, the
main purpose of such tokens is to affect scanner state, and the parser ignores them.

Source directives are delimited by `[:`...`]` and comprise optional colon-separated ordered
parameters, matched by `\[:<path>[:<line>[:<indent>+<omit>]]|:<line>[:<indent>+<omit>]|:\]`:

- Source `<path>`: `"..."`-delimited interpolated string defaults to current source path
- `<line>`: `_*[1-9][0-9_]*`, constrained to the range of `int`, defaults to 1
- `<indent>`: `_*[0-9][0-9_]*`, constrained to the range of `int`, defaults to 0, specifies block
  indentation column of `<line>`, which must be a multiple of 4
- `<omit>`: `_*[0-9][0-9_]*`, constrained to the range of `int`, defaults to 0, specifies number of
  codepoints omitted past `<indent>` column, i.e. `<indent>+<omit>` is the starting column

Every directive resets the source to the primary source and restores the indentation to its value at
the previous source directive, because the source directive itself naturally originates in the
primary source. An empty source directive, `[:]`, has no other effect, but a non-empty source
directive:

- sets the position of the codepoint immediately following the directive, even if it is a newline
- specifies block indentation of the first line, which allows what would otherwise be malformed
  indentation transitions between sources

```hemlock
[:"Foo.hm":42:8+13]
[:"Foo.hm":42]
[:"Foo.hm"]
[:42:8+13]
[:42]
[:]
```

### Practical example

The following practical example expands `Example.hmhi` as though it were a `hocc` interface file
input. The `hocc` keyword expands to a module signature. Note the source directives immediately
enclosing the generated expansion, such that the surrounding code is attributed to "Example.hmhi",
whereas the generated code originates in "Example.hmi"

- "Example.hmhi"
```hocc
open import Basis

include hocc

calculate: string -> zint
```

- "Example.hmi"
```hemlock
# This file was generated by `hocc` based on "Example.hmhi"
[:"Example.hmhi":1]open import Basis

include [:]{
    # Example grammar module signature.
    Spec = {
        # [...]
      }
    Token = {
        # [...]
      }
    # Etc.
  }[:"Example.hmhi":3:0+12]

calculate: string -> zint
```

### Contrived example

The following contrived example imagines that there's an inlining preprocessor which macro-expands
stylized `[@inline]` functions.

- "Tesseract.hmp"
```hemlock
open Basis

square [@inline] = (fn x =
    x * x
  )

tesseract x =
    (square (square x))
```

- "Tesseract.hm"
```hemlock
[:"Tesseract.hmp":1]open Basis

square [@inline] = (fn x =
    x * x
  )

tesseract x =
    ([:"Tesseract.hmp":3:0+19](fn x =
    x * x
  )[:"Tesseract.hmp":8:4+7] ([:"Tesseract.hmp":3:0+19](fn x =
    x * x
  )[:"Tesseract.hmp":8:4+15] x))
```

Of note:

- The inlined body of `square` is under-indented with respect to the surrounding fragments of
  `tesseract`, but that causes no issues because the source directives specify fragments'
  indentation.
- `[:]` is not a useful prefix to ` (` between the `square` tokens, because `[:]` would reset the
  source to "Tesseract.hm" rather than "Tesseract.hmp".
- The parentheses surrounding `(fn x = ...)` are critical, because they assure that dentation is
  neutral, i.e. all indent/dedent tokens are paired.

## Grammar

<a name="expr">[expr](#expr)</a> ::=
<br>&nbsp;&nbsp;&nbsp;&nbsp;| `(` [expr](#expr) `)`
<br>&nbsp;&nbsp;&nbsp;&nbsp;| `(|` [expr](#expr) `|)`
<br>&nbsp;&nbsp;&nbsp;&nbsp;| `[` [expr](#expr) `..` [expr](#expr) `)`
<br>&nbsp;&nbsp;&nbsp;&nbsp;| `[` [expr](#expr) `..` [expr](#expr) `]`
<br>&nbsp;&nbsp;&nbsp;&nbsp;| XXX
