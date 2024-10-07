# https://github.com/BranchTaken/Hemlock
# ‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾

# Detection
# ‾‾‾‾‾‾‾‾‾

hook global BufCreate .*\.hmhi? %{
    set-option buffer filetype hocc
}

# Initialization
# ‾‾‾‾‾‾‾‾‾‾‾‾‾‾

hook global WinSetOption filetype=hocc %{
    require-module hocc
    set-option window static_words %opt{hocc_static_words}
}

hook -group hocc-highlight global WinSetOption filetype=hocc %{
    add-highlighter window/hocc ref hocc
    hook -once -always window WinSetOption filetype=.* %{ remove-highlighter window/hocc }
}

provide-module hocc %{

# Highlighters
# ‾‾‾‾‾‾‾‾‾‾‾‾


add-highlighter shared/hocc regions
add-highlighter shared/hocc/code default-region group

add-highlighter shared/hocc/code/cident regex \b[_]*[A-Z][A-Za-z0-9_']*\b 0:module
add-highlighter shared/hocc/code/uident regex \b[_]*[a-z][A-Za-z0-9_']*\b 0:Default
add-highlighter shared/hocc/code/tab regex \t 0:Error
add-highlighter shared/hocc/code/unaligned regex ^(\ \ )*\ (?![\ ]) 0:Error
add-highlighter shared/hocc/code/unaligned_continue_keyword regex ^(\ \ \ \ )*(and|also|as|else|external|of|or|then|when|with)\b 0:Error
add-highlighter shared/hocc/code/unaligned_continue_punctuation regex ^(\ \ \ \ )*([\x7D\]),!'\-+*/%@$<=>\|:.]) 0:Error
add-highlighter shared/hocc/code/unaligned_continue_caret regex ^(\ \ \ \ )*([\^](?![&A-Za-z_])) 0:Error
add-highlighter shared/hocc/code/trailing regex (\ )+$ 0:ExcessWhitespace
add-highlighter shared/hocc/code/interior_multispace regex (?<=\S)(\ ){2,}(?=\S) 0:ExcessWhitespace

add-highlighter shared/hocc/comment region -recurse \Q(* \Q(* \Q*) fill comment
add-highlighter shared/hocc/line_comment region '#' '\n' fill comment

add-highlighter shared/hocc/string region ((?<!')"|\^\)) ((?<!\\)(\\\\)*"|\(\^) regions
add-highlighter shared/hocc/string/ default-region fill string

add-highlighter shared/hocc/string/escape region [\\]([tnr"%\\]|u\{[0-9a-f]{1,6}\}) () fill meta
add-highlighter shared/hocc/string/width region \%('.')?[<^>]?(\+|_)?#?0?\*\(\^ () fill meta
add-highlighter shared/hocc/string/precision region \%('.')?[<^>]?(\+|_)?#?0?([1-9][0-9]*)?\.=?\*\(\^ () fill meta
add-highlighter shared/hocc/string/fmt region \%('.')?[<^>]?(\+|_)?#?0?([1-9][0-9]*)?(\.=?[1-9][0-9]*)?[bodx]?[mac]?p?f\(\^ () fill meta
add-highlighter shared/hocc/string/value region \%('.')?[<^>]?(\+|_)?#?0?([1-9][0-9]*)?(\.=?[1-9][0-9]*)?[bodx]?[mac]?p?([bnzcs]|([ui](8|16|32|64|128|256|512)?)|(r(32|64)?))([\ ]*[-+*/%@^$<=>|:.][-+*/%@$<=>|:.~?]*[\ ]*)?\(\^ () fill meta

add-highlighter shared/hocc/string/width_precision region \^\)\.=?\*\(\^ () fill meta
add-highlighter shared/hocc/string/width_fmt region \^\)(\.=?[1-9][0-9]*)?[bodx]?[mac]?p?f\(\^ () fill meta
add-highlighter shared/hocc/string/width_value region \^\)(\.=?[1-9][0-9]*)?[bodx]?[mac]?p?([bnzcs]|([ui](8|16|32|64|128|256|512)?)|(r(32|64)?))([\ ]*[-+*/%@^$<=>|:.][-+*/%@$<=>|:.~?]*[\ ]*)?\(\^ () fill meta
add-highlighter shared/hocc/string/precision_fmt region \^\)[bodx]?[mac]?p?f\(\^ () fill meta
add-highlighter shared/hocc/string/precision_value region \^\)[bodx]?[mac]?p?([bnzcs]|([ui](8|16|32|64|128|256|512)?)|(r(32|64)?))([\ ]*[-+*/%@^$<=>|:.][-+*/%@$<=>|:.~?]*[\ ]*)?\(\^ () fill meta
add-highlighter shared/hocc/string/fmt_value region \^\)([\ ]*[-+*/%@^$<=>|:.][-+*/%@$<=>|:.~?]*[\ ]*)?\(\^ () fill meta

add-highlighter shared/hocc/string/unprotected region (?<!\\)% () fill Error
add-highlighter shared/hocc/string/overprotected region \\(?![utnr"\\%\n]) () fill Error

# Anchoring to the beginning of the input (\A) doesn't work as expected here. It appears that
# kakoune is not resetting the input bounds to exclude preceding codepoints on the first line of
# region input.
#add-highlighter shared/hocc/string/caret_rparen region \A\^\) () fill meta
add-highlighter shared/hocc/string/caret_rparen region \^\) () fill meta
add-highlighter shared/hocc/string/dquote region '"' () fill meta

add-highlighter shared/hocc/raw_string region -match-capture '`([a-z_])*`' '`([a-z_])*`' fill string

add-highlighter shared/hocc/codepoint region [']([^'\\]|(\\[tnr'\\])|(\\u\{[a-f0-9]{1,6}\}))['] () regions
add-highlighter shared/hocc/codepoint/ default-region fill string
add-highlighter shared/hocc/codepoint/tick region ['] () fill meta
add-highlighter shared/hocc/codepoint/escape region \\[tnr'\\] () fill meta
add-highlighter shared/hocc/codepoint/unicode region \\u\{[0-9a-f]{1,6}\} () fill meta

add-highlighter shared/hocc/code/punctuation regex %{[{}[\]()|;.,&!'\\]} 0:operator
add-highlighter shared/hocc/code/prefix_operator regex %{[~?][-+*/%@^$<=>|:.~?]*} 0:operator
add-highlighter shared/hocc/code/infix_operator regex %{[-+*/%@^$<=>|:.][-+*/%@^$<=>|:.~?]*} 0:operator

add-highlighter shared/hocc/code/boolean regex \b(true|false)\b 0:value

add-highlighter shared/hocc/code/bin_integer regex \b(0b)([_]*[01][01_]*)(([ui](8|16|32|64|128|256|512)?)|[zn])?\b 1:attribute 2:value 3:attribute
add-highlighter shared/hocc/code/oct_integer regex \b(0o)([_]*[0-7][0-7_]*)(([ui](8|16|32|64|128|256|512)?)|[zn])?\b 1:attribute 2:value 3:attribute
add-highlighter shared/hocc/code/hex_integer regex \b(0x)([_]*[0-9a-f][0-9a-f_]*)(([ui](8|16|32|64|128|256|512)?)|[zn])?\b 1:attribute 2:value 3:attribute
add-highlighter shared/hocc/code/integer regex \b(([1-9][0-9_]*)|0[_]*)(([ui](8|16|32|64|128|256|512)?)|[zn])?\b 1:value 3:attribute

add-highlighter shared/hocc/code/bin_real_dot regex \b(0b)([01][01_]*\.(?!\.)[01_]*(p_*[+\-]?_*[0-9][0-9_]*)?)(r(32|64)?)? 1:attribute 2:value 3:attribute
add-highlighter shared/hocc/code/bin_real_p regex \b(0b)([01][01_]*p_*[+\-]?_*[0-9][0-9_]*)(r(32|64)?)?\b 1:attribute 2:value 3:attribute
add-highlighter shared/hocc/code/bin_real_r regex \b(0b)([01][01_]*)(r(32|64)?)\b 1:attribute 2:value 3:attribute

add-highlighter shared/hocc/code/oct_real_dot regex \b(0o)([0-7][0-7_]*\.(?!\.)[0-7_]*(p_*[+\-]?_*[0-9][0-9_]*)?)(r(32|64)?)? 1:attribute 2:value 3:attribute
add-highlighter shared/hocc/code/oct_real_p regex \b(0o)([0-7][0-7_]*p_*[+\-]?_*[0-9][0-9_]*)(r(32|64)?)?\b 1:attribute 2:value 3:attribute
add-highlighter shared/hocc/code/oct_real_r regex \b(0o)([0-7][0-7_]*)(r(32|64)?)\b 1:attribute 2:value 3:attribute

add-highlighter shared/hocc/code/hex_real_dot regex \b(0x)([0-9a-f][0-9a-f_]*\.(?!\.)[0-9a-f_]*(p_*[+\-]?_*[0-9][0-9_]*)?)(r(32|64)?)? 1:attribute 2:value 3:attribute
add-highlighter shared/hocc/code/hex_real_p regex \b(0x)([0-9a-f][0-9a-f_]*p_*[+\-]?_*[0-9][0-9_]*)(r(32|64)?)?\b 1:attribute 2:value 3:attribute
add-highlighter shared/hocc/code/hex_real_r regex \b(0x)([0-9a-f][0-9a-f_]*)(r(32|64)?)\b 1:attribute 2:value 3:attribute

add-highlighter shared/hocc/code/real_dot regex \b([0-9][0-9_]*\.(?!\.)[0-9_]*(e_*[+\-]?_*[0-9][0-9_]*)?)(r(32|64)?)? 1:value 2:attribute
add-highlighter shared/hocc/code/real_e regex \b([0-9][0-9_]*e_*[+\-]?_*[0-9][0-9_]*)(r(32|64)?)?\b 1:value 2:attribute
add-highlighter shared/hocc/code/real_r regex \b([0-9][0-9_]*)(r(32|64)?)\b 1:value 2:attribute

# Macro
# ‾‾‾‾‾

evaluate-commands %sh{
  keywords="and|also|as|conceal|effect|else|expose|external|fn|function|if|import|include|lazy|let"
  keywords="${keywords}|match|mutability|of|open|or|rec|then|type|when|with"
  keywords="${keywords}|hocc|token|nonterm|start|epsilon|neutral|left|right|prec"

  printf %s\\n "declare-option str-list hocc_static_words ${keywords}" | tr '|' ' '

  printf %s "
    add-highlighter shared/hocc/code/ regex \b(${keywords})\b 0:keyword
  "
}

# Conveniences
# ‾‾‾‾‾‾‾‾‾‾‾‾

}

# Expand '(*' to '(*  *)' in input mode. Enter input mode with '\' prefix to avoid this hook.
#                   /\
hook global WinSetOption filetype=hocc %{
    hook window InsertChar '\*' %{
        try %{
            execute-keys -draft 'HH<a-k>\(\*<ret>'
            execute-keys '  *)<left><left><left>'
        }
    }
}
