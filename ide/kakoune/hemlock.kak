# https://github.com/BranchTaken/Hemlock
# ‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾

# Detection
# ‾‾‾‾‾‾‾‾‾

hook global BufCreate .*\.hmi? %{
    set-option buffer filetype hemlock
}

# Initialization
# ‾‾‾‾‾‾‾‾‾‾‾‾‾‾

hook global WinSetOption filetype=hemlock %{
    require-module hemlock
    set-option window static_words %opt{hemlock_static_words}
}

hook -group hemlock-highlight global WinSetOption filetype=hemlock %{
    add-highlighter window/hemlock ref hemlock
    hook -once -always window WinSetOption filetype=.* %{ remove-highlighter window/hemlock }
}

provide-module hemlock %{

# Highlighters
# ‾‾‾‾‾‾‾‾‾‾‾‾


add-highlighter shared/hemlock regions
add-highlighter shared/hemlock/code default-region group
 
add-highlighter shared/hemlock/code/cident regex \b[_]*[A-Z][A-Za-z0-9_']*\b 0:module
add-highlighter shared/hemlock/code/uident regex \b[_]*[a-z][A-Za-z0-9_']*\b 0:Default
add-highlighter shared/hemlock/code/tab regex \t 0:Error

add-highlighter shared/hemlock/line_directive region '^:[1-9][0-9]*' '\n' fill meta

add-highlighter shared/hemlock/comment region -recurse \Q(* \Q(* \Q*) fill comment
add-highlighter shared/hemlock/line_comment region '#' '\n' fill comment

add-highlighter shared/hemlock/string region ((?<!')"|\^\)) ((?<!\\)(\\\\)*"|\(\^) regions
add-highlighter shared/hemlock/string/ default-region fill string

add-highlighter shared/hemlock/string/escape region [\\]([tnr"%\\]|u\{[0-9a-f]{1,6}\}) () fill meta
add-highlighter shared/hemlock/string/width region \%('.')?[<^>]?(\+|_)?#?0?\*\(\^ () fill meta
add-highlighter shared/hemlock/string/precision region \%('.')?[<^>]?(\+|_)?#?0?([1-9][0-9]*)?\.=?\*\(\^ () fill meta
add-highlighter shared/hemlock/string/fmt region \%('.')?[<^>]?(\+|_)?#?0?([1-9][0-9]*)?(\.=?[1-9][0-9]*)?[bodx]?[mac]?p?\(\^ () fill meta
add-highlighter shared/hemlock/string/value region \%('.')?[<^>]?(\+|_)?#?0?([1-9][0-9]*)?(\.=?[1-9][0-9]*)?[bodx]?[mac]?p?([bnzcs]|([ui](8|16|32|64|128|256|512)?)|(r(32|64)?))([\ ]*[-+*/%@^$<=>|:.][-+*/%@$<=>|:.~?]*[\ ]*)?\(\^ () fill meta
add-highlighter shared/hemlock/string/inner_precision region \^\)\.=?\*\(\^ () fill meta
add-highlighter shared/hemlock/string/inner_fmt region \^\)([\ ]*[-+*/%@^$<=>|:.][-+*/%@$<=>|:.~?]*[\ ]*)?\(\^ () fill meta
add-highlighter shared/hemlock/string/inner_value region \^\)(\.=?[1-9][0-9]*)?[bodx]?[mac]?p?([bnzcs]|([ui](8|16|32|64|128|256|512)?)|(r(32|64)?))([\ ]*[-+*/%@^$<=>|:.][-+*/%@$<=>|:.~?]*[\ ]*)?\(\^ () fill meta

add-highlighter shared/hemlock/string/unprotected region (?<!\\)% () fill Error
add-highlighter shared/hemlock/string/overprotected region \\(?![utnr"\\%]) () fill Error

# Anchoring to the beginning of the input (\A) doesn't work as expected here. It appears that
# kakoune is not resetting the input bounds to exclude preceding codepoints on the first line of
# region input.
#add-highlighter shared/hemlock/string/caret_rparen region \A\^\) () fill meta
add-highlighter shared/hemlock/string/caret_rparen region \^\) () fill meta
add-highlighter shared/hemlock/string/dquote region '"' () fill meta

add-highlighter shared/hemlock/raw_string region -match-capture '`([a-z_])*`' '`([a-z_])*`' fill string
add-highlighter shared/hemlock/bar_string region '`[|]' '^[ ]*`' fill string

add-highlighter shared/hemlock/codepoint region [']([^'\\]|(\\[tnr'\\])|(\\u\{[a-f0-9]{1,6}\}))['] () regions
add-highlighter shared/hemlock/codepoint/ default-region fill string
add-highlighter shared/hemlock/codepoint/tick region ['] () fill meta
add-highlighter shared/hemlock/codepoint/escape region \\[tnr'\\] () fill meta
add-highlighter shared/hemlock/codepoint/unicode region \\u\{[0-9a-f]{1,6}\} () fill meta

add-highlighter shared/hemlock/code/punctuation regex %{[{}[\]()|;.,&!'\\]} 0:operator
add-highlighter shared/hemlock/code/prefix_operator regex %{[~?][-+*/%@^$<=>|:.~?]*} 0:operator
add-highlighter shared/hemlock/code/infix_operator regex %{[-+*/%@^$<=>|:.][-+*/%@^$<=>|:.~?]*} 0:operator

add-highlighter shared/hemlock/code/boolean regex \b(true|false)\b 0:value

add-highlighter shared/hemlock/code/bin_integer regex \b(0b)([_]*[01][01_]*)(([ui](8|16|32|64|128|256|512)?)|[zn])?\b 1:attribute 2:value 3:attribute
add-highlighter shared/hemlock/code/oct_integer regex \b(0o)([_]*[0-7][0-7_]*)(([ui](8|16|32|64|128|256|512)?)|[zn])?\b 1:attribute 2:value 3:attribute
add-highlighter shared/hemlock/code/hex_integer regex \b(0x)([_]*[0-9a-f][0-9a-f_]*)(([ui](8|16|32|64|128|256|512)?)|[zn])?\b 1:attribute 2:value 3:attribute
add-highlighter shared/hemlock/code/integer regex \b(([1-9][0-9_]*)|0[_]*)(([ui](8|16|32|64|128|256|512)?)|[zn])?\b 1:value 3:attribute

add-highlighter shared/hemlock/code/bin_real_dot regex \b(0b)([01][01_]*\.[01_]*(p_*[+\-]?_*[0-9][0-9_]*)?)(r(32|64)?)? 1:attribute 2:value 3:attribute
add-highlighter shared/hemlock/code/bin_real_p regex \b(0b)([01][01_]*p_*[+\-]?_*[0-9][0-9_]*)(r(32|64)?)?\b 1:attribute 2:value 3:attribute
add-highlighter shared/hemlock/code/bin_real_r regex \b(0b)([01][01_]*)(r(32|64)?)\b 1:attribute 2:value 3:attribute

add-highlighter shared/hemlock/code/oct_real_dot regex \b(0o)([0-7][0-7_]*\.[0-7_]*(p_*[+\-]?_*[0-9][0-9_]*)?)(r(32|64)?)? 1:attribute 2:value 3:attribute
add-highlighter shared/hemlock/code/oct_real_p regex \b(0o)([0-7][0-7_]*p_*[+\-]?_*[0-9][0-9_]*)(r(32|64)?)?\b 1:attribute 2:value 3:attribute
add-highlighter shared/hemlock/code/oct_real_r regex \b(0o)([0-7][0-7_]*)(r(32|64)?)\b 1:attribute 2:value 3:attribute

add-highlighter shared/hemlock/code/hex_real_dot regex \b(0x)([0-9a-f][0-9a-f_]*\.[0-9a-f_]*(p_*[+\-]?_*[0-9][0-9_]*)?)(r(32|64)?)? 1:attribute 2:value 3:attribute
add-highlighter shared/hemlock/code/hex_real_p regex \b(0x)([0-9a-f][0-9a-f_]*p_*[+\-]?_*[0-9][0-9_]*)(r(32|64)?)?\b 1:attribute 2:value 3:attribute
add-highlighter shared/hemlock/code/hex_real_r regex \b(0x)([0-9a-f][0-9a-f_]*)(r(32|64)?)\b 1:attribute 2:value 3:attribute

add-highlighter shared/hemlock/code/real_dot regex \b([0-9][0-9_]*\.[0-9_]*(e_*[+\-]?_*[0-9][0-9_]*)?)(r(32|64)?)? 1:value 2:attribute
add-highlighter shared/hemlock/code/real_e regex \b([0-9][0-9_]*e_*[+\-]?_*[0-9][0-9_]*)(r(32|64)?)?\b 1:value 2:attribute
add-highlighter shared/hemlock/code/real_r regex \b([0-9][0-9_]*)(r(32|64)?)\b 1:value 2:attribute

# Macro
# ‾‾‾‾‾

evaluate-commands %sh{
  keywords="and|also|as|conceal|effect|else|expose|external|fn|function|if|import|include|lazy|let"
  keywords="${keywords}|match|mutability|of|open|or|rec|then|type|val|when|with"

  printf %s\\n "declare-option str-list hemlock_static_words ${keywords}" | tr '|' ' '

  printf %s "
    add-highlighter shared/hemlock/code/ regex \b(${keywords})\b 0:keyword
  "
}

}
