%{
%}

%precedence pAS AS
%right pSEMI SEMI
%left pCOMMA COMMA
%left pDOT DOT
%precedence pCIDENT
%precedence pPrec PREC
%precedence pCodeTlEpsilon

%token HOCC "hocc"
%token NONTERM "nonterm"
%token EPSILON_ "epsilon"
%token START "start"
%token TOKEN "token"
%token NEUTRAL "neutral"
%token LEFT "left"
%token RIGHT "right"
%token NONASSOC "nonassoc"
%token PREC "prec"
%token UIDENT
%token CIDENT
%token USCORE "_"
%token ISTRING
%token COLON_COLON_EQ "::="
%token OF "of"
%token COLON ":"
%token DOT "."
%token ARROW "->"
%token BAR "|"
%token LT "<"
%token EQ "="
%token COMMA ","
%token SEMI ";"
%token AS "as"
%token LINE_DELIM
%token INDENT
%token DEDENT
%token LPAREN "("
%token RPAREN ")"
%token LCAPTURE "(|"
%token RCAPTURE "|)"
%token LBRACK "["
%token RBRACK "]"
%token LARRAY "[|"
%token RARRAY "|]"
%token LCURLY "{"
%token RCURLY "}"
%token OTHER_TOKEN
%token EOI

%token HOCC_PSEUDO_START_Hmh
%token HOCC_PSEUDO_START_Hmhi

%%

%start hocc-pseudo-start;

hocc-pseudo-start:
  HOCC_PSEUDO_START_Hmh Hmh;
| HOCC_PSEUDO_START_Hmhi Hmhi;
;

Uident :
  "hocc"
| "nonterm"
| "epsilon"
| "start"
| "token"
| "neutral"
| "left"
| "right"
| "nonassoc"
| "prec"
| UIDENT
;

PrecsTl :
  /* empty */
| "," Uident PrecsTl
;

Precs :
  Uident PrecsTl
;

PrecRels :
  /* empty */
| "<" Precs
;

PrecType :
  "neutral"
| "left"
| "right"
| "nonassoc"
;

PrecSet :
  PrecType Precs PrecRels
;

SymbolTypeQualifier :
  /* empty */
| CIDENT "." SymbolTypeQualifier
;

SymbolType :
  "of" SymbolTypeQualifier Uident
;

PrecRef :
  /* empty */
| "prec" Uident
;

Alias :
  /* empty */
| ISTRING
;

TokenType :
  /* empty */
| SymbolType Code
;

Token :
  "token" CIDENT Alias TokenType PrecRef
;

Sep :
  LINE_DELIM
| ";"
| "|"
;

CodesTl :
  /* empty */
| Sep Code CodesTl
;

Codes :
  Code CodesTl
;

Codes0 :
  /* empty */
| Codes
;

Delimited :
  INDENT Codes DEDENT
| "(" Codes0 ")"
| "(|" Codes0 "|)"
| "[" Codes0 "]"
| "[|" Codes0 "|]"
| "{" Codes0 "}"
;

CodeToken :
  OTHER_TOKEN
| Uident
| CIDENT
| "_"
| ISTRING
| "::="
| "as"
| "of"
| ":"
| "."
| "->"
| "<"
| "="
| ","
;

CodeTl :
  /* empty */ %prec pCodeTlEpsilon
| Delimited CodeTl
| CodeToken CodeTl
;

Code :
  Delimited CodeTl
| CodeToken CodeTl
;

PatternField :
  Uident
| Uident "=" Pattern
;

PatternFields :
  PatternField %prec pSEMI
| PatternField ";" "_"
| PatternField ";" PatternFields
;

SemiSuffix :
  /* empty */
| ";"
;

ModulePath :
  CIDENT
| ModulePath "." ModulePath %prec pDOT
;

Pattern :
  "_"
| Uident
| Pattern "as" Uident
| "(" Pattern ")"
| CIDENT Pattern %prec pCIDENT
| ModulePath "." "(" Pattern ")"
| Pattern "," Pattern %prec pCOMMA
| "{" PatternFields SemiSuffix "}"
| ModulePath "." "{" PatternFields SemiSuffix "}"
;

ProdParamSymbol :
  CIDENT
| ISTRING
;

ProdParam :
  Uident ":" ProdParamSymbol
| "(" Pattern ")" ":" ProdParamSymbol
| ModulePath "." "(" Pattern ")" ":" ProdParamSymbol
| "{" PatternFields SemiSuffix "}" ":" ProdParamSymbol
| ModulePath "." "{" PatternFields SemiSuffix "}" ":" ProdParamSymbol
| "_" ":" ProdParamSymbol
| ProdParamSymbol
;

ProdParamsTl :
  ProdParam ProdParamsTl
| PrecRef
;

ProdParams :
  ProdParam ProdParamsTl
;

ProdPattern :
  ProdParams
| "epsilon" PrecRef
;

Prod :
  ProdPattern
;

ProdsTl :
  /* empty */
| "|" Prod ProdsTl
;

Prods :
  "|" Prod ProdsTl
| Prod ProdsTl
;

Reduction :
  Prods "->" Code
;

ReductionsTl :
  /* empty */
| "|" Reduction ReductionsTl
;

Reductions :
  Reduction ReductionsTl
;

NontermType :
  "nonterm"
| "start"
;

Nonterm :
  NontermType CIDENT PrecRef "::=" Prods
| NontermType CIDENT SymbolType PrecRef "::=" Reductions
;

Stmt :
  PrecSet
| Token
| Nonterm
;

StmtsTl :
  /* empty */
| LINE_DELIM Stmt StmtsTl
;

Stmts :
  Stmt StmtsTl
;

Hocc :
  "hocc" INDENT Stmts DEDENT
;

MatterToken :
  Sep
| "nonterm"
| "epsilon"
| "start"
| "token"
| "neutral"
| "left"
| "right"
| "nonassoc"
| "prec"
| OTHER_TOKEN
| UIDENT
| CIDENT
| "_"
| ISTRING
| "::="
| "as"
| "of"
| ":"
| "."
| "->"
| "<"
| "="
| ","
| INDENT
| DEDENT
| "("
| ")"
| "(|"
| "|)"
| "["
| "]"
| "[|"
| "|]"
| "{"
| "}"
;

Matter :
  /* empty */
| MatterToken Matter
;

Hmh :
  Matter Hocc Matter EOI
;

Hmhi :
  Matter "hocc" Matter EOI
;

%%
