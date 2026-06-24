# NBM Grammar Reference

This document summarizes the grammar implemented by the lexer and parser.

## Source structure

An NBM source file is split into fragments of three kinds:

- **Text fragments** outside special delimiters,
- **Code fragments** inside `<<< ... >>>`, and
- **Eval fragments** inside `{{{ ... }}}`.

Text fragments are copied to the LaTeX output as-is. Code fragments are parsed into definitions. Eval fragments are parsed as expressions and later evaluated against the generated Haskell library.

## Eval fragments

An eval fragment contains a single expression.

```ebnf
evalFragment ::= expr
```

## Code fragments

A code fragment may start with a block annotation and then contains one or more declarations.

```ebnf
codeFragment    ::= [blockAnnotation] declaration+
blockAnnotation ::= "#[" [blockAnnotationItem ("," blockAnnotationItem)*] "]"
```

Supported block annotation items:

- `inline`
- `intext`
- `box`
- `hidden`
- `class = STRING`
- `name = STRING`
- `label = STRING`
- `description = STRING`

A declaration may start with a declaration annotation.

```ebnf
declaration    ::= [declAnnotation] IDENT ":" signature IDENT ["(" [IDENT ("," IDENT)*] ")"] ":=" implementation ["where" whereTerm ("," whereTerm)*]
declAnnotation ::= "@[" ["hidden"] "]"
```

The leading `IDENT` is the declared name used for its signature line. The second `IDENT` precedes the function parameters. If the parameter list is omitted or empty, the declaration is a constant.

## Signatures and types

```ebnf
signature ::= type [" -> " type]
type      ::= primitiveType (" x " primitiveType)*
```

Primitive types are:

- `Z+` for positive integers,
- `N` for natural numbers,
- `Z` for integers,
- `Q` for rational numbers,
- `R` for real numbers,
- `B` for booleans.

The `x` separator forms tuple types, for example `Z x N -> Q` or `Z x R`.

## Implementations

Implementations are either a single expression or a piecewise block.

```ebnf
implementation  ::= "{" branch* otherwiseBranch "}" | expr
branch          ::= expr "if" expr
otherwiseBranch ::= expr "otherwise"
```

The parser accepts zero or more conditional branches in order. The block must end with an `otherwise` branch acting as the fallback.

## Where clauses

A `where` clause contains a comma-separated list of constraints and local declarations.

```ebnf
whereTerm ::= localDecl | expr
localDecl ::= ("(" IDENT ("," IDENT)+ ")" | IDENT) ":=" expr
```

A local declaration can bind either a single identifier or a tuple of identifiers.

## Expressions

Expressions are parsed according to strict precedence and associativity rules. The grammar below reflects this ordering, moving from lowest precedence (top) to highest precedence (bottom).

```ebnf
expr           ::= orExpr
orExpr         ::= andExpr ("or" andExpr)*
andExpr        ::= compExpr ("and" compExpr)*
compExpr       ::= addExpr (("=" | "/=" | "<=" | ">=" | "<" | ">" | "|") addExpr)*
addExpr        ::= mulExpr (("+" | "-") mulExpr)*
mulExpr        ::= prefixExpr (("*" | "/" | "mod") prefixExpr)*
prefixExpr     ::= ("-" | "not")* prefixExpr | powerExpr
powerExpr      ::= tightUnaryExpr ["^" powerExpr]
tightUnaryExpr ::= ("sqrt" | "floor")* tightUnaryExpr | term
term           ::= "(" (tuple | expr) ")" | call | REAL | INTEGER | BOOLEAN
tuple          ::= expr "," expr ("," expr)*
call           ::= IDENT ["(" expr ("," expr)* ")"]
```

### Operator notes

- `-` can act as both a unary prefix negation operator (binding below exponentiation) and a binary subtraction operator.
- `|` represents divisibility.
- `^` is right-associative (e.g., `x^y^z` parses as `x^(y^z)`).
- `sqrt` and `floor` are tight-binding unary prefix operators that bind stronger than exponentiation (e.g., `sqrt x ^ 2` parses as `(sqrt x) ^ 2`).

## Lexical notes

The lexer recognizes the following literals and keywords:

- Whole numbers (`INTEGER`),
- Real/decimal numbers (`REAL`),
- Text strings (`STRING`): e.g., `"hello"` or `"world"`,
- Booleans (`BOOLEAN`): `True` and `False`,
- Keywords: `if`, `otherwise`, `where`, `not`, `and`, `or`, `mod`, `sqrt`, and `floor`.

Percent-prefixed (`%`) comments are ignored inside code and eval fragments.
