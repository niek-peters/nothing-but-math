# NBM Grammar Reference

This document summarizes the grammar implemented by the lexer and parser.

## Source structure

An NBM source file is split into fragments of three kinds:

- text fragments outside special delimiters,
- code fragments inside `<<< ... >>>`, and
- eval fragments inside `{{{ ... }}}`.

Text fragments are copied to the LaTeX output as-is. Code fragments are parsed into definitions. Eval fragments are parsed as expressions and later evaluated against the generated Haskell library.

## Eval fragments

An eval fragment contains a single expression.

```ebnf
evalFragment ::= expr
```

Eval fragments are written between `{{{` and `}}}` in the source file.

## Code fragments

A code fragment may start with a block annotation and then contains one or more declarations.

```ebnf
codeFragment  ::= [blockAnnotation] declaration+
blockAnnotation ::= "#[" blockAnnotationItem ("," blockAnnotationItem)* "]"
```

Supported block annotation items:

- `inline`
- `intext`
- `box`
- `hidden`
- `class = string`
- `name = string`
- `label = string`
- `description = string`

A declaration may start with a declaration annotation.

```ebnf
declaration ::= [declAnnotation] ident ":" signature ident ["(" [ident ("," ident)*] ")"] ":=" implementation ["where" whereTerm ("," whereTerm)*]
declAnnotation ::= "@[" "hidden" "]"
```

The leading `ident` is the declared name. The optional parenthesized identifier list contains function parameters. If the parameter list is omitted, the declaration is a constant.

## Signatures and types

```ebnf
signature ::= type ["->" type]
type      ::= primitiveType ("x" primitiveType)*
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
implementation ::= expr | "{" branch+ otherwiseBranch "}"
branch          ::= expr "if" expr
otherwiseBranch ::= expr "otherwise"
```

The parser accepts the branches in order. The last branch must be the `otherwise` branch.

## Where clauses

A `where` clause contains a comma-separated list of constraints and local declarations.

```ebnf
whereTerm ::= localDecl | expr
localDecl ::= (ident | "(" ident ("," ident)+ ")") ":=" expr
```

A local declaration can bind either a single identifier or a tuple of identifiers.

## Expressions

Expressions are parsed with precedence and associativity rules.

```ebnf
expr        ::= orExpr
orExpr      ::= andExpr ("or" andExpr)*
andExpr     ::= compExpr ("and" compExpr)*
compExpr    ::= addExpr (("=" | "/=" | "<" | "<=" | ">" | ">=" | "|") addExpr)*
addExpr     ::= mulExpr (("+" | "-") mulExpr)*
mulExpr     ::= unaryExpr (("*" | "/" | "mod") unaryExpr)*
unaryExpr   ::= ("-" | "not" | "sqrt" | "floor")* powerExpr
powerExpr   ::= atom ["^" powerExpr]
atom        ::= NUMBER | BOOLEAN | IDENT | call | tuple | "(" expr ")"
call        ::= IDENT "(" [expr ("," expr)*] ")"
tuple       ::= "(" expr "," expr ("," expr)* ")"
```

### Operator notes

- `-` is both unary negation and binary subtraction.
- `|` means divisibility.
- `^` is right-associative.
- `sqrt` and `floor` are unary operators.

## Lexical notes

The lexer also recognizes the following literals and keywords:

- integers,
- real numbers,
- booleans `True` and `False`,
- `if`, `otherwise`, `where`, `not`, `and`, `or`, `mod`, `sqrt`, and `floor`.

Percent-prefixed comments are ignored inside code and eval fragments.
