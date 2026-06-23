# nothing-but-math

Nothing But Math (NBM) is a domain-specific language for writing scientific text and algorithms in a single source file. From that source, the compiler generates two synchronized outputs:

- a LaTeX document where algorithms are rendered in formal mathematical notation, and
- a Haskell library that contains runnable implementations of the same algorithms.

The project was designed for the research paper that accompanies this repository. Its goal is to make mathematical algorithm descriptions easier to write, easier to read, and easier to reproduce.

## What NBM does

NBM lets you interleave three kinds of content in one `.nbm` file:

- text sections, which are passed through as LaTeX,
- code sections, which define functions and constants, and
- eval sections, which run expressions and insert their results into the generated LaTeX.

The compiler then produces:

- a `.hs` file containing the generated Haskell module,
- a `.tex` file containing the generated LaTeX document, and
- optionally a PDF when `pdflatex` is available.

## Installation

### From source

Build the executable with Stack:

```bash
stack build
```

Install it globally into Stack's local bin directory:

```bash
stack install
```

### From a release

Prebuilt binaries are published on the GitHub releases page:

https://github.com/niek-peters/nothing-but-math/releases

Download the release for your platform and add the `nbm` executable to your `PATH`.

## Requirements

The compiler itself is a Haskell application. For day-to-day use, the external tools you need depend on the features you use:

| Feature                                | Extra requirement                                        |
| -------------------------------------- | -------------------------------------------------------- |
| Plain compilation to Haskell and LaTeX | none beyond the NBM executable                           |
| Eval sections                          | `ghc` in `PATH`                                          |
| `--pdf`                                | `ghc` in `PATH` and a LaTeX distribution with `pdflatex` |

The repository contains Dockerfiles under `test/DependencyTesting/` that show the minimal dependency sets for these cases.

## Usage

The CLI entry point is `nbm`.

```bash
nbm PATHNAME [--out-dir DIR] [--module-name MODULE_NAME] [--wrapdoc] [--pdf]
```

### CLI options

| Option                            | Meaning                                                                                                                  |
| --------------------------------- | ------------------------------------------------------------------------------------------------------------------------ |
| `PATHNAME`                        | Path to the `.nbm` file. If the extension is omitted and the file does not already exist, `.nbm` is added automatically. |
| `-o`, `--out-dir DIR`             | Output directory for generated files. Defaults to the current directory.                                                 |
| `-m`, `--module-name MODULE_NAME` | Name of the generated Haskell module. Defaults to `NBM`.                                                                 |
| `-w`, `--wrapdoc`                 | Wrap the generated LaTeX in a minimal document preamble. Use this when you are not already supplying a LaTeX template.   |
| `-p`, `--pdf`                     | Run `pdflatex` after generation and produce a PDF.                                                                       |
| `-v`, `--version`                 | Print the compiler version.                                                                                              |

### Typical workflows

Generate Haskell and LaTeX only:

```bash
stack run nbm -- test/samples/functions.nbm -o out
```

Generate LaTeX plus a standalone PDF:

```bash
stack run nbm -- test/samples/functions.nbm -o out -w -p
```

Change the generated Haskell module name:

```bash
stack run nbm -- test/samples/booleans.nbm -o out -m MyNBM
```

## Language overview

An NBM source file is split into fragments by special delimiters:

- `<<< ... >>>` for code sections,
- `{{{ ... }}}` for eval sections, and
- everything else for text sections.

Text sections are emitted as-is, so you can write ordinary LaTeX there. Code and eval sections use the DSL syntax described below.

### Example source

```nbm
<<<
f : N x N -> Z
f(a, b) := c
where c := a + b

pos : Z -> N
pos(x) := {
    x      if x >= 0
    -x     otherwise
}
>>>

We can evaluate sample values here: {{{(f(1, 2), pos(-3))}}}.
```

### Definitions

A definition has four parts:

1. optional annotations,
2. a name,
3. a type signature, and
4. an implementation, optionally followed by a `where` clause.

Constants are just definitions without arguments:

```nbm
pi : R
pi := 3.1415926535
```

Functions use one or more arguments:

```nbm
g : Z x N -> Q
g(x, a) := x ^ a / 2
```

### Constraints and local declarations

The `where` clause can contain two kinds of entries:

- constraints, which must evaluate to `True`, and
- local declarations, which introduce reusable intermediate values.

Examples:

```nbm
f : Z -> Q
f(x) := 1 / c
where x /= 0, c := x + 1
```

Constraints are checked before the implementation runs. If a constraint fails, the generated Haskell code raises a runtime error instead of continuing.

### Piecewise functions

Conditional implementations are written with braces:

```nbm
abs : Z -> N
abs(x) := {
    -x if x < 0
    x  otherwise
}
```

### Types

NBM supports the following primitive types:

| NBM type | Meaning           |
| -------- | ----------------- |
| `Z+`     | positive integers |
| `N`      | natural numbers   |
| `Z`      | integers          |
| `Q`      | rational numbers  |
| `R`      | real numbers      |
| `B`      | booleans          |

Tuples are written with `x` between types, for example `Z x N -> Q` or `Z x R`.

### Operators

The parser supports the following operators:

| Category   | Operators                         |
| ---------- | --------------------------------- | --- |
| Unary      | `-`, `sqrt`, `floor`, `not`       |
| Arithmetic | `+`, `-`, `*`, `/`, `^`, `mod`, ` | `   |
| Comparison | `=`, `/=`, `<`, `<=`, `>`, `>=`   |
| Boolean    | `and`, `or`                       |

The `|` operator denotes divisibility.

### LaTeX annotations

Annotations let you control how code sections are rendered in the output document.

Section-level annotations use `#[...]` and apply to the entire code block.

Supported section-level annotations are:

- `#[inline]` - render the block inline,
- `#[intext]` - render the block as a multi-line in-text display,
- `#[box]` - render the block inside a framed definition box,
- `#[hidden]` - omit the block from the LaTeX output,
- `#[class=...]` - set the box class name,
- `#[name=...]` - set the box title,
- `#[label=...]` - set the LaTeX label,
- `#[description=...]` - add a short description under the title.

Definition-level annotations use `@[...]` and currently support:

- `@[hidden]` - omit a single definition from the LaTeX output.

Example:

```nbm
<<<
#[box, class="Algorithm", name="Doubling", label="doubling"]
@[hidden]
helper : Z -> Z
helper(x) := x + 1

main : Z -> Z
main(x) := 2 * helper(x)
>>>
```

## Project structure

- `app/` - command-line entry point.
- `src/` - lexer, parser, elaboration, evaluation, and code generation.
- `prelude/` - helper code injected into the generated Haskell module.
- `test/` - Hspec and golden tests.
- `test/samples/` - sample `.nbm` programs used by the tests.
- `test/DependencyTesting/` - Dockerfiles and fixtures for feature-specific dependency checks.
- `build.sh` - convenience build script.

## Running tests

Run the test suite with Stack:

```bash
stack test
```

If you want to build the executable before running tests or samples, use:

```bash
stack build
```

## Implementation notes

The compiler pipeline follows the structure described in the paper:

1. tokenize the source file,
2. parse code and eval fragments into ASTs,
3. elaborate the definitions to resolve names and insert casts,
4. generate a Haskell library and LaTeX output,
5. evaluate eval fragments against the generated library, and
6. splice the results back into the LaTeX document.

The generated Haskell code includes a small prelude with a custom positive-integer type and runtime casting helpers.

## Related paper

The repository implements the prototype described in the accompanying research paper on bridging mathematical notation and functional programming with a dual-target DSL for LaTeX and Haskell.
