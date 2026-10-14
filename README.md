# TypeScript Subset Compiler in OCaml

> **Academic Project**: This project was developed as part of a university course to build a custom compiler for a subset of TypeScript, targeting JavaScript execution.

---

## Overview

This project implements a complete compilation pipeline for a subset of TypeScript using **OCaml** (`ocamllex` and `ocamlyacc`). It parses source files, builds an Abstract Syntax Tree (AST), handles semantic analysis (scopes), and compiles the code down to standard JavaScript.

### Project Structure

* `src/ast.ml`: Definitions of the Abstract Syntax Tree (AST) nodes, types, and code generation routines.
* `src/lexer.mll`: Lexical analysis rules using `ocamllex`.
* `src/parser.mly`: Grammatical rules and grammar definition using `ocamlyacc`.
* `src/main.ml`: Entry point containing the driver, scope verification, and compilation orchestration.
* `Makefile`: Build automation script.

---

## Getting Started

### Prerequisites

To compile and run this project, you will need:
* OCaml (>= 4.x)
* `ocamlbuild` or standard OCaml build tools (`ocamlc`, `ocamllex`, `ocamlyacc`)
* GNU Make

### Installation & Compilation

1. Clone the repository and navigate to the project root.
2. Run the `make` command to build the project:
   ```bash
   make
   ```

This will generate the compiled executable inside the `bin/` directory.

### Usage

To compile and execute a TypeScript subset script (`.ts`), run the binary with the path to your source file:
```bash
./bin/compiler script.ts
```

---

## Known Limitations and Issues

While the core pipeline successfully translates expressions, instructions, and functions into JavaScript, the project currently faces a few technical limitations documented below:

### 1. Parser Shift/Reduce Conflict (`parser.mly`)

* **Issue**: A shift/reduce conflict exists on the rule `type__ -> type__ LBracket RBracket`.
* **Impact**: This conflict is currently unresolved, which creates an important limitation: if a complex type includes an array type (such as `t[]`), it may trigger a syntax error during parsing.

### 2. Scope Checking (`check_scope`)

* **Issue**: The scope validation function does not function correctly when dealing with variables declared using `var`.
* **Impact**: This can lead to unexpected behaviors or missed scope errors during validation.

### 3. Type Checking (`check_type`)

* **Status**: Currently Disabled
* **Issue**: The type-checking module contains implementation flaws, causing it to frequently detect false-positive type errors.
* **Workaround**: To allow testing of the rest of the compiler without interference, type checking has been commented out in `src/main.ml` (line 342):
  ```Ocaml
  (*check_type ast;*)
  ```
  You can uncomment this line if you wish to experiment with or debug the type checker.

### 4. Error Location Reporting

* **Issue**: Error messages are displayed, but the exact character/line precision for where a semantic or syntax error originates can sometimes be imprecise, complicating the debugging process.