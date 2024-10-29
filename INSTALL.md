# Installation Instructions for Camelot Adventures

## Prerequisites

Before installing this project, you need to have the following software installed on your system:

- OCaml (version 3.110 or higher)
- OPAM (OCaml's package manager)
- Dune (build system)

## Setting Up the Development Environment

1. First, make sure you have OPAM initialized:
   ```bash
   opam init
   eval $(opam env)
   ```

2. Create and switch to a new OPAM switch for OCaml 3.110:
   ```bash
   opam switch create . ocaml-base-compiler.3.110.0
   eval $(opam env)
   ```

3. Install the required dependencies:
   ```bash
   opam install . --deps-only
   ```

## Building the Project

1. To build the project, run:
   ```bash
   dune build
   ```

2. To run the tests:
   ```bash
   dune test
   ```

## Running the Game

After building the project, you can run the game using:

```bash
dune exec cs3110finalproject
```
