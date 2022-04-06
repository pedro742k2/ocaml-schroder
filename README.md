# Schröder integer sequence with OCaml

## What is this project for?

- This program calculates a Schröder integer sequence in OCaml with arbitrary-precision integers, reaching incredibly high numbers with high precision.

## How to run this program?

- After clonning this repository, you can run `ocamlfind ocamlopt -o program main.ml -linkpkg -package zarith` to compile the file with the Zarith library, which can be installed by running `opam install zarith`;

- After compiled, just run the output executable file by running `./main.ml`.

## Problems installing zarith?

- Run `sudo apt-get install libgmp3-dev` to install "libgmp3-dev" on your linux, a multiprecision arithmetic library developers tools, needed for the zarith library.
