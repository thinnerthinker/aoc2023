# Advent of Code 2023 Solutions (aoc2023)

## Overview
This repository contains my solutions for the [Advent of Code 2023](https://adventofcode.com/2023) challenges, implemented in OCaml.

## Structure
- **Language**: OCaml
- **Build System**: Dune (version 3.9)
- **Package Management**: OPAM (with generated OPAM files)

## Installation and Usage
### Prerequisites
- [OCaml](https://ocaml.org/)
- [Dune](https://dune.build/)

### Setup
   ```bash
   git clone https://github.com/thinnerthinker/aoc2023.git
   cd aoc2023
   dune build
   ```

### Running Solutions in Utop
To interactively run and test the solutions using `utop`, see this example:

   ```bash
   dune utop
   
   open Aoc;;
   Util.read_all_lines "inputs/input3.txt" |> Day03.part2;;
   ```
