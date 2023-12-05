# Advent of Code 2023 Solutions (aoc2023)

## Overview
This repository contains my solutions for the [Advent of Code 2023](https://adventofcode.com/2023) challenges, implemented in OCaml.

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

### Running Solutions
1. Create an `inputs` directory inside `test`: 
   ```bash
   cd test
   mkdir inputs
   ```

2. Copy your inputs from each day into the directory. The files shall be named `input{day}.txt`, where `{day}` is replaced with the day's number counted from 1 (e.g. `input1.txt`, `input2.txt`, ..., `input25.txt`).

3. Run the included test:

   ```bash
   dune runtest
   ```
