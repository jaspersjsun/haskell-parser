# haskell-parser

[![Build Status](https://travis-ci.org/FakeCola/haskell-parser.svg?branch=master)](https://travis-ci.org/FakeCola/haskell-parser)

This is the final project of Haskell@Tsinghua. 

In this project, we are asked to implement a code parser with specific given grammar using Haskell language.

This project is managed with [Stack](https://docs.haskellstack.org/en/stable/README/), a cross-platform program for developing Haskell projects.

---

To build the project, go to src directory and run:

```sh
$ stack build
```

## Parser Mode

Usage:

1. ```sh $ stack exec haskell-parser -- -i <file> -o <file>```
	
	parse and execute the code from input file and output the results to another file

2. ```sh $ stack exec haskell-parser -- -i <file>```
	
	parse and execute the code from input file and output the results to stdout
	
3.  ```sh $ stack exec haskell-parser -- -t <file> -o <file>```
	
	parse and the code from input file and output the abstract syntax tree (AST) to another file

4. ```sh $ stack exec haskell-parser -- -t <file>```
	
	parse and the code from input file and output the AST to stdout



## Real-Eval-Print-Loop (REPL) Mode

Usage:

To enter REPL mode, run:

```sh
$ stack exec haskell-parser -- -repl
```

In REPL mode, there are three commands:

1. ```sh > :q``` quit
2. ```sh > :i <file-name>``` load and parse the program in the file and output the results
3. ```sh > :t``` print the AST of the last program

Otherwise, any string input with 'Enter' will be regarded as a part of code (expression, statement, whatever) and executed.

---

For now, it supports some data type (`Boolean`, `Floating-point Number`, `Char`, `String`, `List`) and some control logics from imperative programming (like `assignment`, `if-else`, `while-skip`, `begin`)
