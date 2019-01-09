# Compiling to Reverse Polish notation (RPN)


This directory is an illustrative example of a small compiler: a
compiler from expressions to reverse polish notation. Although this is
really a toy example, we have structured the program like a classical
compiler. More complicated compilers can follow this high level
structure.

## The main programs.

This directory contains the source code for two standalone programs.

`ec`
:   The expression compiler which compiles normal expressions into
    reverse polish notation. You should think of the expression as
    your high level language and reverse polish notation as your low
    level language.

`rp`
:   The reverse polish machine, which interprets the expression in
    reverse polish notation. This is given to test your compiler.

Build these programs using the following make command

```
make all
```

## The RPN machine.

The RPN takes a sequence of machine instructions which consists of

1. A number with an optional sign [+-~]. One can use the ~ sign for
   negation following the Standard ML convention

2. Single character commands `p` (for printing the top of the stack)
   `s` for printing the entire stack and `c` for clearing the stack.

3. Line comments starting with the character `#`.

The executable `rp` that this repository provides can be used to run a
_reverse polish script_. For example, here is a "machine language
program" for `rp` that illustrates its syntax.


```
# This is a sample script for rpn
# We compute the answer to life universe and everything

2 40 +ps

```

Save the above script into a file say `test.inp` and run it using the
command

```
./rp test.inp

```

The above program prints 42 (because of the `p`) followed by `[42]`
(because of the `s`).

## How to read the source code ?

A compiler is just a translator from a source language, the
_expression language_ in our case, to a target language, the "machine
language" associated with the _reverse polish_ machine. We recommend
the following design for the compiler:

1. Capture the source and target language using their respective
   _abstract syntax tree_ (AST for short). In our case the
   [`Ast.Expr`][ast] and the [Machine.Program][machine] types
   available inside the files [ast.sml][ast] and
   [machine.sml][machine] captures them respectively.

2. The core of the compiler is then written as a function form the
   source AST to the target AST. In our case this is available in the
   [translate.sml][translate] file.

3. For the actual parser, which just parses the source program and
   builds the AST, use tools like [`mlyacc`][mlyacc] and
   [`mllex`][mllex].

The suggested reading order is therefore [`ast.sml`][ast],
[`machine.sml`][machine], and [`translate.sml`][translate]. The actual
input to the [`mlyacc`][mlyacc] and [`mllex`][mllex] tool is files
[`expr.grm`][expr.grm] and [`expr.lex`][expr.lex], which you can skip
in the first reading.


[ast]: <ast.sml>
[machine]: <machine.sml>
[translate]: <machine.sml>
[expr.grm]: <expr.grm>
[expr.lex]: <expr.lex>
[mlyacc]: <http://mlton.org/MLYacc>
[mllex]: <http://mlton.org/MLLex>
