# haskell-src-transformation

This tool was implemented as part of a bachelor thesis (by Malte Clement at Kiel University) and should be considered as a __alpha version of a prototype__.

# Haskell to Haskell Transformation Tool

This is a tool to translate different language features of Haskell into simpler expressions.

* It translates pattern matching on left hand sides into explicit pattern matching on right sides using
  Wadlers Algorithm for compiling pattern matching.

* Guards are transformed into `case`-expression using their semantics as described in the Haskell-report.

* Partially defined functions are completed such that pattern mismatches do not occur.

## Documentation

* The thesis that describes the transformations can be found in the `LaTeX`-directory.

## Repository

The repository consists of two directories

* `src` contains the source code of the project

  * `src/test` contains modules that have been translated to test certain features
  * `src/Examples` contains two modules which can be used for local debugging with the `Application.hs` module containing a `test`-function to work with these local modules.

* `Examples` contains some examples that have been transformed by hand using Wadler's algorithm.

  Additionally, it contains some special examples that were used to test the [haskell-to-coq-compiler](https://git.informatik.uni-kiel.de/stu203400/haskell-to-coq-compiler)

## Required Software

The tool is written in Haskell and uses Cabal to manage the dependencies. To build it, the GHC and Cabal are required. The tool has been tested with [GHC](https://www.haskell.org/ghc/), version  8.4.3 and 
[Cabal](https://www.haskell.org/cabal/), version 2.2.0.1.

## Installation

In order to install the library and the executable navigate to the root directory of the project and run

```bash
cabal install CodeTransformation
```

## Usage

To transform a Haskell module navigate to `./dist/build` and run

```bash
CodeTransformation -I [InputFile]
```

The generated code is printed to the console. If you want to specify the output directory use the `-o` flag.

 For more information on the different flags you can use the `-h` flag.
