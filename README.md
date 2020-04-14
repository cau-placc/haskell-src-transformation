# haskell-src-transformation

This tool was implemented as part of a bachelor thesis (by Malte Clement at Kiel University) and should be considered as a __alpha version of a prototype__.

# Haskell to Haskell Transformation Tool

This is a tool to translate different language features of Haskell into simpler expressions.

* It translates pattern matching on left hand sides into explicit pattern matching on right sides using Wadlers Algorithm for compiling pattern matching.

* Guards are transformed into `case`-expression using their semantics as described in the Haskell-report.

* Partially defined functions are completed such that pattern mismatches do not occur.

## Repository

The repository consists of two directories

* `src` contains the source code of the project

  * `src/exe` contains modules for the command line interface
  * `src/lib` contains modules for the conde transformation library
  * `src/test` contains modules that have been translated to test certain features

* `example` contains some examples that have been transformed by hand using Wadler's algorithm.

  Additionally, it contains some special examples that were used to test the [haskell-to-coq-compiler](https://git.informatik.uni-kiel.de/stu203400/haskell-to-coq-compiler)

## Required Software

The tool is written in Haskell and uses Cabal to manage the dependencies.
To build it, the GHC and Cabal are required.
The tool has been tested with the following software versions.

 - [GHC](https://www.haskell.org/ghc/), version  8.6.5
 - [Cabal](https://www.haskell.org/cabal/), version 2.4.1.0

## Installation

### Executable

In order to install the command line interface navigate to the root directory of the project and run

```bash
cabal new-install exe:haskell-src-transformations
```

You can also run the executable directly without installing it first.

```bash
cabal new-run exe:haskell-src-transformations -- [options...]
```

### Library

The library provided by this package is not yet available on Hackage.
In order to use the Haskell code transformations in your own project, add the following stanza to your `cabal.project` file.

```cabal
source-repository-package
  type: git
  location: git://github.com/FreeProving/haskell-src-transformations.git
  tag: v0.1.0.0
```

## Usage

To transform a Haskell module, install the command line interface as described above and run the following command.

```bash
haskell-src-transformations -I [InputFile]
```

The generated code is printed to the console. If you want to specify the output directory use the `-o` flag.

For more information on the different flags you can use the `-h` flag.
