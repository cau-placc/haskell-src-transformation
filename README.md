# haskell-src-transformations

<!-- Badges -->
![CI Pipeline](https://github.com/FreeProving/haskell-src-transformations/workflows/CI%20Pipeline/badge.svg)

<!-- Short description -->
This tool was implemented as part of a bachelor thesis (by Malte Clement at Kiel University) and should be considered as an __alpha version of a prototype__.

## Table of Contents

1. [Haskell to Haskell Transformation Tool](#haskell-to-haskell-transformation-tool)
2. [Directory Structure](#directory-structure)
3. [Getting Started](#getting-started)
    1. [Required Software](#required-software)
    2. [Executable Installation](#executable-installation)
    3. [Library Installation](#library-installation)
4. [Usage](#usage)
5. [Get Involved](#get-involved)
6. [License](#license)

# Haskell to Haskell Transformation Tool

This is a tool to translate different language features of Haskell into simpler expressions.

 - It translates pattern matching on left-hand sides into explicit pattern matching on right-hand sides using Wadler's Algorithm for compiling pattern matching.

 - Guards are transformed into `case` expressions using their semantics as described in the Haskell report.

 - Partially defined functions are completed by inserting alternatives whose right-hand side is `undefined` such that pattern matching failures are handled explicitly.

## Directory Structure

The repository is structured as follows.

 - `src` contains the source code of the project.

    + `src/exe` contains modules for the command line interface.
    + `src/lib` contains modules for the code transformation library.
    + `src/test` contains modules that have been translated to test certain features.

 - `example` contains some examples that have been transformed by hand using Wadler's algorithm. \
    Additionally, it contains some special examples that were used to test the [Free Compiler](free-compiler).

 - `tool` contains Bash scripts that are used during development and for testing.

## Getting Started

### Required Software

The tool is written in Haskell and uses Cabal to manage the dependencies.
To build it, the GHC and Cabal are required.
The tool has been tested with the following software versions.

 - [GHC][software/ghc], version  8.6.5
 - [Cabal][software/cabal], version 2.4.1.0

### Executable Installation

In order to install the command line interface, navigate to the root directory of the project and run

```bash
cabal new-install exe:haskell-src-transformations
```

You can also run the executable directly without installing it first.

```bash
cabal new-run exe:haskell-src-transformations -- [options...]
```

### Library Installation

The library provided by this package is not yet available on Hackage.
In order to use the Haskell code transformations in your own project, add the following stanza to your `cabal.project` file.

```cabal
source-repository-package
  type: git
  location: git://github.com/FreeProving/haskell-src-transformations.git
  tag: v0.1.1.0
```

## Usage

To transform a Haskell module, install the command line interface as described above and run the following command.

```bash
haskell-src-transformations -I [InputFile]
```

The generated code is printed to the console. If you want to specify the output directory, use the `-o` flag.

For more information on the different flags you can use the `-h` flag.

## Get Involved

Feature requests, enhancement proposals, bug reports, pull requests and all other contributions are welcome!  
Have a look at our [contributing guidelines][guidelines/CONTRIBUTING] for more information on how to contribute.

## License

The Free Compiler is licensed under The 3-Clause BSD License.  
See the [LICENSE][haskell-src-transformations/LICENSE] file for details.

[haskell-src-transformations/LICENSE]:
  https://github.com/FreeProving/haskell-src-transformations/blob/master/LICENSE
  "haskell-src-transformations — The 3-Clause BSD License"

[free-compiler]:
  https://github.com/FreeProving/free-compiler
  "Free Compiler"

[guidelines/CONTRIBUTING]:
  https://github.com/FreeProving/guidelines/blob/master/CONTRIBUTING.md
  "Contributing Guidelines of the FreeProving project"

[software/ghc]:
  https://www.haskell.org/ghc/
  "The Glasgow Haskell Compiler"
[software/cabal]:
  https://www.haskell.org/cabal/
  "Common Architecture for Building Applications and Libraries"
