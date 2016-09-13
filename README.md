# Brainfuck Compiler
This is a compiler for the Brainfuck programming language. It supports vanilla
Brainfuck as defined in the standard maintained by MuppetLabs Inc. The standard
can be found at
```
http://www.muppetlabs.com/~breadbox/bf/standards.html
```

### Dependencies
This is a Haskell programs. It requires the following dependencies:
```
1. ghc
2. megaparsec
3. bytestring
```
To update dependencies, do the following
```
$ cabal update
```
and then build the repository as follows:
```
$ git clone https://github.com/path-to/repo.git
$ cabal update
$ cabal install <missing-dependencies
$ cabal build
```
And then if you want to install `bfc`, enter
```
cabal install
```
otherwise, just run it from the built directory as
```
cabal run
```
to operate `bfc` from the repository.
