# Scherzo – a music notation language and toolbox

## Introduction

Scherzo is a set of tools for working with Western classical music notation.

It comprises of an implementation of an experimental music type setting language Alma (named after the pianist and composer Alma Mahler).

The plan is to support translating Alma to and from various other music notation formats – especially LilyPond. Alma is supposed to be easier to type, edit and read than LilyPond. The language syntax is configurable and it supports laying out simultaneously played sections of music so that they are visually aligned.

Alma is expected to be especially convenient for composers and people using braille for working iwht music notation.

## Status

Scherzo is in very early stages of development. It cannot yet be used for anything. There is a working prototype of a configurable Alma music expression parser. The current implementation allows for very flexible music parser building. It is not clear yet if all this flexibility is necessary/desirable, and if it is possible to automatically pretty-print the parsed music back to text using the parser's configuration.

All contributions (code, documentation, ideas) are very welcome. Contributing to will become easier once thre is some basic working functionality.

## Building

Cabal is used to build this package. It currently requires using GHC >= 9.2 as it utilizes the OverloadedRecordDot extension. You'll also need to use head.hackage repository because some dependencies of scherzo are still broken on ghc 9.2.

See [here](http://ghc.gitlab.haskell.org/head.hackage/) for instructions on how to set up head.hackage repository.
