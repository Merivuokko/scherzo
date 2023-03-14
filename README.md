# Scherzo – a music notation toolbox

## Introduction

Scherzo is a set of tools for working with Western classical music notation.

The plan is to support translating music between different music notation formats and to render notation as audio.

## Status

Scherzo is in very early stages of planning – let alone development.
It cannot yet be used for anything.

All contributions (code, documentation, ideas) are very welcome.
Contributing will become easier though once there is some basic working functionality.

## Building

Cabal is used to build this package.
Build requires at least version of GHC >= 9.2 as Scherzo utilizes the OverloadedRecordDot language extension.
.cabal files should contain `tested-with` declarations which you may review if your build does not succeed.
