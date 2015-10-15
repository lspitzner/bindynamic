# bindynamic

[![Build Status](https://secure.travis-ci.org/lspitzner/bindynamic.svg)](http://travis-ci.org/lspitzner/bindynamic)
[![Hackage](https://img.shields.io/hackage/v/bindynamic.svg)](https://hackage.haskell.org/package/bindynamic)

A variation of `Data.Dynamic.Dynamic` with a `Binary` instance.

This `Dynamic` is instance `Binary` by encapsulating only
values of types that are `Binary`. It is a bit more
efficient than something similar to `(ByteString, TypeRep)`
as it avoids unnecessary encode/decoding round-trips.

The interface is the same as that of the original `Dynamic`; `dynApp`/`dynApply` do not work with this type.
