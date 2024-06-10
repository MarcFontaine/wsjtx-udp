WSJT-X UDP Server
=================

[![Available on Hackage][badge-hackage]][hackage]
[![License BSD3][badge-license]][license]
[![Windows Binary Release][badge-github-releases]][github-releases]

This library provides:

* The types for WSJT-X UDP network messages.
* Functions for encoding and decoding of packages.
* UDP Server for receiving messages from WSJT-X and sending to WSJT-X.
* JSON instances for network messages.

`wsjtx-dump-udp` is a simple executable that shows how to use the library.
`wsjtx-dump-udp` implements a UDP server that receives packages from WSJT-X and prints each
received package to STDOUT (in a Haskell specific format).
A more interesting application based on the wsjtx-udp library is available on
[GitHub](https://github.com/MarcFontaine).
 
[github-releases]: https://github.com/MarcFontaine/wsjtx-udp/releases
[badge-license]: https://img.shields.io/badge/license-BSD3-green.svg
[license]: https://github.com/MarcFontaine/wsjtx-udp/blob/master/LICENSE
[hackage]: https://hackage.haskell.org/package/wsjtx-udp
[badge-hackage]: https://img.shields.io/hackage/v/wsjtx-udp.svg
