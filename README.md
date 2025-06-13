WSJT-X UDP Server
=================

[![Available on Hackage][badge-hackage]][hackage]
[![License](https://img.shields.io/github/license/MarcFontaine/wsjtx-udp)](https://github.com/MarcFontaine/wsjtx-udp/blob/master/LICENSE)
[![CI](https://github.com/MarcFontaine/wsjtx-udp/actions/workflows/haskell.yml/badge.svg)](https://github.com/MarcFontaine/wsjtx-udp/actions/workflows/haskell.yml)
[![GitHub Releases](https://img.shields.io/github/v/release/MarcFontaine/wsjtx-udp?include_prereleases&sort=semver)](https://github.com/MarcFontaine/wsjtx-udp/releases)

This library provides:

* Types for WSJT-X UDP network messages.
* Functions for encoding and decoding of messages.
* JSON instances for network messages.
* `wsjtx-dump-udp`: A UDP server for receiving messages from WSJT-X and sending to WSJT-X.
* `wsjtx-to-db`: A UDP server that receives messages and stores them in a Postgresql DB.
 
[hackage]: https://hackage.haskell.org/package/wsjtx-udp
[badge-hackage]: https://img.shields.io/hackage/v/wsjtx-udp.svg
