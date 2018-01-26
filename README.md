# WSJT-X UDP Server

[![Build Status](https://travis-ci.org/MarcFontaine/wsjtx-udp.svg?branch=master)](https://travis-ci.org/MarcFontaine/wsjtx-udp)
[![Build status](https://ci.appveyor.com/api/projects/status/few8e9b1c8m77o61/branch/master?svg=true)](https://ci.appveyor.com/project/MarcFontaine/wsjtx-udp/branch/master)


This library provides:

* The types for WSJT-X UDP network messages.
* Functions for encoding and decoding of packages.
* UDP Server for receiving messages from WSJT-X and sending to WSJT-X.
* JSON instances for network messages.

The executable wsjtx-dump-udp
dumps all incoming UDP-packages to std-out (in JSON format).
