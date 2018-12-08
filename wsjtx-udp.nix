{ mkDerivation, aeson, base, binary, binary-parsers, bytestring
, network, stdenv, text, time
}:
mkDerivation {
  pname = "wsjtx-udp";
  version = "0.1.3.5";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson base binary binary-parsers bytestring network text time
  ];
  executableHaskellDepends = [ base ];
  homepage = "https://github.com/MarcFontaine/wsjtx-udp";
  description = "WSJT-X UDP protocol";
  license = stdenv.lib.licenses.bsd3;
}
