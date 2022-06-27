{ lib, mkDerivation, aeson, base, binary, binary-parsers, bytestring
, network, postgresql-simple, text, time
}:
mkDerivation {
  pname = "wsjtx-udp";
  version = "0.3.2.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson base binary binary-parsers bytestring network text time
  ];
  executableHaskellDepends = [ base network postgresql-simple ];
  homepage = "https://github.com/MarcFontaine/wsjtx-udp";
  description = "WSJT-X UDP protocol";
  license = lib.licenses.bsd3;
}
