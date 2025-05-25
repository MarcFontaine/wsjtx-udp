{
  mkDerivation,
  aeson,
  base,
  binary,
  bytestring,
  lib,
  network,
  postgresql-simple,
  text,
  time,
}:
mkDerivation {
  pname = "wsjtx-udp";
  version = "0.5.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson
    base
    binary
    bytestring
    network
    text
    time
  ];
  executableHaskellDepends = [
    aeson
    base
    bytestring
    network
    postgresql-simple
  ];
  homepage = "https://github.com/MarcFontaine/wsjtx-udp";
  description = "WSJT-X UDP protocol";
  license = lib.licenses.bsd3;
}
