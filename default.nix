{ mkDerivation, async, base, BoundedChan, bytestring, cereal
, containers, directory, filepath, process, SHA, stdenv, unix
}:
mkDerivation {
  pname = "document-repository-tool";
  version = "0.1.0.3";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  buildDepends = [
    async base BoundedChan bytestring cereal containers directory
    filepath process SHA unix
  ];
  description = "Tool to collect pdf, ps, djvu, etc documents into a repository";
  license = stdenv.lib.licenses.gpl3;
}
