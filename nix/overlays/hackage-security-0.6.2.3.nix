{ mkDerivation, aeson, base, base16-bytestring, base64-bytestring
, bytestring, Cabal, Cabal-syntax, containers, cryptohash-sha256
, directory, ed25519, filepath, ghc-prim, lib, lukko, mtl, network
, network-uri, parsec, pretty, QuickCheck, tar, tasty, tasty-hunit
, tasty-quickcheck, template-haskell, temporary, text, time
, transformers, unordered-containers, vector, zlib
}:
mkDerivation {
  pname = "hackage-security";
  version = "0.6.2.3";
  sha256 = "52ee0576971955571d846b8e6c09638f89f4f7881f4a95173e44ccc0d856a066";
  revision = "3";
  editedCabalFile = "1vdmpklil8a6r03ixzch5d36ngimmq5q8931i8bg9f7hh8nmq8jv";
  libraryHaskellDepends = [
    base base16-bytestring base64-bytestring bytestring Cabal
    Cabal-syntax containers cryptohash-sha256 directory ed25519
    filepath ghc-prim lukko mtl network network-uri parsec pretty tar
    template-haskell time transformers zlib
  ];
  testHaskellDepends = [
    aeson base bytestring Cabal Cabal-syntax containers network-uri
    QuickCheck tar tasty tasty-hunit tasty-quickcheck temporary text
    time unordered-containers vector zlib
  ];
  homepage = "https://github.com/haskell/hackage-security";
  description = "Hackage security library";
  license = lib.licenses.bsd3;
  doCheck = false;
  doHaddock = false;
}
