{ pkgs       ? import ./pinned-22.05.nix {} }:

rec {
  inherit pkgs;
  # If we are on a Mac, in order to build successfully with cabal we need a bit more work.
  ghc = if pkgs.stdenv.isDarwin
           then haskell1.compiler.ghc8107.overrideAttrs (finalAttrs: previousAttrs: {
                # See https://github.com/NixOS/nixpkgs/pull/149942/files
                patches = previousAttrs.patches ++ [
                            # Reverts the linking behavior of GHC to not resolve `-libc++` to `c++`.
                            (pkgs.fetchpatch {
                              url = "https://raw.githubusercontent.com/input-output-hk/haskell.nix/613ec38dbd62ab7929178c9c7ffff71df9bb86be/overlays/patches/ghc/ghc-macOS-loadArchive-fix.patch";
                              sha256 = "0IUpuzjZb1G+gP3q6RnwQbW4mFzc/OZ/7QqZy+57kx0=";
                            })
                          ];
                })
           else pkgs.haskell.compiler.ghc8107;
  graphviz = pkgs.graphviz.overrideAttrs (finalAttrs: previousAttrs: {
                # Increase the YY_BUF_SIZE, see https://gitlab.iscpif.fr/gargantext/haskell-gargantext/issues/290#note_9015
                patches = [
                            (pkgs.fetchpatch {
                              url = "https://gist.githubusercontent.com/adinapoli/e93ca7b1d714d27f4af537716b03e3bb/raw/b9cc297c3465878da2d18ee92a3f9b8273923493/graphviz-yy-buf-size.patch";
                              sha256 = "sha256-8Q3tf37iYaPV50P+Vf/n263ordECiu5eKwONCy3ynV8=";
                            })
                          ];
                });
  haskell1 = pkgs.haskell // {
      packages = pkgs.haskell.packages // {
        ghc8107 = pkgs.haskell.packages.ghc8107.override {
          overrides = self: super: {
            directory            = self.callPackage ./overlays/directory-1.3.7.0.nix {};
            process              = self.callPackage ./overlays/process-1.6.15.0.nix {};
            hackage-security     = self.callPackage ./overlays/hackage-security-0.6.2.3.nix {};
            Cabal                = self.callPackage ./overlays/Cabal-3.10.1.0.nix {};
            Cabal-syntax         = self.callPackage ./overlays/Cabal-syntax-3.10.1.0.nix {};
            cabal-install-solver = self.callPackage ./overlays/cabal-install-solver-3.10.1.0.nix {};
            cabal-install        = self.callPackage ./overlays/cabal-install-3.10.1.0.nix {};
          };
        };
      };
  };
  cabal_install_3_10_1_0 = pkgs.haskell.lib.compose.justStaticExecutables haskell1.packages.ghc8107.cabal-install;

  igraph_0_10_4 = pkgs.igraph.overrideAttrs (finalAttrs: previousAttrs: {
    version = "0.10.4";

    src = pkgs.fetchFromGitHub {
      owner = "igraph";
      repo = "igraph";
      rev = "0.10.4";
      hash = "sha256-LsTOxUktGZcp46Ec9QH3+9C+VADMYTZZCjKF1gp36xk=";
    };

    postPatch = ''
      echo "0.10.4" > IGRAPH_VERSION
    '';

    outputs = [ "out" "doc" ];

    buildInputs = [
      pkgs.arpack
      pkgs.blas
      pkgs.glpk
      pkgs.gmp
      pkgs.lapack
      pkgs.libxml2
      pkgs.plfit
    ] ++ pkgs.lib.optionals pkgs.stdenv.cc.isClang [
      pkgs.llvmPackages.openmp
    ];

    cmakeFlags = [
      "-DIGRAPH_USE_INTERNAL_BLAS=OFF"
      "-DIGRAPH_USE_INTERNAL_LAPACK=OFF"
      "-DIGRAPH_USE_INTERNAL_ARPACK=OFF"
      "-DIGRAPH_USE_INTERNAL_GLPK=OFF"
      "-DIGRAPH_USE_INTERNAL_GMP=OFF"
      "-DIGRAPH_USE_INTERNAL_PLFIT=OFF"
      "-DIGRAPH_GLPK_SUPPORT=ON"
      "-DIGRAPH_GRAPHML_SUPPORT=ON"
      "-DIGRAPH_OPENMP_SUPPORT=ON"
      "-DIGRAPH_ENABLE_LTO=AUTO"
      "-DIGRAPH_ENABLE_TLS=ON"
      "-DBUILD_SHARED_LIBS=ON"
    ];

    postInstall = ''
      mkdir -p "$out/share"
      cp -r doc "$out/share"
    '';

    postFixup = previousAttrs.postFixup + ''
      CUR_DIR=$PWD
      cd "$out/include/igraph" && cp *.h ../
      cd $CUR_DIR
    '';

  });
  hsBuildInputs = [
    ghc
    cabal_install_3_10_1_0
  ];
  nonhsBuildInputs = with pkgs; [
    bzip2
    czmq
    docker-compose
    git
    gmp
    gsl
    #haskell-language-server
    hlint
    libffi
    lapack
    lzma
    pcre
    pkgconfig
    postgresql
    xz
    zlib
    blas
    gfortran7
    #    gfortran7.cc.lib
    expat
    icu
    graphviz
    llvm_9
    igraph_0_10_4
    libpqxx
    libsodium
  ] ++ ( lib.optionals stdenv.isDarwin [
       darwin.apple_sdk.frameworks.Accelerate
       ]);
  libPaths = pkgs.lib.makeLibraryPath nonhsBuildInputs;
  shellHook = ''
    export LD_LIBRARY_PATH="${pkgs.gfortran7.cc.lib}:${libPaths}:$LD_LIBRARY_PATH"
    export LIBRARY_PATH="${pkgs.gfortran7.cc.lib}:${libPaths}"
  '';
  shell = pkgs.mkShell {
    name = "gargantext-shell";
    buildInputs = hsBuildInputs ++ nonhsBuildInputs;
    inherit shellHook;
  };
}
