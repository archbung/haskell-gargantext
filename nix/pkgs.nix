{ pkgs       ? import ./pinned-23.11.nix {} }:

rec {
  inherit pkgs;
  ghc947 = if pkgs.stdenv.isDarwin
           then pkgs.haskell.compiler.ghc947.overrideAttrs (finalAttrs: previousAttrs: {
                patches = previousAttrs.patches ++ [
                            # Reverts the linking behavior of GHC to not resolve `-libc++` to `c++`.
                            (pkgs.fetchpatch {
                              url = "https://gist.githubusercontent.com/adinapoli/bf722db15f72763bf79dff13a3104b6f/raw/362da0aa3db5c530e0d276183ba68569f216d65a/ghc947-macOS-loadArchive-fix.patch";
                              sha256 = "sha256-0tHrkWRKFWUewj3uIA0DujVCXo1qgX2lA5p0MIsAHYs=";
                            })
                          ];
                })
           else pkgs.haskell.compiler.ghc947;
  cabal_install_3_10_1_0 = pkgs.haskell.lib.compose.justStaticExecutables pkgs.haskell.packages.ghc947.cabal-install;
  graphviz = pkgs.graphviz.overrideAttrs (finalAttrs: previousAttrs: {
                # Increase the YY_BUF_SIZE, see https://gitlab.iscpif.fr/gargantext/haskell-gargantext/issues/290#note_9015
                patches = [
                            (pkgs.fetchpatch {
                              url = "https://gist.githubusercontent.com/adinapoli/e93ca7b1d714d27f4af537716b03e3bb/raw/b9cc297c3465878da2d18ee92a3f9b8273923493/graphviz-yy-buf-size.patch";
                              sha256 = "sha256-8Q3tf37iYaPV50P+Vf/n263ordECiu5eKwONCy3ynV8=";
                            })
                          ];
                });

  igraph_0_10_4 = pkgs.igraph.overrideAttrs (finalAttrs: previousAttrs: {
    version = "0.10.4";

    nativeBuildInputs = previousAttrs.nativeBuildInputs or [] ++ [ pkgs.clang_12 ];

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
      "-DIGRAPH_GRAPHML_SUPPORT=OFF"
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
    ghc947
    cabal_install_3_10_1_0
    pkgs.haskellPackages.alex
    pkgs.haskellPackages.happy
    pkgs.haskellPackages.pretty-show
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
    pkg-config
    postgresql
    xz
    zlib
    blas
    gfortran7
    expat
    icu
    graphviz
    clang_12
    llvm_12
    gcc12
    igraph_0_10_4
    libpqxx
    libsodium
    zeromq
    curl
  ] ++ ( lib.optionals stdenv.isDarwin [
       darwin.apple_sdk.frameworks.Accelerate
       ]);
  libPaths = pkgs.lib.makeLibraryPath nonhsBuildInputs;
  shellHook = ''
    export LD_LIBRARY_PATH="${pkgs.gfortran7.cc.lib}:${libPaths}:$LD_LIBRARY_PATH"
    export LIBRARY_PATH="${pkgs.gfortran7.cc.lib}:${libPaths}"
    export PATH="${pkgs.gccStdenv}/bin:$PATH"
    export NIX_CC="${pkgs.gccStdenv}"
    export CC="${pkgs.gccStdenv}/bin/gcc"
  '';
  shell = pkgs.mkShell.override { stdenv = pkgs.gccStdenv; } {
    name = "gargantext-shell";
    buildInputs = hsBuildInputs ++ nonhsBuildInputs;
    inherit shellHook;
  };
}
