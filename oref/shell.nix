with import (fetchTarball https://github.com/NixOS/nixpkgs/archive/18.03.tar.gz) {};
let ghc = haskell.compiler.ghc802;
in haskell.lib.buildStackProject {
    inherit ghc;
    name = "oref";
    buildInputs = [ zlib unzip ];
    LANG = "en_US.UTF-8";
    TMPDIR = "/tmp";
}
