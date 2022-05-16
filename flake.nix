{
  description = "A very basic flake";

  outputs = { self, nixpkgs }: let
      pkgs = nixpkgs.legacyPackages.x86_64-linux;

      name = "ourstuff";

      t = pkgs.lib.trivial;

      project = returnShellEnv: pkgs.haskellPackages.developPackage {
        root = pkgs.lib.sourceFilesBySuffices ./. [ ".cabal" ".hs" ];
        inherit name;
        returnShellEnv = returnShellEnv;
        modifier = (t.flip t.pipe) [
          pkgs.haskell.lib.dontHaddock
          pkgs.haskell.lib.enableStaticLibraries
          pkgs.haskell.lib.justStaticExecutables
          pkgs.haskell.lib.disableLibraryProfiling
          pkgs.haskell.lib.disableExecutableProfiling
        ];
      };
  in {

    inherit project;

    packages.x86_64-linux.${name} = project false;
    defaultPackage.x86_64-linux = self.packages.x86_64-linux.${name};

    devShell.x86_64-linux = pkgs.mkShell {
      inputsFrom = [
        (project true)
      ];
      packages = [
        # package manager
        pkgs.cabal-install

        # dev tools
        pkgs.haskell-language-server
        pkgs.ghcid

        # lint
        pkgs.hlint

        # formatters
        pkgs.haskellPackages.cabal-fmt
        pkgs.haskellPackages.fourmolu

        # database
        pkgs.postgresql
        (pkgs.writeShellScriptBin "db" ''
          if [ ! -d $PGHOST ]; then
            mkdir -p $PGHOST
          fi
          if [ ! -d $PGDATA ]; then
            echo 'Initializing postgresql database...'
            initdb $PGDATA --auth=trust >/dev/null
            (
              X=8; until pg_isready -q -d postgres || [ $X -eq 0 ]; do sleep 0.1; let X=X-1; done
              pg_isready -q -d postgres && createdb && echo "created database $PGDATABASE" || echo "failed to init db because psql didnt start" >&2
            ) &
          fi
          exec postgres -c listen_addresses= -c unix_socket_directories=$PGHOST
        '')
      ];
      shellHook = ''
        export PGDATABASE=${name}
        export PGDATA=$PWD/.postgres_data
        export PGHOST=$PWD/.postgres
        export LOG_PATH=$PWD/.postgres/LOG
        export DATABASE_URL="postgresql:///$PGDATABASE?host=$PGHOST"
      '';
    };
  };
}
