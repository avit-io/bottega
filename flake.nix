{
  description = "Bottega: a Racket core with three dialects, disegno, prospettiva and affresco";

  inputs.nixpkgs.url = "github:NixOS/nixpkgs/nixos-25.11";

  outputs = { self, nixpkgs, ... }:
    let
      # ponytail: x86_64-linux only, like disegno; genAttrs over systems when needed.
      system = "x86_64-linux";
      pkgs = nixpkgs.legacyPackages.${system};

      # Dagger 1.0, still in beta: the latest one published on dl.dagger.io.
      # ponytail: back to github:dagger/nix when 1.0 is out.
      dagger = pkgs.stdenvNoCC.mkDerivation rec {
        pname = "dagger";
        version = "1.0.0-beta.28";
        src = pkgs.fetchurl {
          url = "https://dl.dagger.io/dagger/releases/${version}/dagger_v${version}_linux_amd64.tar.gz";
          hash = "sha256-q9n36V8DxRfjPPurCRf6FZPXPgiNaTBCWYhPikfMYaE=";
        };
        sourceRoot = ".";
        installPhase = "install -Dm755 dagger $out/bin/dagger";
      };

      # The Dagger engine, behind a TLS-inspecting proxy (Netskope). Normally the CLI starts its
      # own engine on the host's Docker; behind the proxy that engine cannot pull images. So
      # Bottega starts one with the host's CA bundle mounted: the engine installs the CAs from
      # /usr/local/share/ca-certificates and passes them to the containers it runs.
      # BOTTEGA_HOST_CA picks the bundle: a host path, or none; by default the system's.
      # Prints the engine's name, or nothing if Dagger's own will do.
      # ponytail: if the host's bundle changes, `docker rm -f bottega-engine-v…` and it is recreated.
      engine = pkgs.writeShellScript "bottega-engine" ''
        host_ca="''${BOTTEGA_HOST_CA:-}"
        if [ -z "$host_ca" ] && [ -e /etc/ssl/certs/ca-bundle.crt ]; then
          host_ca=$(readlink -f /etc/ssl/certs/ca-bundle.crt)
        fi
        [ -n "$host_ca" ] && [ "$host_ca" != none ] || exit 0
        name=bottega-engine-v${dagger.version}
        if [ "$(docker inspect -f '{{.State.Running}}' $name 2>/dev/null)" != true ]; then
          docker rm -f $name >/dev/null 2>&1
          docker run -d --privileged --name $name \
            -v "$host_ca:/usr/local/share/ca-certificates/host-bundle.crt:ro" \
            -v $name:/var/lib/dagger registry.dagger.io/engine:v${dagger.version} >/dev/null
        fi
        echo $name
      '';
      # Bottega as a package: the repository's collections, compiled, and a racket that sees them.
      # The flake's source is what git tracks, so compiled/ stays out.
      bottega = pkgs.stdenvNoCC.mkDerivation {
        pname = "bottega";
        version = "0.1";
        src = self;
        nativeBuildInputs = [ pkgs.racket pkgs.makeWrapper ];
        buildPhase = ''
          export HOME=$TMPDIR PLTCOLLECTS="$PWD:"
          raco make bottega/main.rkt bottega/lib/*.rkt prospettiva/render.rkt prospettiva/language.rkt \
                    disegno/run.rkt disegno/language.rkt disegno/lib/*.rkt \
                    affresco/run.rkt affresco/language.rkt affresco/lib/*.rkt
        '';
        installPhase = ''
          mkdir -p $out/share/bottega
          cp -r info.rkt bottega prospettiva disegno affresco $out/share/bottega/
          makeWrapper ${pkgs.racket}/bin/racket $out/bin/racket --set PLTCOLLECTS "$out/share/bottega:"
          makeWrapper ${pkgs.racket}/bin/raco $out/bin/raco --set PLTCOLLECTS "$out/share/bottega:"
        '';
      };

      # A prospettiva model, built: its types and theorems are checked, and every environment
      # becomes <env>/values.yaml in the output. No IFD: Nix builds the files, it does not read them.
      #   bottega.lib.prospettiva { src = ./.; model = "model.rkt"; }
      # ponytail: one derivation for all the environments; one each would mean listing them here too.
      prospettiva = { src, model, name ? "prospettiva" }:
        pkgs.runCommand name { } ''
          export HOME=$TMPDIR
          ${bottega}/bin/racket ${src}/${model} render $out
        '';

      # An affresco program: compiling it checks its types and proofs; bin/<name> PORT serves it.
      #   bottega.lib.affresco { src = ./.; program = "app.rkt"; name = "app"; }
      affresco = { src, program, name ? "affresco" }:
        pkgs.runCommand name { nativeBuildInputs = [ pkgs.makeWrapper ]; meta.mainProgram = name; } ''
          export HOME=$TMPDIR
          mkdir -p $out/share $out/bin
          cp -r ${src} $out/share/app && chmod -R u+w $out/share/app
          ${bottega}/bin/raco make $out/share/app/${program}
          makeWrapper ${bottega}/bin/racket $out/bin/${name} --add-flags "$out/share/app/${program} serve"
        '';
    in {
      packages.${system} = {
        inherit dagger bottega;
        default = bottega;
        example = prospettiva { src = ./cookbook/prospettiva; model = "service.rkt"; name = "service"; };
        counter = affresco { src = ./cookbook/affresco; program = "counter.rkt"; name = "counter"; };
      };

      lib = { inherit prospettiva affresco; };

      # nix flake check: the example's theorems, and the test suite.
      checks.${system} = {
        example = self.packages.${system}.example;
        counter = self.packages.${system}.counter;
        tests = pkgs.runCommand "bottega-tests" { } ''
          export HOME=$TMPDIR
          cp -r ${self} src && chmod -R u+w src && cd src
          PLTCOLLECTS="$PWD:" ${pkgs.racket}/bin/raco test tests/ disegno/graphql.rkt bottega/core/primitives.rkt
          touch $out
        '';
      };

      # nix flake init -t github:avit-io/bottega#prospettiva
      templates.prospettiva = {
        path = ./cookbook/prospettiva;
        description = "A prospettiva model: one service, its environments as projections, built by Nix";
      };
      templates.affresco = {
        path = ./cookbook/affresco;
        description = "An affresco program: Elm's architecture, total, on the server; its proofs are its tests";
      };

      # PLTCOLLECTS: the repository is the bottega collection, without raco pkg install.
      devShells.${system}.default = pkgs.mkShell {
        packages = [ pkgs.racket dagger ];
        shellHook = ''
          export PLTCOLLECTS="$PWD:"
          export DAGGER_NO_NAG=1
          name=$(${engine})
          if [ -n "$name" ]; then export DAGGER_ENGINE=container://$name; fi
        '';
      };
    };
}
