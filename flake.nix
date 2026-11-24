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
    in {
      packages.${system} = { inherit dagger; };

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
