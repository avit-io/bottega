{
  description = "An affresco program: nix build proves it, nix run serves it";

  inputs.bottega.url = "github:avit-io/bottega";

  outputs = { self, bottega }: {
    # nix run . -- 8080
    packages.x86_64-linux.default = bottega.lib.affresco { src = ./.; program = "counter.rkt"; name = "counter"; };
    checks.x86_64-linux.default = self.packages.x86_64-linux.default;
  };
}
