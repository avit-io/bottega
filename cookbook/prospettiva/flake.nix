{
  description = "A prospettiva model: nix build gives <env>/values.yaml, nix flake check proves the theorems";

  inputs.bottega.url = "github:avit-io/bottega";

  outputs = { self, bottega }: {
    packages.x86_64-linux.default = bottega.lib.prospettiva { src = ./.; model = "service.rkt"; };
    checks.x86_64-linux.default = self.packages.x86_64-linux.default;
  };
}
