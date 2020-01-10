{ fetchurl, lib, stdenv }:

let
  version = "1.3.2";
  urlTemplate = arch: "https://github.com/saksdirect/okta-aws-login/releases/download/v${version}/okta-aws-login-${arch}";
  sources = {
    x86_64-linux = fetchurl {
      url = urlTemplate "Linux-x86_64";
      sha256 = "13fdixjv2x9ja4csflhij7namgbc7g0zjmzcy4p82qy5m7sxbh9v";
    };
    x86_64-darwin = fetchurl {
      url = urlTemplate "Darwin-x86_64";
      sha256 = "081mkzmzv6790rwnqprkwhfmrwc3vx3hs30r1pqx166667nzhwiw";
    };
  };
  unsupported = throw "Platform ${stdenv.hostPlatform.system} unsupported for okta-aws-login";
in
stdenv.mkDerivation rec {
  name = "okta-aws-login-${version}";
  src = lib.attrByPath [ stdenv.hostPlatform.system ] unsupported sources;

  dontUnpack = true;
  dontFixup = true;

  installPhase = ''
    mkdir -p $out/bin
    install -m 755 ${src} $out/bin/okta-aws-login
  '';
}
