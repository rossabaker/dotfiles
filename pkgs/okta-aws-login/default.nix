{ fetchurl, stdenv }:

stdenv.mkDerivation rec {
  version = "1.3.2";
  name = "okta-aws-login-${version}";

  src = fetchurl {
    url = "https://github.com/saksdirect/okta-aws-login/releases/download/v${version}/okta-aws-login-Linux-x86_64";
    sha256 = "13fdixjv2x9ja4csflhij7namgbc7g0zjmzcy4p82qy5m7sxbh9v";
  };

  dontUnpack = true;
  dontFixup = true;

  installPhase = ''
    mkdir -p $out/bin
    install -m 755 ${src} $out/bin/okta-aws-login
  '';
}
