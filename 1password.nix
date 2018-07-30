# https://github.com/NixOS/nixpkgs/pull/43981/files
{ stdenv, fetchzip }:

stdenv.mkDerivation rec {
  name = "1password-${version}";
  version = "0.5.1";
  src =
    if stdenv.system == "i686-linux" then
    fetchzip {
    url = "https://cache.agilebits.com/dist/1P/op/pkg/v${version}/op_linux_386_v${version}.zip";
    sha256 = "1vpl6pwqc718n5a5p4lghyb5360h0vnbhlphydbwfqn6fc2xgj40";
    stripRoot = false;
  }
  else if stdenv.system == "x86_64-linux" then
    fetchzip {
    url = "https://cache.agilebits.com/dist/1P/op/pkg/v${version}/op_linux_amd64_v${version}.zip";
    sha256 = "1bsbzaqws0z991r6rkjrxay74fj4g5ld4d748ygr0950zwi1m3h7";
    stripRoot = false;
  }
  else if stdenv.system == "x86_64-darwin" then
    fetchzip {
    url = "https://cache.agilebits.com/dist/1P/op/pkg/v${version}/op_darwin_amd64_v${version}.zip";
    sha256 = "14dxa0wqaz9kihikzq6f8ig6xgvd5g3fx5hb9qf2sgzs8xxx7xs8";
    stripRoot = false;
  }
  else throw "Architecture not supported";

  installPhase = ''
    install -D op $out/bin/op
  '';
  postFixup = stdenv.lib.optionalString stdenv.isLinux ''
    patchelf \
    --set-interpreter $(cat $NIX_CC/nix-support/dynamic-linker) \
    $out/bin/op
  '';

  meta = with stdenv.lib; {
    description = "1Password command-line tool";
    homepage    = [
      "https://blog.agilebits.com/2017/09/06/announcing-the-1password-command-line-tool-public-beta/"
      "https://app-updates.agilebits.com/product_history/CLI"
    ];
    maintainers = with maintainers; [ joelburget ];
    license     = licenses.unfree;
    platforms   = [ "i686-linux" "x86_64-linux" "x86_64-darwin" ];
  };
}
