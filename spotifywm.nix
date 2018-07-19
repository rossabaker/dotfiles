{ stdenv, fetchFromGitHub, makeWrapper, substituteAll, spotify, libX11 }:
stdenv.mkDerivation rec {
  name = "spotifywm";

  src = "${fetchFromGitHub {
    owner = "dasJ";
    repo = "spotifywm";
    rev = "91dd553";
    sha256 = "01z088i83410bpx1vbp7c6cq01r431v55l7340x3izp53lnpp379";
  }}";

  buildInputs = [ spotify libX11 makeWrapper ];

  installPhase = ''
    make spotifywm
    mkdir -p $out/bin $out/lib
    cp spotifywm.so $out/lib/spotifywm.so
    makeWrapper ${spotify}/bin/spotify $out/bin/spotify --set LD_PRELOAD $out/lib/spotifywm.so
  '';

  meta = with stdenv.lib; {
    description = "Set Spotify's WM_NAME before opening the window";
    license = licenses.mit;
    maintainers = with maintainers; [ rossabaker ];
  };
}
