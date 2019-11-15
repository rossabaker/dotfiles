self: super: {
  zoom-us =
    let version = "3.0.317369.1110";
    in super.zoom-us.overrideAttrs (_: {
      inherit version;
      src = super.fetchurl {
        url = "https://zoom.us/client/${version}/zoom_x86_64.tar.xz";
        sha256 = "0r4wp9qb1739xwr24kglc4sj8qaxwr4nh5p1igi3x6f1f8gczia7";
      };
    });
}
