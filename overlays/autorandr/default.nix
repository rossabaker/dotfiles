self: super: {
  autorandr = super.autorandr.overrideAttrs (
    old: rec {
      version = "1.10.1-ross";

      src = self.fetchFromGitHub {
        owner = "rossabaker";
        repo = "autorandr";
        rev = "4526fff46b7041d9044e77c0b318fecb664b815e";
        sha256 = "09pkb6n1kq4kp5h21v3vmzrjn7831c8kb145sdybwjbm4vq93hvk";
      };
    }
  );
}
