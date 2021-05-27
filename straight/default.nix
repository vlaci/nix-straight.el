{ fetchFromGitHub, trivialBuild }:

trivialBuild rec {
  pname = "straight.el";
  ename = pname;
  src = fetchFromGitHub {
    owner = "raxod502";
    repo = "straight.el";
    rev = "5541697ceee7e7ba937eddebba44f1e8e8af6a4f";
    sha256 = "sha256-Ecf0QU/ZbCirGHYBSZGIA4Gb0ION56S/pOElYZXgytI=";
  };
}
