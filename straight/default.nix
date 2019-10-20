{ fetchFromGitHub, trivialBuild }:

trivialBuild rec {
  pname = "straight.el";
  ename = pname;
  src = fetchFromGitHub {
    owner = "raxod502";
    repo = "straight.el";
    rev = "388bf37f30f0934c8ef1b52edfd65875e945da21";
    sha256 = "1d8kg63yamkanrx6l8mi8ybj2xb8zmkhqc10q5p3xn319gs36jgp";
  };
}
