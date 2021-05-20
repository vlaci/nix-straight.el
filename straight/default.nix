{ fetchFromGitHub, trivialBuild }:

trivialBuild rec {
  pname = "straight.el";
  ename = pname;
  src = fetchFromGitHub {
    owner = "raxod502";
    repo = "straight.el";
    rev = "e1390a933b6f5a15079d6dec91eac97a17aad10c";
    sha256 = "sha256-fzIvJ5sDNDtF0w8t1WtM2SpnWT8zM2RG/EiSthy1URU=";
  };
}
