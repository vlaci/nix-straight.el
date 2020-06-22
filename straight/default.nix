{ fetchFromGitHub, trivialBuild }:

trivialBuild rec {
  pname = "straight.el";
  ename = pname;
  src = fetchFromGitHub {
    owner = "raxod502";
    repo = "straight.el";
    rev = "59c92dd45085b8f8fc44ea0039c205f4a3c43b62";
    sha256 = "00ibxmgqfb5bqd4b9jqj8vdiszkph6vv64m1y3kf9xav15v8sfyx";
  };
}
