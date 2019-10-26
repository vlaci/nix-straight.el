{ lib, stdenv, epkgs, writeScript }:

let
  inherit (builtins) filter trace;
  inherit (lib) concatMapStringsSep escapeShellArgs importJSON flatten unique optionalString;

  expandDependencies = packages:
    let
      withDeps = p:
        map (x:
          if x == null then [ ] else
          [ x ] ++ withDeps x.propagatedBuildInputs
        ) (flatten p);
   in (unique (filter (d: d ? ename) (flatten (withDeps packages))));

  install = repo: packages:
    let
      installPkg = repo: pkg: (''
        REPO=${repo}
        psrc=(${pkg}/share/emacs/*/*/${pkg.ename}*)
        if [[ ! -d $psrc ]]; then
          ln -snf ${pkg}/share/emacs/site-lisp $REPO/${pkg.ename}
        else
          ln -snf $psrc $REPO/${pkg.ename}
        fi
        ${optionalString ((pkg.src ? meta) && (pkg.src.meta ? homepage)) ''
          if [[ ! -d $REPO/${baseNameOf pkg.src.meta.homepage} ]]; then
            ln -snf $psrc $REPO/${baseNameOf pkg.src.meta.homepage}
          fi
        ''}
      '');
    in  writeScript "install-repo" ''
      mkdir -p ${repo}
      ${(concatMapStringsSep "\n" (installPkg repo) (expandDependencies packages))}
    '';

  parsePackagesJSON = json:
    let
      list = importJSON json;
    in map (x:
      if epkgs ? "${x}" then epkgs.${x}
      else (trace "XXX no attribute found for use-package ${x}") null) list;

  packagesJSON = { emacsInitFile, emacsLoadFiles, emacsArgs }: stdenv.mkDerivation {
    name = "emacs-straight-packages.json";
    buildInputs = [ epkgs.emacs ];
    buildPhase = ":";
    installPhase = ''
      runHook preInstall
      emacs -q      \
            --batch \
            --directory=${epkgs.straight}/share/emacs/site-lisp \
            --load=${./setup.el} \
            ${concatMapStringsSep "\n" (f: "--load=${f}") emacsLoadFiles} \
            --eval="(nix-straight-get-used-packages \"${emacsInitFile}\")" \
            ${escapeShellArgs emacsArgs} > $out
      runHook postInstall
    '';
  };

  emacsEnv = { emacsInitFile, emacsLoadFiles, emacsArgs }: { packages, straightDir }: stdenv.mkDerivation {
    name = "straight-emacs-env";
    buildPhase = ":";
    buildInputs = [ epkgs.emacs ];
    installPhase = ''
      runHook preInstall

      mkdir -p $out
      ${(install "${straightDir}/repos" packages)}
      emacs -q      \
            --batch \
            --directory=${epkgs.straight}/share/emacs/site-lisp \
            --load=${./setup.el} \
            ${concatMapStringsSep "\n" (f: "--load=${f}") emacsLoadFiles} \
            --eval="(nix-straight-build-packages \"${emacsInitFile}\")" ${escapeShellArgs emacsArgs}

      runHook postInstall
    '';
  };
in {
  inherit install parsePackagesJSON packagesJSON emacsEnv;
}
