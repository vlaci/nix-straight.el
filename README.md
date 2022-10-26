**:warning: I no longer use or develop this package. Use the (nix-community)[https://github.com/nix-community/nix-straight.el] mirror instead.**

# nix-straight.el

Low-level Nix integration to [straight.el](https://github.com/raxod502/straight.el)

See [nix-doom-emacs](https://github.com/vlaci/nix-doom-emacs) for a usage example.

There are a couple of issues you may need to be aware of:
* Package names are dumbly inferred using the `meta.homepage` and `ename`
  attributes. If they are both present and not match, then the packages is
  instaled under both names in the `repos` directory.
* Packages are symlinked from the store so all VCS related operations are
  unavailable and package contents may differ from the ones would be installed
  from e.g. MELPA.
* Recipe targets are totally ignored, so there is currently no way of knowing if
  a derivation from melpa would be used instead of a fork for example. However I
  am planning to add a tool to generate Nix expressions from straight recipes in
  the future.
