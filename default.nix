{ # Package set to build the Emacs environment from
  emacsPackages
  # You may override to use your version of `straight`
, straight ? null
  # Your `init.el` file to use for discovering and installing packages
, emacsInitFile
  # Additional argument to pass to Emacs or your init file
, emacsArgs ? []
  # Additional files you wish to load prior to executing package discovery
  # Good place to place to call `advice-add` from
, emacsLoadFiles ? []
  # Abort processing if a package not found in `emacsPackages`
  # Setting it to false will result in just skipping an unavailable package
, abortOnNotFound ? false }:

let
  libstraight = epkgs.callPackage ./libstraight.nix { inherit abortOnNotFound epkgs; };
  epkgs =
    if straight == null then
      emacsPackages.overrideScope' (self: super:
        { straight = self.callPackage ./straight {  }; })
    else
      emacsPackages;


  packageJSON = libstraight.packagesJSON {
    inherit emacsInitFile emacsLoadFiles emacsArgs;
  };

  packageList = override:
    libstraight.parsePackagesJSON (packageJSON.overrideAttrs override);

  emacsEnv = libstraight.emacsEnv { inherit emacsInitFile emacsLoadFiles emacsArgs; };

in {
  /* Final environment (.emacs.d) with the populated `straight`` repository

     The required packages can be acquired from a call to
     `packageList` function.

     Type: emacsEnv :: { straightDir :: string, packages :: [derivation] } -> derivation

     Example:
       emacsEnv {
         straightDir = "$out/straight";
         packages = packageList (super: { ... } );
       };
  */
  inherit emacsEnv;
  /* Package list inferred from processing `emacsInitFile`

     The passed function will be called via an `overrideAttrs
     call on the`underlying derivation.

     Type: packageList :: attrs -> attrs -> derivation

     Example:
       packageList (super: {
         src = ...;
         preInstall = ''
           ...
         ''
       });
  */
  inherit packageList;

  /* JSON formatted list of packages

     These are the packages needed to be populated in the
     `straight` repository. Mostly for diagnostic purposes.

     Type: packageJSON :: derivation

  */
  inherit  packageJSON;
}
