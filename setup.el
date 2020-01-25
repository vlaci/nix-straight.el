;;; -*- lexical-binding: t; -*-
(require 'json)
(require 'straight)

(defun nix-straight-get-used-packages (init-file)
  (let ((nix-straight--packages nil))
    (advice-add 'straight-use-package
                :override (lambda (recipe &rest r)
                            (let ((pkg (if (listp recipe)
                                              (car recipe)
                                         recipe)))
                              (message "straight-use-package %s %s" recipe r)
                              (add-to-list 'nix-straight--packages pkg))))
    (advice-add 'straight-recipes-retrieve
                :override (lambda (pkg)
                            (list)))
    (advice-add 'straight-use-recipes
                :override (lambda (&rest r)
                            (message "straight-use-recipes %s" r)))

    (load init-file nil nil t)
    (princ (if (null nix-straight--packages)
               "[]"
             (json-encode nix-straight--packages)))

    nix-straight--packages))

(defun nix-straight-build-packages (init-file)
  (setq straight-default-files-directive '("*" (:exclude "*.elc")))
  (advice-add 'straight-vc-git-clone
              :override (lambda (&rest r)))
  (advice-add 'straight-recipes-retrieve
              :override (lambda (pkg &rest r)
                          (message "  Crafting recipe %s" pkg)
                          (let ((pkg-name (symbol-name pkg)))
                            (if (file-exists-p (straight--repos-dir pkg-name))
                                (list pkg :local-repo pkg-name :repo pkg-name :type 'git)
                              (message  "  --> Repo directory for package not exists, assuming built-in; %s" pkg)
                              (list pkg :type 'built-in)))))
  (load init-file nil nil t))

(provide 'setup)
;;; setup.el ends here
