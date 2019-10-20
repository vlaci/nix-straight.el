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
                              (message "straight-use-package %s %s; pkg=%s" recipe r pkg)
                              (add-to-list 'nix-straight--packages pkg))))
    (advice-add 'straight-recipes-retrieve
                :override (lambda (pkg)
                            (list)))

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
                          (if (file-exists-p (straight--repos-dir (format "%s" pkg)))
                              `(,pkg :local-repo ,(format "%s" pkg))
                            (message  "  --> Repo directory for package not exists, assuming built-in; %s" pkg)
                            `(,pkg :type built-in))))
  (load init-file nil nil t))

(provide 'setup)
;;; setup.el ends here
