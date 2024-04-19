;;; ../../.local/src/dotfiles/dot-config/doom/variants/matlab-setup.el -*- lexical-binding: t; -*-

;; associate .m file with the matlab-mode (major mode)
(autoload 'matlab-mode "matlab" "Matlab Editing Mode" t)
(add-to-list 'auto-mode-alist '("\\.m$" . matlab-mode))
;; setup matlab-shell
(setq matlab-shell-command "/home/davide/.local/bin/nix/matlab.sh"
      matlab-shell-command-switches (list "-nodesktop")
      matlab-indent-function t)
(defadvice! inhibit-real-only-a (oldfun &rest r)
 "Temporary remove read-only lines in shell buffer"
 :around#'matlab-shell-collect-command-output
  (let ((inhibit-read-only t)) (apply oldfun r)))
