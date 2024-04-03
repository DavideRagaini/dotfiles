;;; ../../sources/dotfiles/dot-config/doom/load/erc.el -*- lexical-binding: t; -*-
(setq erc-fill-column 120
      erc-fill-function 'erc-fill-static
      erc-fill-static-center 20)

(setq erc-track-exclude '("#emacs")
      erc-track-exclude-types '("JOIN" "NICK" "QUIT" "MODE" "AWAY")
      erc-hide-list '("JOIN" "NICK" "QUIT" "MODE" "AWAY")
      erc-track-exclude-server-buffer t)

(setq erc-track-visibility nil) ; Only use the selected frame for visibility

(setq erc-pals '("daviwil" "ashraz" "shom_" "masteroman" "benoitj")
      erc-fools '("daviwil-test")
      erc-keywords '("guix" "emacs" "linux" "qtile" "sway" "home manager" "wiki" "nixos"))

;; (add-to-list 'erc-modules 'notifications)

;; (use-package! erc-hl-nicks
;;   :ensure t
;;   :after erc
;;   :config
;;   (add-to-list 'erc-modules 'hl-nicks))

;; (use-package! erc-image
;;   :ensure t
;;   :after erc
;;   :config
  ;; (setq erc-image-inline-rescale 300))
;; (setq erc-image-inline-rescale 300)
  ;; (add-to-list 'erc-modules 'image))

;; (use-package emojify
;;   :ensure t
;;   :hook (erc-mode . emojify-mode)
;;   :commands emojify-mode)

(defun my/connect-irc ()
  (interactive)
  (erc-tls
      :server "irc.libera.chat"
      :port 6697
      :nick "raghyz"))
      ;; This is using password-store.el.  Not needed if you use auth-source!
      ;; :password (password-store-get "IRC/irc.libera.chat")))

(global-set-key (kbd "C-c c c") 'my/connect-irc)

(setq erc-track-enable-keybindings t)

;; (setq erc-prompt-for-password nil)

;; RESOURCES
;; [[https://systemcrafters.net/live-streams/june-04-2021/][Improving the IRC Experience in ERC - System Crafters]]
;; [[https://systemcrafters.net/irc-tips/][IRC Tips and Configurations - System Crafters]]
