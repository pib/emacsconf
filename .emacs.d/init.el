(add-to-list 'load-path  "~/.emacs.d/elisp/")

(require 'package)
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
(package-initialize)

;(require 'org-trello)
;(add-hook 'org-mode-hook 'org-trello-mode)

(require 'better-defaults) ; start with basic better defaults
(add-hook 'after-init-hook #'global-flycheck-mode)
(server-start) ; run server so emacsclient can connect

;; Config files should be organized with the following order as much
;; as possible...
; 0. dependencies (requires)
; 1. Key bindings
; 2. Hooks/auto-mode-alist settings
; 3. setq block of settings
; 4. Custom functions
; ...with each section's contents sorted alphabetically for easy
; searching.

; General editor settings
(load "~/.emacs.d/editor-settings.el")

; Language settings
(load "~/.emacs.d/language-settings.el")

; Org mode settings
(load "~/.emacs.d/org-settings.el")

; emms-mode settings
;(load "~/.emacs.d/emms-settings.el")

; Custom vars set interactively
(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(column-number-mode t)
 '(help-at-pt-display-when-idle (quote (flymake-overlay)) nil (help-at-pt))
 '(safe-local-variable-values (quote ((eval setenv "GOPATH" "/home/pib/marketvibe/scraper/.goat/deps:/home/pib/projects/go"))))
 '(session-use-package t nil (session))
 '(show-paren-mode t)
 '(tool-bar-mode nil))

(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(default ((t (:stipple nil :background "white" :foreground "black" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 98 :width normal :foundry "adobe" :family "Source Code Pro")))))
