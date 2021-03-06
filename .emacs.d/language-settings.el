;; Shared Language settings
(require 'electric)
(require 'help-at-pt)
(require 'mmm-auto)
(require 'auto-complete-config)
(load "flymake/flymake.el")

;; Autocomplete stuff
(add-to-list 'ac-dictionary-directories "~/.emacs.d/ac-dict")
(ac-config-default)
(define-key ac-mode-map (kbd "M-TAB") 'auto-complete)
;;(ac-flyspell-workaround)

(setq glasses-face (quote bold)
      glasses-original-separator ""
      glasses-separate-parentheses-p nil
      glasses-separator ""
      help-at-pt-display-when-idle (quote (flymake-overlay))
      help-at-pt-timer-delay 0.5

      ;flymake-log-level 3
      ;flymake-log-level -1
      ;help-at-pt-display-when-idle '(flymake-overlay))

      indent-tabs-mode nil
      mmm-global-mode 'maybe
      show-paren-mode t
      tab-width 4
      which-function-mode t
      which-func-modes '(ada-mode c++-mode c-mode cperl-mode emacs-lisp-mode
                                  f90-mode fortran-mode makefile-mode perl-mode
                                  php-mode sh-mode)
      )


;; Other language settings below in alphabetical order

;; C
(setq c-default-style "k&r"
      c-basic-offset 4)
(add-hook 'c-mode-hook 'textmate-mode)

;; CSV
(add-to-list 'auto-mode-alist '("\\.[Cc][Ss][Vv]\\'" . csv-mode))
(autoload 'csv-mode "csv-mode"
  "Major mode for editing comma-separated value files." t)

;; Fantom
(require 'fan-mode)
(add-to-list 'auto-mode-alist '("\\.fan$" . fan-mode))

;; Go
(require 'go-mode-load)
(add-hook 'go-mode-hook '(lambda () (glasses-mode)))
(add-hook 'before-save-hook 'gofmt-before-save)
(require 'go-autocomplete)
(add-to-list 'load-path "~/projects/go/src/github.com/dougm/goflymake")
(require 'go-flymake)
(add-hook 'go-mode-hook 'textmate-mode)

;; Javascript
(eval-after-load 'js2-mode
  '(progn
     (require 'js2-imenu-extras)
     (js2-imenu-extras-setup)))
(autoload 'js2-mode "js2-mode.el" "Major mode for editing JS files" t)
(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
(add-to-list 'interpreter-mode-alist '("node" . js2-mode))
(add-hook 'js2-mode-hook
  (lambda ()
    ;;; make emacs recognize the error format produced by jslint
    (set (make-local-variable 'compilation-error-regexp-alist)
       '(("^\\([a-zA-Z.0-9_/-]+\\):\\([0-9]+\\):\\([0-9]+\\)" 1 2 3)))
    (set (make-local-variable 'compile-command)
       (let ((file (file-name-nondirectory buffer-file-name)))
          (concat "rhino /home/pib/jslint.js " file)))))

(add-hook 'js2-mode-hook 'textmate-mode)

(setq js-indent-level 2
      js2-basic-offset 2
      js2-bounce-indent t)

;; Lisp
(setq inferior-lisp-program "/usr/bin/clisp")
(require 'slime)
(slime-setup)
(add-hook 'lisp-mode-hook 'textmate-mode)


;; Lua
(setq lua-indent-level 4)
(require 'love-minor-mode)
(add-hook 'lua-mode-hook 'textmate-mode)


;; Mako
(load "mmm-mako")
(add-to-list 'auto-mode-alist '("\\.mako\\'" . html-mode))
(mmm-add-mode-ext-class 'html-mode "\\.mako\\'" 'mako)

;; Markdown
(autoload 'markdown-mode "markdown-mode" "Major mode for editing Markdown files" t)
(add-to-list 'auto-mode-alist '("\\.text" . markdown-mode))

;; Mustache
(require 'mustache-mode)
(add-hook 'mustache-mode-hook 'textmate-mode)



;; Python has enough stuff it needs its own file
(load "~/.emacs.d/python-settings.el")

;; Ruby
(autoload 'ruby-mode "ruby-mode" "Major mode for editing ruby scripts." t)
(add-to-list 'auto-mode-alist  '(".rb$" . ruby-mode))
(add-to-list 'auto-mode-alist  '(".rhtml$" . html-mode))
(add-hook 'ruby-mode-hook 'textmate-mode)


;; Yaml
(require 'yaml-mode)
(add-to-list 'auto-mode-alist '("\\.yaml$" . yaml-mode))
(add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode))
