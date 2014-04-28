;; Dependencies
(require 'session)
(require 'tramp)

;; Hooks
(add-hook 'after-init-hook 'session-initialize)

(fset 'yes-or-no-p 'y-or-n-p) ; use y or n instead of requiring "yes" or "no"

;; Autosave and backup
; Put autosave files (ie #foo#) in one place, *not* scattered all over
; the file system!
(defvar autosave-dir (concat "~/.emacs.d/autosaves/" (user-login-name) "/"))
(make-directory autosave-dir t)

; These functions override the construction of autosave file names
(defun auto-save-file-name-p (filename)
  (string-match "^#.*#$" (file-name-nondirectory filename)))
(defun make-auto-save-file-name ()
  (concat autosave-dir
   (if buffer-file-name
      (concat "#" (file-name-nondirectory buffer-file-name) "#")
    (expand-file-name
     (concat "#%" (buffer-name) "#")))))

;; Put backup files (ie foo~) in one place too. (The backup-directory-alist
;; list contains regexp=>directory mappings; filenames matching a regexp are
;; backed up in the corresponding directory. Emacs will mkdir it if necessary.)
(defvar backup-dir (concat "~/.emacs.d/backups/" (user-login-name) "/"))
(setq backup-directory-alist (list (cons "." backup-dir)))


(setq browse-url-browser-function 'browse-url-generic
      browse-url-generic-program "google-chrome"
      calendar-week-start-day 1

      ;; UI
      column-number-mode t ; Show column number
      inhibit-startup-message t ; don't show startup message
      menu-bar-mode t ; Show menu bar (override better-defaults disabling it)
      scroll-bar-mode nil ; Hide scroll bars
      tool-bar-mode nil ; Hide tool bar
      uniquify-buffer-name-style 'post-forward
      uniquify-separator ":" frame-title-format "%b %f"
      version-control t

      ;; File behhavior
      delete-old-versions t
      tramp-default-method "scp"

      ;; Session
      session-save-file "/home/pib/.emacs.d/.session"
      )

;; Custom functions