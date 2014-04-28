;;;; Pylint/PyFlymake setup
(load "pylint")

(when (load-file "~/.emacs.d/flymake/flymake.el")
  (defun flymake-pylint-init (&optional trigger-type)
    (let* ((temp-file (flymake-init-create-temp-buffer-copy
                       'flymake-create-temp-inplace))
           (local-file (file-relative-name
                        temp-file
                        (file-name-directory buffer-file-name)))
           (options (when trigger-type (list "--trigger-type" trigger-type))))
      (list "~/.emacs.d/flymake/pyflymake.py" (append options (list local-file)))))
  
  (add-to-list 'flymake-allowed-file-name-masks
               '("\\.py\\'" flymake-pylint-init)))

(add-hook 'python-mode-hook '(lambda () (flymake-mode)))

;;;; Nosetests setup
(require 'nose)
(setq nose-global-name "/home/pib/.virtualenvs/py3/bin/nosetests")
(add-to-list 'nose-project-names ".run_nose")
(setq nose-use-verbose nil)

;;;; autopep8 setup
(require 'py-autopep8)
(add-hook 'before-save-hook 'python-fmt-before-save)
;;(remove-hook 'before-save-hook 'python-fmt-before-save)
