;; org-mode key bindings
(define-key global-map "\C-ca" 'org-agenda)
(define-key global-map "\C-cl" 'org-store-link)
(define-key global-map "\C-cr" 'org-remember)
(global-set-key (kbd "C-c g") 'gtd)

;; Turn on org-mode for .org files
(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))
(add-hook 'org-agenda-mode-hook 'hl-line-mode)
(add-hook 'remember-mode-hook 'org-remember-apply-template)
(add-hook 'org-mode-hook 'textmate-mode)

;; Configure org-mode

;; First section is out of order to be more useful, the rest are
;; ordered alphabetically.
(setq org-directory "~/Dropbox/org"
      org-default-notes-file (concat org-directory "/gtd/notes.org")
      org-mobile-directory (concat org-directory "/stage/")
      org-mobile-inbox-for-pull (concat org-directory "/gtd/gtd.org")

      org-agenda-files `(,(concat org-directory "/ideas.org")
                         ,(concat org-directory "/glidetv.org")
                         ,(concat org-directory "/gtd/birthday.org")
                         ,(concat org-directory "/gtd/gtd.org"))

      org-refile-targets `((nil :maxlevel . 2)
                           (org-agenda-files :maxlevel . 2)
                           (,(concat org-directory "/gtd/notes.org") :maxlevel . 2)
                           (,(concat org-directory "/gtd/someday.org") :maxlevel . 2))

      org-remember-templates `(("Todo" ?t "* TODO %^{Brief Description} %^g\n%?\nAdded: %U"
                                ,(concat org-directory "/gtd/gtd.org") "Tasks")
                               ("Journal" ?j "\n* %^{topic} %T \n%i%?\n"
                                ,(concat org-directory "/journal.org"))
                               ("Notes" ?n "\n* %^{topic} %T \n%i%?\n"
                                ,(concat org-directory "/gtd/notes.org"))
                               ("Contact" ?c "\n* %^{Name} :CONTACT:\n%[~/Dropbox/org/contemp.txt]\n"
                                ,(concat org-directory "/gtd/notes.org"))
                               ("Someday" ?s "** %^{Someday Heading} %U\n%?\n"
                                ,(concat org-directory "/gtd/someday.org")))

      org-agenda-custom-commands '(("P" "Projects" ((tags "PROJECT")))
                                   ("H" "Work and Home Lists" ((agenda)
                                                               (tags-todo "WORK")
                                                               (tags-todo "HOME")
                                                               (tags-todo "COMPUTER")
                                                               (tags-todo "DVD")
                                                               (tags-todo "READING")))
                                   ("S" "Consulting and Home Lists" ((agenda)
                                                                     (tags-todo "CONSULTING")
                                                                     (tags-todo "HOME")
                                                                     (tags-todo "COMPUTER")
                                                                     (tags-todo "DVD")
                                                                     (tags-todo "READING")))
        
                                   ("D" "Daily Action List"
                                    ((agenda ""((org-agenda-ndays 1)
                                                (org-agenda-sorting-strategy
                                                 (quote ((agenda time-up priority-down tag-up) )))
                                                (org-deadline-warning-days 0)
                                                )))))

      org-agenda-exporter-settings '((ps-number-of-columns 1)
                                     (ps-landscape-mode t)
                                     (htmlize-output-type 'css))
      org-agenda-include-diary nil
      org-agenda-ndays 7
      org-agenda-repeating-timestamp-show-all nil
      org-agenda-show-all-dates t
      org-agenda-skip-deadline-if-done t
      org-agenda-skip-scheduled-if-done t
      org-agenda-sorting-strategy '((agenda time-up priority-down tag-up) (todo tag-up))
      org-agenda-start-on-weekday nil
      org-agenda-todo-ignore-deadlines t
      org-agenda-todo-ignore-scheduled t
      org-agenda-todo-ignore-with-date t
      org-agenda-window-setup 'other-window
      org-clock-in-switch-to-state "STARTED"
      org-deadline-warning-days 7
      org-fast-tag-selection-single-key nil
      org-insert-mode-line-in-empty-file t
      org-log-done 'time
      org-refile-allow-creating-parent-nodes 'confirm
      org-refile-use-outline-path 'file
      org-reverse-note-order nil
      org-timeline-show-empty-dates t
      org-use-fast-todo-selection t
      org-use-tag-inheritance t
      remember-annotation-functions '(org-remember-annotation)
      remember-handler-functions '(org-remember-handler)
      )

(defun gtd ()
    (interactive)
    (find-file "/home/pib/Dropbox/org/gtd/gtd.org")
)
