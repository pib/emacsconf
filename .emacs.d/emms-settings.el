(setq load-path (cons  "/home/pib/.emacs.d/emms" load-path))
(require 'emms-player-simple)
(require 'emms-info)
(require 'emms-player-mpd)
(require 'emms-source-file)
(require 'emms-source-playlist)
(setq emms-player-list '(emms-player-mpd))
(add-to-list 'emms-info-functions 'emms-info-mpd)
(add-hook 'emms-player-started-hook 'emms-show)
(add-hook 'emms-player-paused-hook 'emms-show)
;; Stolen and adapted from TWB
(defun my-emms-info-track-description (track)
  "Return a description of the current track."
  (if (and (emms-track-get track 'info-artist)
           (emms-track-get track 'info-title))
      (let ((pmin (emms-track-get track 'info-playing-time-min))
            (psec (emms-track-get track 'info-playing-time-sec))
            (ptot (emms-track-get track 'info-playing-time))
            (art  (emms-track-get track 'info-artist))
            (tit  (emms-track-get track 'info-title)))
        (cond ((and pmin psec) (format "%s - %s [%02d:%02d]" art tit pmin psec))
              (ptot (format  "%s - %s [%02d:%02d]" art tit (/ ptot 60) (% ptot 60)))
              (t (emms-track-simple-description track))))))

(setq emms-track-description-function 'my-emms-info-track-description)
