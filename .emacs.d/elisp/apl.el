;;; apl.el --- APL input method for Emacs

;; Author: Markus Triska <markus.triska@gmx.at>
;; Keywords: languages

;; Public domain code.

;;; Commentary:

;; Copy apl.el to your load-path and add to your .emacs:

;;     (require 'apl)

;; Enable the APL input method with

;;     M-x set-input-method RET apl-ascii RET

;; Then enter {iota}, {times}, {execute} etc.

;;; Code:

(defconst apl-version "0.9-PRE")

(defvar apl-ascii-codepoint
  '(("{#%}" . ?\x2339)
    ("{#&}" . ?\x233a)
    ("{#/=}" . ?\x236f)
    ("{#/\\}" . ?\x234d)
    ("{#/}" . ?\x2341)
    ("{#:}" . ?\x2360)
    ("{#<}" . ?\x2343)
    ("{#=}" . ?\x2338)
    ("{#>}" . ?\x2344)
    ("{#?}" . ?\x2370)
    ("{#O}" . ?\x233c)
    ("{#\\/}" . ?\x2354)
    ("{#\}" . ?\x2342)
    ("{#^}" . ?\x2353)
    ("{#o}" . ?\x233b)
    ("{#v}" . ?\x234c)
    ("{%}" . ?\x00f7)
    ("{&_}" . ?\x235a)
    ("{&}" . ?\x22c4)
    ("{'_}" . ?\x2358)
    ("{,-}" . ?\x236a)
    ("{->}" . ?\x2192)
    ("{-|}" . ?\x22a3)
    ("{-}" . ?\x2212)
    ("{/=_}" . ?\x2262)
    ("{/=}" . ?\x2260)
    ("{/\\_}" . ?\x2359)
    ("{/\\|}" . ?\x234b)
    ("{/\\}" . ?\x2206)
    ("{/match}" . ?\x2262)
    ("{0~}" . ?\x236c)
    ("{;_}" . ?\x236e)
    ("{<-}" . ?\x2190)
    ("{<=}" . ?\x2264)
    ("{=_}" . ?\x2261)
    ("{>=}" . ?\x2265)
    ("{>\"}" . ?\x2369)
    ("{@}" . ?\x235d)
    ("{I-beam}" . ?\x2336)
    ("{I}" . ?\x2336)
    ("{O*}" . ?\x235f)
    ("{O-}" . ?\x2296)
    ("{O\"}" . ?\x2365)
    ("{O\\}" . ?\x2349)
    ("{O_}" . ?\x235c)
    ("{O}" . ?\x25cb)
    ("{T}" . ?\x22a4)
    ("{\"}" . ?\x00a8)
    ("{\\/~}" . ?\x236b)
    ("{abs}" . ?\x2223)
    ("{alpha-underbar}" . ?\x2376)
    ("{alpha_}" . ?\x2376)
    ("{alpha}" . ?\x237a)
    ("{and-overbar}" . ?\x22bc)
    ("{and}" . ?\x2227)
    ("{assign}" . ?\x2190)
    ("{backslash-bar}" . ?\x2340)
    ("{base}" . ?\x22a5)
    ("{box}" . ?\x2395)
    ("{branch}" . ?\x2192)
    ("{caret}" . ?\x2227)
    ("{cat-bar}" . ?\x236a)
    ("{catenate1}" . ?\x236a)
    ("{catenate}" . ?\x002c)
    ("{ceiling}" . ?\x2308)
    ("{circle-backslash}" . ?\x2349)
    ("{circle-dash}" . ?\x2296)
    ("{circle-diaeresis}" . ?\x2365)
    ("{circle-jot}" . ?\x233e)
    ("{circle-star}" . ?\x235f)
    ("{circle-stile}" . ?\x233d)
    ("{circle-underbar}" . ?\x235c)
    ("{circle_}" . ?\x235c)
    ("{circle}" . ?\x25cb)
    ("{comma-bar}" . ?\x236a)
    ("{comment}" . ?\x235d)
    ("{compress1}" . ?\x233f)
    ("{compress}" . ?\x002f)
    ("{deal}" . ?\x003f)
    ("{decode}" . ?\x22a5)
    ("{del-diaeresis}" . ?\x2362)
    ("{del-stile}" . ?\x2352)
    ("{del-tilde}" . ?\x236b)
    ("{delta-stile}" . ?\x234b)
    ("{delta-underbar}" . ?\x2359)
    ("{delta_}" . ?\x2359)
    ("{delta}" . ?\x2206)
    ("{del}" . ?\x2207)
    ("{depth}" . ?\x2261)
    ("{dex}" . ?\x22a3)
    ("{diaeresis}" . ?\x00a8)
    ("{diamond-underbar}" . ?\x235a)
    ("{diamond_}" . ?\x235a)
    ("{diamond}" . ?\x22c4)
    ("{diaresis-dot}" . ?\x2235)
    ("{disclose}" . ?\x2283)
    ("{divide}" . ?\x00f7)
    ("{domino}" . ?\x2339)
    ("{down-arrow}" . ?\x2193)
    ("{down-caret}" . ?\x2228)
    ("{down-shoe}" . ?\x222a)
    ("{down-tack}" . ?\x22a4)
    ("{down-vane}" . ?\x2356)
    ("{downcaret-tilde}" . ?\x2371)
    ("{downshoe-stile}" . ?\x2366)
    ("{downtack-diaeresis}" . ?\x2361)
    ("{downtack-jot}" . ?\x2355)
    ("{downtack-overbar}" . ?\x2351)
    ("{downwards-vane}" . ?\x2356)
    ("{drop}" . ?\x2193)
    ("{each}" . ?\x00a8)
    ("{enclose}" . ?\x2282)
    ("{encode}" . ?\x22a4)
    ("{enlist}" . ?\x2208)
    ("{epsilon-underbar}" . ?\x2377)
    ("{epsilon_}" . ?\x2377)
    ("{epsilon}" . ?\x2208)
    ("{execute}" . ?\x234e)
    ("{expand1}" . ?\x2340)
    ("{expand}" . ?\x5c)
    ("{exp}" . ?\x22c6)
    ("{find}" . ?\x2377)
    ("{first}" . ?\x2191)
    ("{floor}" . ?\x230a)
    ("{format}" . ?\x2355)
    ("{frog}" . ?\x2362)
    ("{gets}" . ?\x2190)
    ("{goto}" . ?\x2192)
    ("{grade-down}" . ?\x2352)
    ("{grade-up}" . ?\x234b)
    ("{greater-of}" . ?\x2308)
    ("{greater-than-diaeresis}" . ?\x2369)
    ("{greater-than-equal}" . ?\x2265)
    ("{holler}" . ?\x2365)
    ("{hoot}" . ?\x2364)
    ("{index-of}" . ?\x2373)
    ("{index}" . ?\x2337)
    ("{index}" . ?\x2373)
    ("{intersection}" . ?\x2229)
    ("{intersect}" . ?\x2229)
    ("{inverted-caret}" . ?\x2228)
    ("{iota-underbar}" . ?\x2378)
    ("{iota_}" . ?\x2378)
    ("{iota}" . ?\x2373)
    ("{is}" . ?\x2190)
    ("{jot-diaeresis}" . ?\x2364)
    ("{jot-underbar}" . ?\x235b)
    ("{jot_}" . ?\x235b)
    ("{jot}" . ?\x2218)
    ("{laminate1}" . ?\x236a)
    ("{laminate}" . ?\x002c)
    ("{lamp}" . ?\x235d)
    ("{left-arrow}" . ?\x2190)
    ("{left-shoe}" . ?\x2282)
    ("{left-tack}" . ?\x22a3)
    ("{left-vane}" . ?\x2345)
    ("{leftbrace}" . ?\x007b)
    ("{leftshoe-stile}" . ?\x2367)
    ("{leftwards-vane}" . ?\x2345)
    ("{less-than-equal}" . ?\x2264)
    ("{lesser-of}" . ?\x230a)
    ("{lev}" . ?\x22a2)
    ("{ln}" . ?\x235f)
    ("{log}" . ?\x235f)
    ("{mat-divide}" . ?\x2339)
    ("{mat-inverse}" . ?\x2339)
    ("{match}" . ?\x2261)
    ("{max}" . ?\x2308)
    ("{member-of}" . ?\x2208)
    ("{member}" . ?\x2208)
    ("{minus}" . ?\x2212)
    ("{min}" . ?\x230a)
    ("{nabla}" . ?\x2207)
    ("{nand}" . ?\x2372)
    ("{nazg}" . ?\x2218)
    ("{negative}" . ?\x2212)
    ("{nor}" . ?\x2371)
    ("{not-equal}" . ?\x2260)
    ("{not-match}" . ?\x2262)
    ("{not}" . ?\x223c)
    ("{o\"}" . ?\x2364)
    ("{o_}" . ?\x235b)
    ("{omega-underbar}" . ?\x2379)
    ("{omega_}" . ?\x2379)
    ("{omega}" . ?\x2375)
    ("{or-overbar}" . ?\x22bd)
    ("{or}" . ?\x2228)
    ("{o}" . ?\x2218)
    ("{partition}" . ?\x2282)
    ("{paw}" . ?\x2235)
    ("{pi-times}" . ?\x25cb)
    ("{pick}" . ?\x2283)
    ("{pow}" . ?\x22c6)
    ("{quad->}" . ?\x2348)
    ("{quad-backslash}" . ?\x2342)
    ("{quad-circle}" . ?\x233c)
    ("{quad-colon}" . ?\x2360)
    ("{quad-delta}" . ?\x234d)
    ("{quad-del}" . ?\x2354)
    ("{quad-diamond}" . ?\x233a)
    ("{quad-divide}" . ?\x2339)
    ("{quad-down-arrow}" . ?\x2357)
    ("{quad-downcaret}" . ?\x234c)
    ("{quad-equal}" . ?\x2338)
    ("{quad-greater-than}" . ?\x2344)
    ("{quad-jot}" . ?\x233b)
    ("{quad-left-arrow}" . ?\x2347)
    ("{quad-less-than}" . ?\x2343)
    ("{quad-nabla}" . ?\x2354)
    ("{quad-not-equal}" . ?\x236f)
    ("{quad-question}" . ?\x2370)
    ("{quad-right-arrow}" . ?\x2348)
    ("{quad-slash}" . ?\x2341)
    ("{quad-up-arrow}" . ?\x2350)
    ("{quad-up-caret}" . ?\x2353)
    ("{quad<-}" . ?\x2347)
    ("{quad<}" . ?\x2343)
    ("{quad>}" . ?\x2344)
    ("{quad}" . ?\x2395)
    ("{quote-quad}" . ?\x235e)
    ("{quote-underbar}" . ?\x2358)
    ("{quote_}" . ?\x2358)
    ("{rank}" . ?\x2364)
    ("{ravel}" . ?\x2c)
    ("{reciprocal}" . ?\x00f7)
    ("{reduce1}" . ?\x233f)
    ("{reduce}" . ?\x2f)
    ("{replicate1}" . ?\x233f)
    ("{replicate}" . ?\x2f)
    ("{represent}" . ?\x22a4)
    ("{reshape}" . ?\x2374)
    ("{residue}" . ?\x2223)
    ("{reverse}" . ?\x233d)
    ("{rho}" . ?\x2374)
    ("{right-arrow}" . ?\x2192)
    ("{right-shoe}" . ?\x2283)
    ("{right-tack}" . ?\x22a2)
    ("{right-vane}" . ?\x2346)
    ("{rightbrace}" . ?\x007d)
    ("{rightwards-vane}" . ?\x2346)
    ("{ring}" . ?\x2218)
    ("{roll}" . ?\x003f)
    ("{rotate}" . ?\x2296)
    ("{rotate}" . ?\x233d)
    ("{scan1}" . ?\x2340)
    ("{scan}" . ?\x005c)
    ("{semicolon-underbar}" . ?\x236e)
    ("{shape}" . ?\x2374)
    ("{signum}" . ?\x00d7)
    ("{slash-bar}" . ?\x233f)
    ("{slashbar}" . ?\x233f)
    ("{slope-bar}" . ?\x2340)
    ("{smirk}" . ?\x2368)
    ("{snout}" . ?\x2361)
    ("{sourpuss}" . ?\x2363)
    ("{squad}" . ?\x2337)
    ("{squish-quad}" . ?\x2337)
    ("{star-diaeresis}" . ?\x2363)
    ("{star}" . ?\x22c6)
    ("{stile-tilde}" . ?\x236d)
    ("{stile}" . ?\x2223)
    ("{take}" . ?\x2191)
    ("{tilde-diaeresis}" . ?\x2368)
    ("{tilde}" . ?\x223c)
    ("{times}" . ?\x00d7)
    ("{transpose}" . ?\x2349)
    ("{type}" . ?\x2208)
    ("{union}" . ?\x222a)
    ("{up-arrow}" . ?\x2191)
    ("{up-caret}" . ?\x2227)
    ("{up-shoe}" . ?\x2229)
    ("{up-tack}" . ?\x22a5)
    ("{up-vane}" . ?\x234f)
    ("{upcaret-tilde}" . ?\x2372)
    ("{upshoe-jot}" . ?\x235d)
    ("{uptack-jot}" . ?\x234e)
    ("{uptack-underbar}" . ?\x234a)
    ("{upwards-vane}" . ?\x234f)
    ("{w}" . ?\x2375)
    ("{x}" . ?\x00d7)
    ("{zilde}" . ?\x236c)
    ("{|-}" . ?\x22a2)
    ("{|~}" . ?\x236d)
    ("{~\"}" . ?\x2368)
    ("{~^}" . ?\x2372)
    ("{~v}" . ?\x2371)
    ("{~}" . ?\x223c))
  "Correspondence between ASCII transliterations and Unicode codepoints.")

(defvar apl-aplus-pairs
  '((254 . ?\x22c4) (126 . ?\x223c) (161 . ?\xa8) (224 . ?\x2336)
    (162 . ?\xaf) (230 . ?\x236b) (231 . ?\x2352) (164 . ?\x2264)
    (232 . ?\x234b) (166 . ?\x2265) (244 . ?\x2349) (225 . ?\x2296)
    (168 . ?\x2260) (240 . ?\x235f) (169 . ?\x2228) (185 . ?\x2371)
    (94 . ?\x2227) (176 . ?\x2372) (171 . ?\xd7) (223 . ?\xf7)
    (247 . ?\x233d) (173 . ?\x2339) (215 . ?\x2375) (197 . ?\x2208)
    (229 . ?\x2377) (210 . ?\x2374) (217 . ?\x2191) (213 . ?\x2193)
    (201 . ?\x2373) (233 . ?\x2378) (207 . ?\x25cb) (239 . ?\x2365)
    (42 . ?\x22c6) (179 . ?\x235f) (251 . ?\x2190) (253 . ?\x2192)
    (220 . ?\x2340) (252 . ?\x2359) (193 . ?\x237a) (225 . ?\x2296)
    (211 . ?\x2308) (196 . ?\x230a) (189 . ?\x2261) (199 . ?\x2207)
    (231 . ?\x2352) (200 . ?\x2206) (202 . ?\x2218) (234 . ?\x2364)
    (167 . ?\x233c) (204 . ?\x2395) (236 . ?\x235e) (45 . ?\x2212)
    (219 . ?\x22a2) (188 . ?\x2342) (221 . ?\x22a3) (187 . ?\x233c)
    (218 . ?\x2282) (216 . ?\x2283) (195 . ?\x2229) (227 . ?\x235d)
    (214 . ?\x222a) (194 . ?\x22a5) (226 . ?\x234e) (206 . ?\x22a4)
    (238 . ?\x2355) (124 . ?\x2223) (175 . ?\x233f) (205 . ?\x2223))
    "Correspondence between custom A+ encoding and Unicode codepoints.")


(defvar apl-aplus-table
  (mapcar (lambda (pair)
	    (cons (decode-char 'ucs (car pair))
		  (decode-char 'ucs (cdr pair))))
	  apl-aplus-pairs)
  "Translation table mapping A+ characters to Unicode symbols.")

(defun apl-set-font (font)
  "Set font for APL characters. FONT is a font as passed to
set-fontset-font and should provide glyphs for all APL characters
that you want to use. An example is GNU unifont. Try
-gnu-unifont-*-r-*--*-*-*-*-*-*-ISO10646-1. Emacs is becoming
increasingly better at handling Unicode and may choose the right
glyphs for all characters out of the box, sometimes drawing from
various fonts. You therefore probably do not not need this
function at all, except maybe for enforcing a uniform font for
all APL characters."
  (let ((all-symbols (mapcar (lambda (x)
                               (decode-char 'ucs (cdr x)))
                             apl-ascii-codepoint)))
    (dolist (s all-symbols)
      (set-fontset-font "fontset-default" s font))))

(defvar apl-aplus-keyboard nil)

(defun apl-toggle-aplus-keyboard ()
  "Toogle translation of A+ keyboard input to Unicode characters."
  (interactive)
  (setq apl-aplus-keyboard (not apl-aplus-keyboard))
  (dolist (pair apl-aplus-table)
    (keyboard-translate (car pair)
			(if apl-aplus-keyboard
			    (cdr pair)
			  (car pair)))))

(defun apl-aplus-to-unicode (begin end)
  "Convert A+ symbols in region to Unicode characters."
  (interactive "r")
  (translate-region begin end (make-translation-table apl-aplus-table)))

(defun apl-flip (f) (cons (cdr f) (car f)))

(defun apl-unicode-to-aplus (begin end)
  "Convert APL Unicode characters in region to A+."
  (interactive "r")
  (translate-region begin end
		    (make-translation-table (mapcar #'apl-flip
						    apl-aplus-table))))

(defun apl-ascii-to-unicode ()
  "Translate ASCII transliterations to Unicode APL codepoints in the buffer.
In transient mark mode, if the region is active, operate on the region."
  (interactive)
  (save-excursion
    (save-restriction
      (when (and transient-mark-mode mark-active)
        (narrow-to-region (region-beginning) (region-end)))
      (dolist (pair apl-ascii-codepoint)
        (goto-char (point-min))
        (while (search-forward (car pair) nil t)
          (replace-match (string (decode-char 'ucs (cdr pair))) nil t))))))


(when (require 'quail nil t)
  (quail-define-package
   "apl-ascii" "APL" "apl" t
   "APL ASCII input method. Use {rho}, {iota} etc."
   '(("\t" . quail-completion))
   t nil nil nil nil nil nil nil nil t)
  (dolist (pair apl-ascii-codepoint)
    (quail-defrule (car pair) (decode-char 'ucs (cdr pair)))))


(defun apl-version ()
  (interactive)
  (message "Using version %s of apl.el" apl-version))

(provide 'apl)
;;; apl.el ends here