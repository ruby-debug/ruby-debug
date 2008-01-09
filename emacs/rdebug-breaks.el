;;; rdebug-breaks.el --- Ruby debugger frames buffer

;; Copyright (C) 2008 Rocky Bernstein (rocky@gnu.org)
;; Copyright (C) 2008 Anders Lindgren

;; $Id$

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; This file contains code dealing with the breakpoints secondary buffer.


(require 'rdebug-regexp)
(require 'rdebug-vars)
(require 'rdebug-dbg)
(require 'rdebug-secondary)
(require 'rdebug-source)

(defun rdebug-display-breakpoints-buffer ()
  "Display the rdebug breakpoints buffer."
  (interactive)
  (rdebug-display-secondary-buffer "breakpoints"))

(defvar rdebug-breakpoints-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map [mouse-2] 'rdebug-goto-breakpoint-mouse)
    (define-key map [mouse-3] 'rdebug-goto-breakpoint-mouse)
    (define-key map "t" 'rdebug-toggle-breakpoint)
					; (define-key map "i" 'rdebug-add-breakpoint-condition)
    (define-key map "\C-x\C-e" 'gud-print) ; FIXME show expression in msg area
    (rdebug-populate-digit-keys map)
    (define-key map [(control m)] 'rdebug-goto-breakpoint)
    (define-key map [?d] 'rdebug-delete-breakpoint)
    (rdebug-populate-secondary-buffer-map map)

    ;; --------------------
    ;; The "Breakpoints window" submenu.
    (let ((submenu (make-sparse-keymap)))
      (define-key-after map [menu-bar debugger breakpoints]
        (cons "Breakpoints window" submenu)
        'placeholder))

    (define-key map [menu-bar debugger breakpoints toggle]
      '(menu-item "Toggle breakpoint" rdebug-toggle-breakpoint))

    (define-key map [menu-bar debugger breakpoints goto]
      '(menu-item "Goto breakpoint" rdebug-goto-breakpoint))

    (define-key map [menu-bar debugger breakpoints delete]
      '(menu-item "Delete breakpoint" rdebug-delete-breakpoint))

    map)
  "Keymap to navigate/set/enable rdebug breakpoints.")

(defun rdebug-breakpoints-mode ()
  "Major mode for displaying breakpoints in the `rdebug' Ruby debugger.

\\{rdebug-breakpoints-mode-map}"
  (kill-all-local-variables)
  (setq major-mode 'rdebug-breakpoints-mode)
  (setq mode-name "RDEBUG Breakpoints")
  (use-local-map rdebug-breakpoints-mode-map)
  (setq buffer-read-only t)
  (set (make-local-variable 'rdebug-secondary-buffer) t)
  (run-mode-hooks 'rdebug-breakpoints-mode-hook)
  ;;(if (eq (buffer-local-value 'gud-minor-mode gud-comint-buffer) 'gdba)
  ;;    'gdb-invalidate-breakpoints
  ;;  'gdbmi-invalidate-breakpoints)
  )

;; Work in progress...
(defvar rdebug-breakpoints-font-lock-keywords
  '(("\\([0-9]+\\) +[ny] +at +\\(\\(.*\\):\\([0-9]+\\)\\|\\(.*\\):\\([a-zA-Z_][a-zA-Z0-9_]\\)$")
    (1 font-lock-constant-face)
    (2 font-lock-warning-face)))

;;  '(("@[a-zA-Z0-9_]+" 0 font-lock-variable-name-face)
;;    ("\\<\\(nil\\|true\\|false\\)\\>" 0 font-lock-constant-face)
;;    ("#<\\([a-zA-Z0-9_]+\\):\\([0-9a-fx]*\\)"
;;     (1 font-lock-type-face)
;;     (2 font-lock-constant-face)))
;;  "Font-lock rules for the variables and watch windows in `rdebug'.")

(defun rdebug--setup-breakpoints-buffer (buf comint-buffer)
  "Detects breakpoint lines and sets up keymap and mouse navigation."
  (rdebug-debug-enter "rdebug--setup-breakpoints-buffer"
    (with-current-buffer buf
      (let ((inhibit-read-only t) 
	    (old-line-number (buffer-local-value 'rdebug-current-line-number 
						 buf))
	    (flag))
        (rdebug-breakpoints-mode)
        (goto-char (point-min))
        (while (not (eobp))
          (let ((b (line-beginning-position)) (e (line-end-position)))
            (when (string-match rdebug--breakpoint-regexp
                                (buffer-substring b e))
              (add-text-properties b e
                                   (list 'mouse-face 'highlight
                                         'keymap rdebug-breakpoints-mode-map))
              (add-text-properties
               (+ b (match-beginning 1)) (+ b (match-end 1))
               (list 'face font-lock-constant-face
                     'font-lock-face font-lock-constant-face))
              (setq flag (char-after (+ b (match-beginning 2))))
              (add-text-properties
               (+ b (match-beginning 2)) (+ b (match-end 2))
               (if (eq flag ?y)
                   '(face font-lock-warning-face)
                 '(face font-lock-type-face)))
              (add-text-properties
               (+ b (match-beginning 3)) (+ b (match-end 3))
               (list 'face font-lock-comment-face
                     'font-lock-face font-lock-comment-face))
              (add-text-properties
               (+ b (match-beginning 4)) (+ b (match-end 4))
               (list 'face font-lock-constant-face
                     'font-lock-face font-lock-constant-face))
;;;             ;; fontify "keep/del"
;;;             (let ((face (if (string= "keep" (buffer-substring
;;;                                              (+ b (match-beginning 2))
;;;                                              (+ b (match-end 2))))
;;;                             compilation-info-face
;;;                           compilation-warning-face)))
;;;               (add-text-properties
;;;                (+ b (match-beginning 2)) (+ b (match-end 2))
;;;                (list 'face face 'font-lock-face face)))
;;;             ;; fontify "enabled"
;;;             (when (string= "y" (buffer-substring (+ b (match-beginning 3))
;;;                                                  (+ b (match-end 3))))
;;;               (add-text-properties
;;;                (+ b (match-beginning 3)) (+ b (match-end 3))
;;;                (list 'face compilation-error-face
;;;                      'font-lock-face compilation-error-face)))
              )
            (forward-line)
            (beginning-of-line)))
	(goto-line old-line-number)))))

(defun rdebug-delete-breakpoint (pt)
  "Deletes the breakpoint at PT in the breakpoints buffer."
  (interactive "d")
  (save-excursion
    (goto-char pt)
    (let ((s (buffer-substring (line-beginning-position) (line-end-position))))
      (when (string-match rdebug--breakpoint-regexp s)
        (let ((bpnum (substring s (match-beginning 1) (match-end 1))))
          (gud-call (format "delete %s" bpnum)))))))

(defun rdebug-display-line (file line &optional move-arrow)
  (let ((oldpos (and gud-overlay-arrow-position
                     (marker-position gud-overlay-arrow-position)))
        (oldbuf (and gud-overlay-arrow-position
                     (marker-buffer gud-overlay-arrow-position))))
    (gud-display-line file line)
    (unless move-arrow
      (when gud-overlay-arrow-position
        (set-marker gud-overlay-arrow-position oldpos oldbuf)))))


(defun rdebug-goto-breakpoint (pt)
  "Displays the location in a source file of the selected breakpoint."
  (interactive "d")
  (save-excursion
    (goto-char pt)
    (let ((s (buffer-substring (line-beginning-position) (line-end-position))))
      (when (string-match rdebug--breakpoint-regexp s)
        (rdebug-display-line
         (substring s (match-beginning 3) (match-end 3))
         (string-to-number (substring s (match-beginning 4) (match-end 4))))
        ))))


(defun rdebug-goto-breakpoint-mouse (event)
  "Displays the location in a source file of the selected breakpoint."
  (interactive "e")
  (with-current-buffer (window-buffer (posn-window (event-end event)))
    (rdebug-goto-breakpoint (posn-point (event-end event)))))


(defun rdebug-get-breakpoint-number (pt)
  "Return the current breakpoint number in the breakpoint
secondary window or nil if none found."
  (interactive "d")
  (save-excursion
    (goto-char pt)
    (let ((s (buffer-substring (line-beginning-position) (line-end-position))))
      (if (string-match rdebug--breakpoint-regexp s)
	  (substring s (match-beginning 1) (match-end 1))
	nil))))

(defun rdebug-toggle-breakpoint (pt)
  "Toggles the breakpoint at PT in the breakpoints buffer."
  (interactive "d")
  (save-excursion
    (goto-char pt)
    (let ((s (buffer-substring (line-beginning-position) (line-end-position))))
      (when (string-match rdebug--breakpoint-regexp s)
        (let* ((enabled
                (string= (substring s (match-beginning 2) (match-end 2)) "y"))
               (cmd (if enabled "disable" "enable"))
               (bpnum (substring s (match-beginning 1) (match-end 1))))
          (gud-call (format "%s breakpoint %s" cmd bpnum)))))))

;;; (defun rdebug-add-breakpoint-condition (pt expr)
;;;   "Add an expression as a condition to the break `rdebug' Ruby debugger."
;;;   (interactive "dsRuby expression for breakpoint condition: ")
;;;   (let ((bpnum (rdebug-get-breakpoint-number pt)))
;;;     (if (and bpnum (not (string= expr "")))
;;; 	(gud-call (format "condition %s %s" bpnum expr)))))


(provide 'rdebug-breaks)

;;; Local variables:
;;; eval:(put 'rdebug-debug-enter 'lisp-indent-hook 1)
;;; End:

;;; rdebug-breaks.el ends here
