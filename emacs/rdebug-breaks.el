;;; rdebug-breaks.el --- Ruby debugger breakpints buffer.

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

;; See the manual and the file `rdebug.el' for more information.

;; This file contains code dealing with the breakpoints secondary buffer.

;;; Code:

(require 'rdebug-dbg)
(require 'rdebug-gud)
(require 'rdebug-regexp)
(require 'rdebug-secondary)
(require 'rdebug-source)
(require 'rdebug-vars)

(defun rdebug-display-breakpoints-buffer ()
  "Display the rdebug breakpoints buffer."
  (interactive)
  (rdebug-display-secondary-buffer "breakpoints"))

(defvar rdebug-breakpoints-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map [double-mouse-1] 'rdebug-goto-breakpoint-mouse)
    (define-key map [mouse-2] 'rdebug-goto-breakpoint-mouse)
    (define-key map [mouse-3] 'rdebug-goto-breakpoint-mouse)
    (define-key map "t" 'rdebug-toggle-breakpoint)
    (define-key map "i" 'rdebug-add-breakpoint-condition)
    (define-key map [insert] 'rdebug-add-breakpoint-condition)
    ;; (define-key map "\C-x\C-e" 'gud-print) ; FIXME show expression in msg area
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

;; Here the "anchored match" method is used, see `font-lock-keywords'
;; for details.
(defvar rdebug-breakpoints-font-lock-keywords
  '(("\\([0-9]+\\) +\\(\\(n\\)\\|\\(y\\)\\) +at "
     (1 font-lock-constant-face)
     (3 font-lock-type-face    nil t)   ; t = ok if not present
     (4 font-lock-warning-face nil t)   ; ditto.
     ;; File name and line
     ("\\(.*\\):\\([0-9]+\\)$"
      nil                               ; Preform (not used)
      nil                               ; Postfrom (not used)
      (1 font-lock-warning-face)
      (2 font-lock-constant-face))
     ;; Class:function
     ("\\(.*\\):\\([a-zA-Z_].+\\)$"
      nil                               ; Preform (not used)
      nil                               ; Postfrom (not used)
      (1 font-lock-type-face)
      (2 font-lock-function-name-face))))
  "Rules for coloring the rdebug breakpoints buffer.")

(defun rdebug-breakpoints-mode ()
  "Major mode for displaying breakpoints in the `rdebug' Ruby debugger.

\\{rdebug-breakpoints-mode-map}"
  (kill-all-local-variables)
  (setq major-mode 'rdebug-breakpoints-mode)
  (setq mode-name "RDEBUG Breakpoints")
  (use-local-map rdebug-breakpoints-mode-map)
  (setq buffer-read-only t)
  (set (make-local-variable 'rdebug-secondary-buffer) t)
  (set (make-local-variable 'font-lock-defaults)
       '(rdebug-breakpoints-font-lock-keywords))
  (run-mode-hooks 'rdebug-breakpoints-mode-hook))


(defun rdebug--setup-breakpoints-buffer (buf comint-buffer)
  "Detects breakpoint lines and sets up keymap and mouse navigation."
  (rdebug-debug-enter "rdebug--setup-breakpoints-buffer"
    (with-current-buffer buf
      (let ((inhibit-read-only t)
	    (old-line-number (buffer-local-value 'rdebug-current-line-number
						 buf)))
        (rdebug-breakpoints-mode)
        (goto-char (point-min))
        (while (not (eobp))
          (let ((b (line-beginning-position)) (e (line-end-position)))
            (when (string-match rdebug--breakpoint-regexp
                                (buffer-substring b e))
              (add-text-properties b e
                                   (list 'mouse-face 'highlight
                                         'keymap rdebug-breakpoints-mode-map)))
            (forward-line)))
	(goto-line old-line-number)))
    (rdebug-breakpoints-parse-and-update-cache)
    (rdebug-breakpoints-update-icons (rdebug-all-breakpoints))))


(defvar rdebug-breakpoints-cache '()
  "The cached return value of `rdebug-all-breakpoints'.

Buffer-local to the debugger shell window.")


;; Implementation note: If Emacs could talk directly to the Ruby
;; debugger, this would be roughly "Debugger.breakpoints". Since we
;; currently can't do that we parse the content of the breakpoints
;; window.
;;
;; Note: The :function kind is not yet implemented.
(defun rdebug-breakpoints-parse-and-update-cache ()
  "Build up the return value of `rdebug-all-breakpoints'."
  (save-excursion
    (goto-char (point-min))
    (let ((res '()))
      (while (not (eobp))
        (when (looking-at rdebug--breakpoint-regexp)
          (push (list :file
                      ;; Break point number
                      (string-to-number (match-string 1))
                      ;; Enabled
                      (string= (match-string 2) "y")
                      ;; File name
                      (file-truename
                       (match-string-no-properties 3))
                      ;; Line number
                      (string-to-number (match-string 4)))
                res))
        (forward-line 1))
      ;; The result goes into a buffer-local variable in the debugger
      ;; shell. (This ensures that this would work in a multi-session
      ;; environment.)
      (if gud-comint-buffer
          (with-current-buffer gud-comint-buffer
            (set (make-local-variable 'rdebug-breakpoints-cache)
                 (nreverse res)))))))


(defun rdebug-all-breakpoints ()
  "Return a list of all breakpoints.

Each entry in the list is on the form:

    (:file number enabled file line)

or

    (:function number enabled class function)"
  (and gud-comint-buffer
       (buffer-local-value 'rdebug-breakpoints-cache gud-comint-buffer)))


;; ---------------------------------------------------------
;; Commands of the rdebug breakpoints buffer.
;;

(defun rdebug-delete-breakpoint (&optional pt)
  "Deletes the breakpoint at PT in the breakpoints buffer."
  (interactive "d")
  (save-excursion
    (if pt
        (goto-char pt))
    (let ((s (buffer-substring (line-beginning-position) (line-end-position))))
      (when (string-match rdebug--breakpoint-regexp s)
        (let ((bpnum (substring s (match-beginning 1) (match-end 1))))
          (gud-call (format "delete %s" bpnum)))))))

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

(defun rdebug-toggle-breakpoint (&optional pt)
  "Toggles the breakpoint at PT in the breakpoints buffer."
  (interactive "d")
  (save-excursion
    (if pt
        (goto-char pt))
    (let ((s (buffer-substring (line-beginning-position) (line-end-position))))
      (when (string-match rdebug--breakpoint-regexp s)
        (let* ((enabled
                (string= (substring s (match-beginning 2) (match-end 2)) "y"))
               (cmd (if enabled "disable" "enable"))
               (bpnum (substring s (match-beginning 1) (match-end 1))))
          (gud-call (format "%s breakpoint %s" cmd bpnum)))))))

(defun rdebug-add-breakpoint-condition (pt)
  "Add an expression as a condition to the break `rdebug' Ruby debugger."
  (interactive "d")
  (let ((bpnum (rdebug-get-breakpoint-number pt))
	(expr (read-string  "Ruby expression for breakpoint condition: ")))
    (if bpnum
	(gud-call (format "condition %s %s" bpnum expr))
      (message "Breakpoint number not found"))))


;; -----------------------------------------------
;; Breakpoint icon support.
;;

;; This is a trivial implementation, it has the following shortcomings:
;;
;; * It assumes that the buffer content doesn't change, if it does it
;;   will not be able to remove the icon.
;;
;; * No support for displaying an icon in a newly opened file.
;;
;; * It has no support for more than one session.

;; Note: This is implemented on top of `gdb-ui'. In the future, it
;; would be better if that code is generalized.

(require 'gdb-ui)

;; This is a local variable, should not be placed in rdebug-vars.el.
(defvar rdebug-breakpoint-icons-current-state nil)

(defun rdebug-breakpoint-remove-icon (entry)
  (if (eq (nth 0 entry) :file)
      (let ((buf (find-buffer-visiting (nth 3 entry))))
        (if buf
            (save-current-buffer
              (set-buffer buf)
              (save-excursion
                (goto-line (nth 4 entry))
                (gdb-remove-breakpoint-icons (point) (point))))))))

(defun rdebug-breakpoint-add-icon (entry)
  (if (eq (nth 0 entry) :file)
      (let ((buf (find-buffer-visiting (nth 3 entry))))
        (if buf
            (save-current-buffer
              (set-buffer buf)
              (save-excursion
                (goto-line (nth 4 entry))
                ;; Workaround for bug in `gdb-ui'. (It checks
                ;; `left-fringe-width' but it doesn't interpret the
                ;; `nil' value correctly.
                (let ((gdb-buffer-fringe-width (car (window-fringes))))
                  (gdb-put-breakpoint-icon (nth 2 entry)
                                           (number-to-string (nth 1 entry))))))))))

(defun rdebug-test-test ()
  (interactive)
  (rdebug-breakpoints-update-icons (rdebug-all-breakpoints)))


(defun rdebug-breakpoint-list-member (file line list)
  (let ((res nil))
    (dolist (entry list)
      (if (and (equal file (nth 3 entry))
               (equal line (nth 4 entry)))
          (setq res t)))
    res))

;; bpts has the same representation as returned by `rdebug-all-breakpoints'.
(defun rdebug-breakpoints-update-icons (bpts)
  ;; Make sure there are is only one reference for each line.
  (let ((state '()))
    ;; An enabled breakpoint take precedence.
    (dolist (enabled '(t nil))
      (dolist (bpt bpts)
        (if (and (eq (nth 0 bpt) :file)
                 (eq (nth 2 bpt) enabled)
                 (not (rdebug-breakpoint-list-member
                       (nth 3 bpt) (nth 4 bpt) state)))
            (setq state (cons bpt state)))))
    (dolist (entry rdebug-breakpoint-icons-current-state)
      (unless (member entry state)
        (rdebug-breakpoint-remove-icon entry)))
    (dolist (entry state)
      (unless (member entry rdebug-breakpoint-icons-current-state)
        (rdebug-breakpoint-add-icon entry)))
    (setq rdebug-breakpoint-icons-current-state state)))

;; -------------------------------------------------------------------
;; The end.
;;

(provide 'rdebug-breaks)

;;; Local variables:
;;; eval:(put 'rdebug-debug-enter 'lisp-indent-hook 1)
;;; End:

;;; rdebug-breaks.el ends here
