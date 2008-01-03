;;; rdebug-cmd.el --- Ruby debugger user commands.

;; Copyright (C) 2007 Rocky Bernstein (rocky@gnu.org)
;; Copyright (C) 2007 Anders Lindgren

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

;; This file contains interactive rdebug Emacs commands (or the
;; routines in support of this) which ultimately send command(s) to
;; the debugger.

;;; Code:

;; -------------------------------------------------------------------
;; Dependencies.
;;

(require 'gud)
(require 'rdebug-vars)
(require 'rdebug-regexp)


;; -------------------------------------------------------------------
;; Rdebug commands.
;;

(defun rdebug-continue (&optional arg)
  "Run a debugger \"continue\" command.

With a numeric argument, continue to that line number of the current file."
  (interactive "p")
  (if arg
      (gud-call (format "continue %d" arg))
    (gud-call (format "continue"))))


(defun rdebug-next (&optional arg)
  "Run a debugger \"next\" command, respecting `rdebug-stepping-prefix'.

With a numeric argument, continue to that line number of the current file."
  (interactive "p")
  (rdebug-stepping "next" arg))

(defvar rdebug-stepping-prefix ""
  "The kind of stepping modifier that is desired.

This variable will have a string value which is either \"\",
\"+\", or \"-\"; this string is be appended to the debugger
stepping commands (\"next\", or \"step\").")

(defun rdebug-quit ()
  "Kill the debugger process associated with the current buffer.

When `rdebug-many-windows' is active, the original window layout
is restored."
  (interactive)
  (if (yes-or-no-p "Really quit? ")
      (gud-call "quit unconditionally")))

(defun rdebug-restart ()
  "Restart the debugged Ruby script.

An exec restart is used."
  (interactive)
  (if (yes-or-no-p "Restart? ")
      (gud-call "restart")))

(defun rdebug-set-stepping-prefix ()
  "Set the granularity of stepping on the subsequent 'next' or 'step' command.
As long as repeated next or step commands are given, they inherit this setting.
"
  (interactive)
  (setq rdebug-stepping-prefix (this-command-keys)))

(defun rdebug-step (&optional arg)
  "Run a debugger \"next\" command, respecting `rdebug-stepping-prefix'.

With a numeric argument, continue to that line number of the current file."
  (interactive "p")
  (rdebug-stepping "step" arg))

(defun rdebug-stepping (step-or-next &optional arg)
  (or arg (setq arg 1))
  ;;(if (not (member '('rdebug-next 'rdebug-step 'digit-argument) last-command))
  ;; (setq rdebug-stepping-prefix ""))
  (unless (member rdebug-stepping-prefix '("" "+" "-"))
    (setq rdebug-stepping-prefix ""))
  (gud-call (format "%s%s %d" step-or-next rdebug-stepping-prefix arg)))


;; -------------------------------------------------------------------
;; Secondary buffer commands.
;;


;; -----------------------------------------------
;; Go directly to an entry using digit keys, used by several secondary
;; buffers.
;;


(defun rdebug-goto-entry-try (str)
  (goto-char (point-min))
  (if (re-search-forward (concat "^[^0-9]*\\(" str "\\)[^0-9]") nil t)
      (progn
        (goto-char (match-end 1))
        t)
    nil))


;; The following is split in two to facilitate debugging.
(defun rdebug-goto-entry-n-internal (keys)
  (if (and (stringp keys)
           (= (length keys) 1))
      (progn
        (setq rdebug-goto-entry-acc (concat rdebug-goto-entry-acc keys))
        ;; Try to find the longest suffix.
        (let ((acc rdebug-goto-entry-acc)
              (p (point)))
          (while (not (string= acc ""))
            (if (not (rdebug-goto-entry-try acc))
                (setq acc (substring acc 1))
              (setq p (point))
              ;; Break loop.
              (setq acc "")))
          (goto-char p)))
    (message "`rdebug-goto-entry-n' must be bound to a number key")))


(defun rdebug-goto-entry-n ()
  "Go to an entry number.

Breakpoints, Display expressions and Stack Frames all have
numbers associated with them which are distinct from line
numbers. In a secondary buffer, this function is usually bound to
a numeric key which will position you at that entry number. To
go to an entry above 9, just keep entering the number. For
example, if you press 1 and then 9, you should jump to entry
1 (if it exists) and then 19 (if that exists). Entering any
non-digit will start entry number from the beginning again."
  (interactive)
  (if (not (eq last-command 'rdebug-goto-entry-n))
      (setq rdebug-goto-entry-acc ""))
  (rdebug-goto-entry-n-internal (this-command-keys)))


;; -----------------------------------------------
;; The breakpoints buffer.
;;

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



;; -----------------------------------------------
;; The stack frame buffer.
;;

;; The following is split in two to facilitate debugging.
(defun rdebug-goto-frame-n-internal (keys)
  (if (and (stringp keys)
           (= (length keys) 1))
      (progn
        (setq rdebug-goto-entry-acc (concat rdebug-goto-entry-acc keys))
        ;; Try to find the longest suffix.
        (let ((acc rdebug-goto-entry-acc))
          (while (not (string= acc ""))
            (if (not (rdebug-goto-entry-try acc))
                (setq acc (substring acc 1))
              (gud-call (format "frame %s" acc))
              ;; Break loop.
              (setq acc "")))))
    (message "`rdebug-goto-frame-n' must be bound to a number key")))

(defun rdebug-goto-frame-n ()
  "Go to the frame number indicated by the accumulated numeric keys just entered.

This function is usually bound to a numeric key in a 'frame'
secondary buffer. To go to an entry above 9, just keep entering
the number. For example, if you press 1 and then 9, frame 1 is selected
\(if it exists) and then frame 19 (if that exists). Entering any
non-digit will start entry number from the beginning again."
  (interactive)
  (if (not (eq last-command 'rdebug-goto-frame-n))
      (setq rdebug-goto-entry-acc ""))
  (rdebug-goto-frame-n-internal (this-command-keys)))


;; -----------------------------------------------
;; The variables buffers.
;;

(defun rdebug-variables-edit-mouse (&optional event)
  "Assign a value to a variable displayed in the variables buffer.
This function is intended to be bound to a mouse key"
  (interactive (list last-input-event))
  (save-excursion
    (if event (posn-set-point (event-end event)))
    (call-interactively 'rdebug-variables-edit)))

(defun rdebug-variables-edit (var value)
  "Assign a value to a variable displayed in the variables buffer."
  (interactive
   (let ((var nil)
         (value nil))
     (save-excursion
       (beginning-of-line)
       (when (looking-at "^\\(@?[a-zA-Z_0-9]+\\) *= *\\(.*\\)$")
         (setq var (match-string 1))
         (setq value (match-string 2))
         (setq value (read-from-minibuffer
                      (format "New value (%s): " var) value)))
       (list var value))))
  (gud-call (format "p %s=%s" var value)))


;; -----------------------------------------------
;; The watch (a.k.a display) buffers.
;;

(defun rdebug-watch-add (expr)
  "Add an expression to watch in the `rdebug' Ruby debugger."
  (interactive "sRuby expression: ")
  (if (not (string= expr ""))
      (gud-call (format "display %s" expr))))

(defun rdebug-watch-delete ()
  "Delete a display expression in the `rdebug' Ruby debugger."
  (interactive)
  (save-excursion
    (beginning-of-line)
    (if (looking-at "^\\([0-9]+\\):")
        (gud-call (format "undisplay %s" (match-string 1))))))

(defun rdebug-watch-edit (number expr)
  "Edit a display expression in the `rdebug' Ruby debugger."
  (interactive
   (let ((number nil)
         (expr nil))
     (save-excursion
       (beginning-of-line)
       (when (looking-at "^\\([0-9]+\\): *\\([^=]*[^= ]\\) *=")
         (setq number (match-string 1))
         (setq expr (match-string 2))
         (setq expr (read-from-minibuffer "Ruby expression: " expr)))
       (list number expr))))
  (when expr
    (gud-call (format "undisplay %s" number))
    (gud-call (format "display %s" expr))))


;; -------------------------------------------------------------------
;; The end.
;;

(provide 'rdebug-cmd)

;;; Local variables:
;;; eval:(put 'rdebug-debug-enter 'lisp-indent-hook 1)
;;; End:

;;; rdebug-cmd.el ends here
