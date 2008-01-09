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


(provide 'rdebug-cmd)

;;; Local variables:
;;; eval:(put 'rdebug-debug-enter 'lisp-indent-hook 1)
;;; End:

;;; rdebug-cmd.el ends here
