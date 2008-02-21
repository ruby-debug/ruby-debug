;;; rdebug-cmd.el --- Ruby debugger command buffer

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

;; This file contains code dealing primarily with the command buffer.

;;; Code:

(require 'ring)

(defun rdebug-command-initialization ()
  "Initialization of command buffer common to `rdebug' and
`rdebug-track-attach'."

  ;; This opens up "Gud" menu, which isn't used since we've got our
  ;; own "Debugger" menu.
  ;; (set (make-local-variable 'gud-minor-mode) 'rdebug)

  (set (make-local-variable 'rdebug-call-queue) '())
  (make-local-variable 'rdebug-source-location-ring-size) ; ...to global val.
  (set (make-local-variable 'rdebug-source-location-ring) 
       (make-ring rdebug-source-location-ring-size))
  (set (make-local-variable 'rdebug-source-location-ring-index) 0)

  (gud-def gud-args   "info args" "a"
           "Show arguments of current stack frame.")
  (gud-def gud-break  "break %d%f:%l""\C-b"
           "Set breakpoint at current line.")
  (gud-def gud-cont   "continue"   "\C-r"
           "Continue with display.")
  (gud-def gud-down   "down %p"     ">"
           "Down N stack frames (numeric arg).")
  (gud-def gud-finish "finish"      "\C-f"
           "Finish executing current function.")
  (gud-def gud-source-resync "up 0" "\C-l"
           "Show current source window")
  (gud-def gud-remove "clear %d%f:%l" "\C-d"
           "Remove breakpoint at current line")
  (gud-def gud-quit    "quit"       "Q"
           "Quit debugger.")

  (gud-def gud-statement "eval %e" "\C-e"
           "Execute Ruby statement at point.")
  (gud-def gud-tbreak "tbreak %d%f:%l"  "\C-t"
           "Set temporary breakpoint at current line.")
  (gud-def gud-up     "up %p"
           "<" "Up N stack frames (numeric arg).")
  (gud-def gud-where   "where"
           "T" "Show stack trace.")

  (local-set-key [M-insert] 'rdebug-internal-short-key-mode)
  (local-set-key [M-up] 'rdebug-previous-location)
  (local-set-key [M-down] 'rdebug-next-location)
  (local-set-key "\C-i" 'gud-gdb-complete-command)
  (local-set-key "\C-c\C-n" 'comint-next-prompt)
  (local-set-key "\C-c\C-p" 'comint-previous-prompt))

;; stopping location motion routines.

(defun rdebug-goto-source-location (ring-position)
  "Go the source position `ring-position' in the stopping history."
  (interactive "p")
  (with-current-buffer gud-comint-buffer
    (setq rdebug-source-location-ring-index ring-position)
    (let* ((frame (ring-ref rdebug-source-location-ring ring-position))
	   (file (car frame))
	   (line (cdr frame)))
      (when file
	(rdebug-display-line file line)
	(message (format "%d %s:%d" rdebug-source-location-ring-index 
			 file line))))))
    
(defun rdebug-previous-location ()
  "Cycle backwards through source location stopping history."
  (interactive)
  (with-current-buffer gud-comint-buffer
    (rdebug-goto-source-location 
     (ring-plus1 rdebug-source-location-ring-index 
		 (ring-length rdebug-source-location-ring)))))
    

(defun rdebug-next-location ()
  "Cycle forward through source location stopping history."
  (interactive)
  (with-current-buffer gud-comint-buffer
    (rdebug-goto-source-location 
     (ring-minus1 rdebug-source-location-ring-index 
		  (ring-length rdebug-source-location-ring)))))

(provide 'rdebug-cmd)

;;; Local variables:
;;; eval:(put 'rdebug-debug-enter 'lisp-indent-hook 1)
;;; End:

;;; rdebug-cmd.el ends here
