;;; rdebug-var.el --- Ruby debugger variables (other than regexps)

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

;;
;; Introduction:
;;
;; This is a full-blown debugger user interface to the Ruby rdebug
;; debugger shell.
;;
;; Internal debug support. When `rdebug-debug-active' is non-nil,
;; internal debug messages are placed in the buffer *Xrdebug*.
;; Functions can be annotated with `rdebug-debug-enter' to display a
;; call trace.
;;

(defvar rdebug-debug-active nil
  "Non-nil when rdebug should emit internal debug output to *Xrdebug*.")

;; Indentation depth of `rdebug-debug-enter'.
(defvar rdebug-debug-depth 0)

(defvar rdebug-debugger-window-configuration nil
  "The saved window layout of the debugger.")

(defvar rdebug-goto-entry-acc "")

(defvar rdebug-output-marker-number 0
  "Number to be used when `rdebug-output-add-divider' is next called on the secondary output buffer.")

(defvar rdebug-original-window-configuration nil
  "The window layout rdebug should restore when the debugger exits.")

;; Terminology: a "secondary buffer" is the physical emacs buffer,
;; which can be visible or invisible. A "secondary window", is a window
;; that rdebug is reusing to display different secondary buffers.
;;
;; For example, the "secondary-window-help" buffer is named the way it
;; is since it gives help on how the secondary window is used.
(defvar rdebug-secondary-buffer nil
  "Non-nil for rdebug secondary buffers (e.g. the breakpoints buffer).")

;; This is used to ensure that the original frame configuration is
;; restored even when the user re-starts the debugger several times.
(defvar rdebug-window-configuration-state 'original
  "Represent the window layout that currently is in use.
Can be `original' or `debugger'.")


(provide 'rdebug-vars)

;;; Local variables:
;;; eval:(put 'rdebug-debug-enter 'lisp-indent-hook 1)
;;; End:

;;; rdebug-vars.el ends here
