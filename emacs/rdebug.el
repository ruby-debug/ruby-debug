;;; rdebug.el --- Ruby debugger user interface, startup file.

;; Copyright (C) 2006, 2007 Rocky Bernstein (rocky@gnu.org)
;; Copyright (C) 2007 Anders Lindgren

;; $Id: rdebug.el 409 2007-12-14 02:36:37Z rockyb $

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

;; This file contains the initialization needed for the Emacs user
;; interface to the `rdebug' Ruby debugger.
;;
;; To use this package, place the following line in an appropriate
;; init file (for example ~/.emacs):
;;
;;    (require 'rdebug)
;;

;;; Code:

(if (< emacs-major-version 22)
    (error
     "This version of rdebug.el needs at least Emacs 22 or greater - you have version %d."
     emacs-major-version))

(autoload 'rdebug "rdebug-core"
  "Run the rdebug Ruby debugger and start the Emacs user interface.

By default, the \"standard\" user window layout looks like the following:

+----------------------------------------------------------------------+
|                                Toolbar                               |
+-----------------------------------+----------------------------------+
| Debugger shell                    | Variables buffer                 |
+-----------------------------------+----------------------------------+
|                                   |                                  |
| Source buffer                     | Output buffer                    |
|                                   |                                  |
+-----------------------------------+----------------------------------+
| Stack buffer                      | Breakpoints buffer               |
+-----------------------------------+----------------------------------+

The variable `rdebug-many-windows-layout-function' can be
customized so that another layout is used. In addition to a
number of predefined layouts it's possible to define a function
to perform a custom layout.

If `rdebug-many-windows' is nil, only a traditional debugger
shell and source window is opened.

The directory containing the debugged script becomes the initial
working directory and source-file directory for your debugger.

The custom variable `gud-rdebug-command-name' sets the command
and options used to invoke rdebug." t)


(autoload 'rdebug-turn-on-debugger-support "rdebug-core"
  "Enable extra source buffer support for the `rdebug' Ruby debugger.

This includes a 'Debugger' menu and special key bindings when the
debugger is active."
 t)


(autoload 'turn-on-rdebugtrack-mode "rdebug-track"
  "Turn on rdebugtrack mode.

This function is designed to be added to hooks, for example:
  (add-hook 'comint-mode-hook 'turn-on-rdebugtrack-mode)"
  t)


(add-hook 'ruby-mode-hook 'rdebug-turn-on-debugger-support)

(provide 'rdebug)

;;; rdebug.el ends here
