;;; rdebug-core.el --- Ruby debugger user interface.

;; Copyright (C) 2006, 2007 Rocky Bernstein (rocky@gnu.org)
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
;; The main features are:
;;
;;  * Window layout with dedicted windows for:
;;      + Local and member variables
;;      + Stack trace
;;      + Watch expressions
;;      + Breakpoints
;;      + Output
;;      + Debugger Shell
;;
;;  * Source-level debugging:
;;      + The current source file is shown and current line is marked.
;;      + Function keys bindings for effective stepping in the source code.
;;      + A "Debugger" menu for easy access to all features.
;;
;;  * A number of predefined window layouts and key bindings are
;;    supplied, including binding that emulate Eclipse and NetBeans.
;;    The user can easily provide their own window layout and
;;    settings.
;;
;;  * `rdebugtrack-mode' allows access to full debugger user interface
;;    for Ruby deugger sesstions started in a standard shell window.
;;

;;
;; Installation:
;;
;; To use this package, place the following line in an appropriate
;; init file (for example ~/.emacs):
;;
;;    (require 'rdebug)
;;

;;
;; History and Future:
;;
;; The design of this debugger user interface was inspired by
;; `gdb-ui', a similar user interface to GDB.
;;
;; Hopefully, rdebug, gdb-ui, and other emacs user interfaces could
;; join forces to create a common user-level look and feel, and a
;; battery of underlying support functions.
;;

;;; Code:

(if (< emacs-major-version 22)
    (error
     "This version of rdebug.el needs at least Emacs 22 or greater - you have version %d."
     emacs-major-version))
(require 'gud)


;; user definable variables
;; vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv

(defcustom gud-rdebug-command-name
  "rdebug --emacs --no-control --no-quit --post-mortem --annotate=3"
  "File name for executing the Ruby debugger.
This should be an executable on your path, or an absolute file name."
  :type 'string
  :group 'gud)

(defcustom rdebug-many-windows t
  "*If non-nil, use the full debugger user interface, see `rdebug'.

However only set to the multi-window display if the rdebug
command invocation has an annotate options (\"--annotate 3\")."
  :type 'boolean
  :group 'rdebug)

(defcustom rdebug-window-layout-function
  'rdebug-window-layout-standard
  "*A function that performs the window layout of `rdebug'.

This is only used in `rdebug-many-windows' mode. This should be
bound to a function that performs the actual window layout. The
function should takes two arguments, the first is the source
buffer and the second the name of the script to debug.

Rdebug provides the following predefined layout functions:

* `rdebug-window-layout-standard'         -- See `rdebug'

* `rdebug-window-layout-conservative'     -- Source + Shell + Output

* `rdebug-window-layout-stack-of-windows' -- Extra windows to the right

* `rdebug-window-layout-rocky'            -- Rocky's own layout"
  :type
  '(choice
    (function :tag "Standard"         rdebug-window-layout-standard)
    (function :tag "Conservative"     rdebug-window-layout-conservative)
    (function :tag "Stack of windows" rdebug-window-layout-stack-of-windows)
    (function :tag "Rocky's own"      rdebug-window-layout-rocky)
    (function :tag "Other"            function))
  :group 'rdebug)


(defcustom rdebug-populate-common-keys-function
  'rdebug-populate-common-keys-standard
  "The function to call to populate key bindings common to all rdebug windows.
This includes the secondary windows, the debugger shell, and all
Ruby source buffers when the debugger is active.

This variable can be bound to the following:

* nil -- Don't bind any keys.

* `rdebug-populate-common-keys-standard' -- Bind according to a videly used
  debugger covention:

\\{rdebug-example-map-standard}

* `rdebug-populate-common-keys-eclipse' -- Bind according to Eclipse.

\\{rdebug-example-map-eclipse}

* `rdebug-populate-common-keys-netbeans' -- Bind according to NetBeans.

\\{rdebug-example-map-netbeans}

* Any other value is expected to be a callable function that takes one
  argument, the keymap, and populates it with suitable keys."
  :type 'function
  :group 'rdebug)



(defcustom rdebug-restore-original-window-configuration :many
  "*Control if the original window layout is restored when the debugger exits.
The value can be t, nil, or :many.

A value of t means that the original layout is always restored,
nil means that it's never restored.

:many means that the original layout is restored only when
`rdebug-many-windows' is used."
  :type '(choice (const :tag "Always restore" t)
		 (const :tag "Never restore" nil)
		 (const :tag "Restore in many windows mode" :many))
  :group 'rdebug)

(defgroup rdebugtrack nil
  "Rdebug file tracking by watching the prompt."
  :prefix "rdebugtrack"
  :group 'shell)

(defcustom rdebugtrack-do-tracking-p nil
  "*Controls whether the rdebugtrack feature is enabled or not.
When non-nil, rdebugtrack is enabled in all comint-based buffers,
e.g. shell buffers and the *Ruby* buffer.  When using rdebug to debug a
Ruby program, rdebugtrack notices the rdebug prompt and displays the
source file and line that the program is stopped at, much the same way
as gud-mode does for debugging C programs with gdb."
  :type 'boolean
  :group 'rdebug)
(make-variable-buffer-local 'rdebugtrack-do-tracking-p)

(defcustom rdebugtrack-minor-mode-string " rdebug"
  "*String to use in the minor mode list when rdebugtrack is enabled."
  :type 'string
  :group 'rdebug)


;; ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
;; NO USER DEFINABLE VARIABLES BEYOND THIS POINT

(defvar gud-rdebug-history nil
  "History of argument lists passed to rdebug.")

(defconst gud-rdebug-marker-regexp
  "^\\([^:\n]*\\):\\([0-9]*\\).*\n"
  "Regular expression used to find a file location given by rdebug.

Program-location lines look like this:
   /tmp/gcd.rb:29:  gcd
   /tmp/gcd.rb:29
   \\sources\\capfilterscanner\\capanalyzer.rb:3:  <module>
")

(defconst rdebug-marker-regexp-file-group 2
  "Group position in rdebug-position-re that matches the file name.")

(defconst rdebug-marker-regexp-line-group 3
  "Group position in rdebug-position-re that matches the line number.")

(defconst rdebug-annotation-start-regexp
  "\\([a-z]+\\)\n"
  "Start of an annotation. Note that in contrast to
gud-rdebug-marker-regexp, we don't allow a colon. That's what
distinguishes the two." )
(defconst rdebug-annotation-end-regexp
  "\n")

(defvar rdebug-original-window-configuration nil
  "The window layout rdebug should restore when the debugger exits.")

(defvar rdebug-debugger-window-configuration nil
  "The saved window layout of the debugger.")

;; This is used to ensure that the original frame configuration is
;; restored even when the user re-starts the debugger several times.
(defvar rdebug-window-configuration-state 'original
  "Represent which frame configuration  currently is in use.
Can be `original' or `debugger'.")

;; Terminology: a "secondary buffer" is the physical emacs buffer,
;; which can be visible or invisible. A "secondary window", is a window
;; that rdebug is reusing to display different secondary buffers.
;;
;; For example, the "secondary-window-help" buffer is named the way it
;; is since it gives help on how the secondary window is used.
(defvar rdebug-secondary-buffer nil
  "Non-nil for rdebug secondary buffers (e.g. the breakpoints buffer).")

;; rdebugtrack constants
(defconst rdebugtrack-stack-entry-regexp
  "^(\\([-a-zA-Z0-9_/.]*\\):\\([0-9]+\\)):[ \t]?\\(.*\n\\)"
  "Regular expression rdebugtrack uses to find a stack trace entry.")

(defconst rdebugtrack-input-prompt "\n(+rdb:\\([0-9]+\\|post-mortem\\))+ *"
  "Regular expression rdebugtrack uses to recognize a rdebug prompt.")

(defconst rdebugtrack-track-range 10000
  "Max number of characters from end of buffer to search for stack entry.")

;;
;; Internal debug support. When `rdebug-debug-active' is non-nil,
;; internal debug messages are placed in the buffer *Xrdebug*.
;; Functions can be annotated with `rdebug-debug-enter' to display a
;; call trace.
;;

(defvar rdebug-debug-active nil
  "Non-nil when rdebug should emit internal debug output to *Xrdebug*.")


;; Identation depth of `rdebug-debug-enter'.
(defvar rdebug-debug-depth 0)

(defun rdebug-debug-message (&rest args)
  (if rdebug-debug-active
      (let ((buf (get-buffer-create "*Xrdebug*")))
        (with-current-buffer buf
          (save-excursion
            (goto-char (point-max))
            ;; 32 = space.
            (insert (make-string (* 4 rdebug-debug-depth) 32))
            (insert (apply #'format args))
            (insert "\n"))))))


(defmacro rdebug-debug-enter (str &rest body)
  (declare (indent 1) (debug t))
  `(progn
     (rdebug-debug-message "--> %s" ,str)
     (setq rdebug-debug-depth (+ rdebug-debug-depth 1))
     (unwind-protect
         (progn
           ,@body)
       (setq rdebug-debug-depth (max 0 (- rdebug-debug-depth 1)))
       (rdebug-debug-message "<-- %s" ,str))))


;; Interface to gud.

(defun gud-rdebug-massage-args (file args)
  args)


;; There's no guarantee that Emacs will hand the filter the entire
;; marker at once; it could be broken up across several strings.  We
;; might even receive a big chunk with several markers in it.  If we
;; receive a chunk of text which looks like it might contain the
;; beginning of a marker, we save it here between calls to the
;; filter.
(defun gud-rdebug-marker-filter (string)
  (rdebug-debug-enter "gud-rdebug-marker-filter"
    (rdebug-debug-message "GOT: %S" string)
    (setq gud-marker-acc (concat gud-marker-acc string))
    (let ((output "") s s2 (tmp ""))

      ;; ALB first we process the annotations (if any)
      (while (setq s (string-match rdebug-annotation-start-regexp
				   gud-marker-acc))
	(rdebug-debug-message "ACC: %S" gud-marker-acc)
	(let ((name (substring gud-marker-acc (match-beginning 1) (match-end 1)))
	      (end (match-end 0)))
	  (if (setq s2 (string-match rdebug-annotation-end-regexp
				     gud-marker-acc end))
	      ;; ok, annotation complete, process it and remove it
	      (let ((contents (substring gud-marker-acc end s2))
		    (end2 (match-end 0)))
		(rdebug-process-annotation name contents)
		(setq gud-marker-acc
		      (concat (substring gud-marker-acc 0 s)
			      (substring gud-marker-acc end2))))
	    ;; otherwise, save the partial annotation to a temporary, and re-add
	    ;; it to gud-marker-acc after normal output has been processed
	    (setq tmp (substring gud-marker-acc s))
	    (setq gud-marker-acc (substring gud-marker-acc 0 s)))))

      (when (setq s (string-match rdebug-annotation-end-regexp gud-marker-acc))
	;; save the beginning of gud-marker-acc to tmp, remove it and restore it
	;; after normal output has been processed
	(setq tmp (substring gud-marker-acc 0 s))
	(setq gud-marker-acc (substring gud-marker-acc s)))

      ;; Process all the complete markers in this chunk.
      ;; Format of line looks like this:
      ;;   /etc/init.d/ntp.init:16:
      (while (string-match gud-rdebug-marker-regexp gud-marker-acc)
	(setq

	 ;; Extract the frame position from the marker.
	 gud-last-frame
	 (cons (substring gud-marker-acc
			  (match-beginning 1) (match-end 1))
	       (string-to-number
		(substring gud-marker-acc
			   (match-beginning 2) (match-end 2))))

	 ;; Append any text before the marker to the output we're going
	 ;; to return - we don't include the marker in this text.
	 output (concat output
			(substring gud-marker-acc 0 (match-beginning 0)))

	 ;; Set the accumulator to the remaining text.
	 gud-marker-acc (substring gud-marker-acc (match-end 0))))

      ;; Does the remaining text look like it might end with the
      ;; beginning of another marker?  If it does, then keep it in
      ;; gud-marker-acc until we receive the rest of it.  Since we
      ;; know the full marker regexp above failed, it's pretty simple to
      ;; test for marker starts.
      (if (string-match "\032\032.*\\'" gud-marker-acc)
	  (progn
	    ;; Everything before the potential marker start can be output.
	    (setq output (concat output (substring gud-marker-acc
						   0 (match-beginning 0))))

	    ;; Everything after, we save, to combine with later input.
	    (setq gud-marker-acc
		  (concat tmp (substring gud-marker-acc (match-beginning 0)))))
	(setq output (concat output gud-marker-acc)
	      gud-marker-acc tmp))
      output)))

(defun gud-rdebug-find-file (f)
  (find-file-noselect f))

(defun rdebug-get-script-name (args &optional annotate-p)
  "Pick out the script name from the command line and return a
list of that and whether the annotate option was set. Initially
annotate should be set to nil."
  (let ((arg (pop args)))
    (cond
     ((not arg) (list nil annotate-p))
     ((string-match "^--annotate=[1-9]" arg)
      (rdebug-get-script-name args t))
     ((equal "--annotate" arg)
      (rdebug-get-script-name (cdr args) t))
     ((equal "-A" arg)
      (rdebug-get-script-name (cdr args) t))
     ((member arg '("-h" "--host" "-p" "--port"
		    "-I", "--include" "-r" "--require"))
      (if args
          (rdebug-get-script-name (cdr args) annotate-p)
        ;;else
        (list nil annotate-p)))
     ((string-match "^-[a-zA-z]" arg) (rdebug-get-script-name args annotate-p))
     ((string-match "^--[a-zA-z]+" arg) (rdebug-get-script-name args annotate-p))
     ((string-match "^rdebug" arg) (rdebug-get-script-name args annotate-p))
     ;; found script name (or nil
     (t (list arg annotate-p)))))

;; From Emacs 23
(unless (fboundp 'split-string-and-unquote)
  (defun split-string-and-unquote (string &optional separator)
    "Split the STRING into a list of strings.
It understands Emacs Lisp quoting within STRING, such that
  (split-string-and-unquote (combine-and-quote-strings strs)) == strs
The SEPARATOR regexp defaults to \"\\s-+\"."
    (let ((sep (or separator "\\s-+"))
          (i (string-match "[\"]" string)))
      (if (null i)
          (split-string string sep t)	; no quoting:  easy
        (append (unless (eq i 0) (split-string (substring string 0 i) sep t))
                (let ((rfs (read-from-string string i)))
                  (cons (car rfs)
                        (split-string-and-unquote (substring string (cdr rfs))
                                                  sep)))))))
  )


(defun rdebug-set-window-configuration-state (state &optional dont-restore)
  "Change window configuration state.

Two states are supported `original' and `debugger'.

When `dont-restore' is non-nil, the old window layout is not
restored. This is used when a new layout is being drawn, for
example when the debugger starts."
  (rdebug-debug-message "Setting state to %s (was %s)"
                        state rdebug-window-configuration-state)
  (when (not (eq state rdebug-window-configuration-state))
    ;; Save the previous state.
    (cond ((not (eq rdebug-window-configuration-state 'original))
           (setq rdebug-debugger-window-configuration
                 (current-window-configuration)))
          ((eq rdebug-window-configuration-state 'original)
           (setq rdebug-original-window-configuration
                 (current-window-configuration))))
    (unless dont-restore
      ;; Switch to the saved state,
      (cond
       ((not (eq state 'original))
        (if rdebug-debugger-window-configuration
            (set-window-configuration rdebug-debugger-window-configuration)))
       ((eq state 'original)
        (if rdebug-original-window-configuration
            (set-window-configuration rdebug-original-window-configuration)))))
    (setq rdebug-window-configuration-state state)))


;; -- Common key support.

(defun rdebug-populate-common-keys-standard (map)
  "Bind the basic debugger key layout used by many debuggers:

\\{rdebug-example-map-standard}"
  (define-key map [f5]    'gud-cont)
  (define-key map [S-f5]  'rdebug-quit)
  (define-key map [f9]    'gud-break)   ; TODO: Should be "toggle"
  (define-key map [f10]   'gud-next)
  (define-key map [f11]   'gud-step)
  (define-key map [S-f11] 'gud-finish))


;; TODO: Verify and complement.
(defun rdebug-populate-common-keys-eclipse (map)
  "Bind the basic debugger key layout used by Eclipse:

\\{rdebug-example-map-eclipse}"
  ;;(define-key map []  'gud-cont)
  ;;(define-key map []  'rdebug-quit)
  (define-key map [S-C-b]    'gud-break) ; TODO: Should be "toggle"
  (define-key map [f6]   'gud-next)
  (define-key map [f5]   'gud-step)
  (define-key map [f7] 'gud-finish))


;; TODO: Verify and complement.
(defun rdebug-populate-common-keys-netbeans (map)
  "Bind the basic debugger key layout used by NetBeans:

\\{rdebug-example-map-netbeans}"
  ;;(define-key map []  'gud-cont)
  ;;(define-key map []  'rdebug-quit)
  ;; F4 - Run to cursor.
  (define-key map [S-f8]   'gud-break)  ; TODO: Should be "toggle"
  (define-key map [f8]     'gud-next)
  (define-key map [f7]     'gud-step)
  (define-key map [M-S-f7] 'gud-finish))


;; Note: This is only used in doc-strings.
(defvar rdebug-example-map-standard
  (let ((map (make-sparse-keymap)))
    (rdebug-populate-common-keys-standard map)
    map))

(defvar rdebug-example-map-eclipse
  (let ((map (make-sparse-keymap)))
    (rdebug-populate-common-keys-eclipse map)
    map))

(defvar rdebug-example-map-netbeans
  (let ((map (make-sparse-keymap)))
    (rdebug-populate-common-keys-netbeans map)
    map))


(defun rdebug-populate-common-keys (map)
  "Define the keys that are used by all debugger windows, even by the source.

The variable `rdebug-populate-common-keys-function' controls the layout."
  (if rdebug-populate-common-keys-function
      (funcall rdebug-populate-common-keys-function map)))


;; -- The debugger

;;;###autoload
(defun rdebug (command-line)
  "Run the rdebug Ruby debugger and start the Emacs user interface.

By default, the user interface looks like the following:

+----------------------------------------------------------------------+
|                                Toolbar                               |
+-----------------------------------+----------------------------------+
| Debugger shell                    | Variables buffer                 |
|                                   | RET rdebug-variables-edit        |
|                                   |                                  |
|                                   |                                  |
+-----------------------------------+----------------------------------+
|                                   |                                  |
| Source buffer                     | Output buffer                    |
|                                   |                                  |
+-----------------------------------+----------------------------------+
| Stack buffer                      | Breakpoints buffer               |
| RET rdebug-goto-stack-frame       | t    rdebug-toggle-breakpoint    |
|                                   | RET  rdebug-goto-breakpoint      |
|                                   | DEL  rdebug-delete-breakpoint    |
+-----------------------------------+----------------------------------+

The variable `rdebug-window-layout-function' can be
customized so that another layout is used. In addition to a
number of predefined layouts it's possible to define a function
to perform a custom layout.

If `rdebug-many-windows' is nil, only a traditional debugger
shell and source window is opened.

The directory containing the debugged script becomes the initial
working directory and source-file directory for your debugger.

The custom variable `gud-rdebug-command-name' sets the command
and options used to invoke rdebug."
  (interactive
   (list (gud-query-cmdline 'rdebug)))

  (rdebug-debug-enter "rdebug"
    (rdebug-set-window-configuration-state 'debugger t)
    ;; Parse the command line and pick out the script name and whether
    ;; --annotate has been set.
    (let* ((words (split-string-and-unquote command-line))
           (script-name-annotate-p (rdebug-get-script-name
                                    (gud-rdebug-massage-args "1" words) nil))
           (target-name (file-name-nondirectory (car script-name-annotate-p)))
           (annotate-p (cadr script-name-annotate-p))
           (rdebug-buffer-name (format "*rdebug-cmd-%s*" target-name))
           (rdebug-buffer (get-buffer rdebug-buffer-name))
           )

      ;; `gud-rdebug-massage-args' needs whole `command-line'.
      ;; command-line is refered through dyanmic scope.
      (gud-common-init command-line 'gud-rdebug-massage-args
                       'gud-rdebug-marker-filter 'gud-rdebug-find-file)

      ;; gud-common-init sets the rdebug process buffer name
      ;; incorrectly, because it can't parse the command line properly
      ;; to pick out the script name. So we'll do it here and rename
      ;; that buffer. The buffer we want to rename happens to be the
      ;; current buffer.
      (setq gud-target-name target-name)
      (when rdebug-buffer (kill-buffer rdebug-buffer))
      (rename-buffer rdebug-buffer-name)

      ;; Setup exit callback so that the original frame configuration
      ;; can be restored.
      (let ((process (get-buffer-process gud-comint-buffer)))
        (if process
            (set-process-sentinel process
                                  'rdebug-process-sentinel)))

      ;; This opens up "Gud" menu, which isn't used since we've got our
      ;; own "Debugger" menu.

      ;; (set (make-local-variable 'gud-minor-mode) 'rdebug)

      (gud-def gud-args   "info args" "a"
               "Show arguments of current stack.")
      (gud-def gud-break  "break %d%f:%l""\C-b"
               "Set breakpoint at current line.")
      (gud-def gud-cont   "continue"   "\C-r"
               "Continue with display.")
      (gud-def gud-down   "down %p"     ">"
               "Down N stack frames (numeric arg).")
      (gud-def gud-finish "finish"      "\C-f"
               "Finish executing current function.")
      (gud-def gud-next   "next %p"     "\C-n"
               "Step one line (skip functions).")
      (gud-def gud-print  "p %e"        "\C-p"
               "Evaluate Ruby expression at point.")
      (gud-def gud-source-resync "up 0" "\C-l"
               "Show current source window")
      (gud-def gud-remove "clear %d%f:%l" "\C-d"
               "Remove breakpoint at current line")
      (gud-def gud-quit    "quit"       "Q"
               "Quit debugger.")
      (gud-def gud-run    "restart"       "R"
               "Restart the Ruby script.")
      (gud-def gud-statement "eval %e" "\C-e"
               "Execute Ruby statement at point.")
      (gud-def gud-step   "step %p"       "\C-s"
               "Step one source line with display.")
      (gud-def gud-step-plus "step+ %p"       "+"
               "Step one source line with display.")
      (gud-def gud-tbreak "tbreak %d%f:%l"  "\C-t"
               "Set temporary breakpoint at current line.")
      (gud-def gud-up     "up %p"
               "<" "Up N stack frames (numeric arg).")
      (gud-def gud-where   "where"
               "T" "Show stack trace.")
      (local-set-key "\C-i" 'gud-gdb-complete-command)

      ;; Add the buffer-displaying commands to the Gud buffer,
      ;; accessible using the C-c prefix.
      (rdebug-populate-secondary-buffer-map
       (lookup-key (current-local-map) "\C-c")
       t)

      (rdebug-populate-debugger-menu (current-local-map))
      (rdebug-populate-common-keys (current-local-map))

      ;; Update GUD menu bar
      (define-key gud-menu-map [args]   '("Show arguments of current stack" .
                                          gud-args))
      (define-key gud-menu-map [down]   '("Down Stack" . gud-down))
      (define-key gud-menu-map [eval]   '("Execute Ruby statement at point"
                                          . gud-statement))
      (define-key gud-menu-map [finish] '("Finish Function" . gud-finish))
      (define-key gud-menu-map [run]    '("Restart the Ruby Script" .
                                          gud-run))
      (define-key gud-menu-map [stepi]  'undefined)
      (define-key gud-menu-map [tbreak] '("Temporary break" . gud-tbreak))
      (define-key gud-menu-map [up]     '("Up Stack" . gud-up))
      (define-key gud-menu-map [where]  '("Show stack trace" . gud-where))

      ;;    (local-set-key [menu-bar debug finish] '("Finish Function" . gud-finish))
      ;;    (local-set-key [menu-bar debug up] '("Up Stack" . gud-up))
      ;;    (local-set-key [menu-bar debug down] '("Down Stack" . gud-down))

      (setq comint-prompt-regexp "^(rdb:-) ")
      (setq paragraph-start comint-prompt-regexp)

      (setcdr (assq 'rdebug-debugger-support-minor-mode minor-mode-map-alist)
              rdebug-debugger-support-minor-mode-map-when-active)
      (when rdebug-many-windows (rdebug-setup-windows-initially))

      (run-hooks 'rdebug-mode-hook))))


(defun rdebug-quit ()
  "Kill current debugger process.

When `rdebug-many-windows' is active, restores the original
window layout."
  (interactive)
  (if (yes-or-no-p "Really quit? ")
      (gud-call "quit unconditionally")))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; rdebugtrack --- tracking rdebug debugger in an Emacs shell window
;;; Modified from  python-mode in particular the part:
;; pdbtrack support contributed by Ken Manheimer, April 2001.

(require 'comint)
(require 'custom)
(require 'cl)
(require 'compile)
(require 'shell)

;; have to bind rdebug-file-queue before installing the kill-emacs-hook
(defvar rdebug-file-queue nil
  "Queue of Makefile temp files awaiting execution.
Currently-active file is at the head of the list.")

(defvar rdebugtrack-is-tracking-p t)


;; Constants

(defconst rdebug-position-re
  "\\(\\)\\([-a-zA-Z0-9_/.]*\\):\\([0-9]+\\)"
  "Regular expression for a rdebug position")

(defconst rdebug-traceback-line-re
  "^[ \t]+from \\([^:]+\\):\\([0-9]+\\)\\(in `.*'\\)?"
  "Regular expression that describes a Ruby traceback line.")

(defconst rdebug-dollarbang-traceback-line-re
  "^[ \t]+[[]?\\([^:]+\\):\\([0-9]+\\):in `.*'"
  "Regular expression that describes a Ruby traceback line from $! list.")

(defun rdebugtrack-overlay-arrow (activation)
  "Activate or de arrow at beginning-of-line in current buffer."
  ;; This was derived/simplified from edebug-overlay-arrow
  (cond (activation
	 (setq overlay-arrow-position (make-marker))
	 (setq overlay-arrow-string "=>")
	 (set-marker overlay-arrow-position (point) (current-buffer))
	 (setq rdebugtrack-is-tracking-p t))
	(rdebugtrack-is-tracking-p
	 (setq overlay-arrow-position nil)
	 (setq rdebugtrack-is-tracking-p nil))
	))

(defun rdebugtrack-track-stack-file (text)
  "Show the file indicated by the rdebug stack entry line, in a separate window.
Activity is disabled if the buffer-local variable
`rdebugtrack-do-tracking-p' is nil.

We depend on the rdebug input prompt matching `rdebugtrack-input-prompt'
at the beginning of the line."
  ;; Instead of trying to piece things together from partial text
  ;; (which can be almost useless depending on Emacs version), we
  ;; monitor to the point where we have the next rdebug prompt, and then
  ;; check all text from comint-last-input-end to process-mark.
  ;;
  ;; Also, we're very conservative about clearing the overlay arrow,
  ;; to minimize residue.  This means, for instance, that executing
  ;; other rdebug commands wipe out the highlight.  You can always do a
  ;; 'where' (aka 'w') command to reveal the overlay arrow.
  (rdebug-debug-enter "rdebugtrack-track-stack-file"
    (let* ((origbuf (current-buffer))
           (currproc (get-buffer-process origbuf)))

      (if (not (and currproc rdebugtrack-do-tracking-p))
          (rdebugtrack-overlay-arrow nil)
        ;;else
        (let* ((procmark (process-mark currproc))
               (block-start (max comint-last-input-end
                                 (- procmark rdebugtrack-track-range)))
               (block-str (buffer-substring block-start procmark))
               target target_fname target_lineno target_buffer)

          (if (not (string-match rdebugtrack-input-prompt block-str))
              (rdebugtrack-overlay-arrow nil)
            ;;else
            (setq target (rdebugtrack-get-source-buffer block-str))

            (if (stringp target)
                (rdebug-debug-message "rdebugtrack: %s" target)
              ;;else
              (gud-rdebug-marker-filter block-str)
              (setq target_lineno (car target))
              (setq target_buffer (cadr target))
              (setq target_fname (buffer-file-name target_buffer))
              (switch-to-buffer-other-window target_buffer)
              (goto-line target_lineno)
              (rdebug-debug-message "rdebugtrack: line %s, file %s"
                                    target_lineno target_fname)
              (rdebugtrack-overlay-arrow t)
              (pop-to-buffer origbuf t)
              )

            ;; Delete processed annotations from buffer.
            (save-excursion
              (let ((annotate-start)
                    (annotate-end (point-max)))
                (goto-char block-start)
                (while (re-search-forward
                        rdebug-annotation-start-regexp annotate-end t)
                  (setq annotate-start (match-beginning 0))
                  (if (re-search-forward
                       rdebug-annotation-end-regexp annotate-end t)
                      (delete-region annotate-start (point))
                    ;;else
                    (forward-line)))))))))))

(defun rdebugtrack-get-source-buffer (block-str)
  "Return line number and buffer of code indicated by block-str's traceback
text.

We look first to visit the file indicated in the trace.

Failing that, we look for the most recently visited ruby-mode buffer
with the same name or having having the named function.

If we're unable find the source code we return a string describing the
problem as best as we can determine."

  (if (not (string-match rdebug-position-re block-str))
      "line number cue not found"
    ;;else
    (let* ((filename (match-string rdebug-marker-regexp-file-group block-str))
           (lineno (string-to-number
		    (match-string rdebug-marker-regexp-line-group block-str)))
           funcbuffer)

      (cond ((file-exists-p filename)
             (list lineno (find-file-noselect filename)))

            ((= (elt filename 0) ?\<)
             (format "(Non-file source: '%s')" filename))

            (t (format "Not found: %s" filename))))))


;; rdebugtrack
(defcustom rdebugtrack-mode-text " rdebug"
  "*String to display in the mode line when rdebugtrack mode is active.

\(When the string is not empty, make sure that it has a leading space.)"
  :tag "rdebug mode text"           ; To separate it from `global-...'
  :group 'rdebug
  :type 'string)

(define-minor-mode rdebugtrack-mode ()
  "Minor mode for tracking ruby debugging inside a process shell."
  :init-value nil
  ;; The indicator for the mode line.
  :lighter rdebugtrack-mode-text
  ;; The minor mode bindings.
  :global nil
  :group 'rdebug
  (rdebugtrack-toggle-stack-tracking 1)
  (setq rdebugtrack-is-tracking-p t)
  (local-set-key "\C-cg" 'rdebug-goto-traceback-line)
  (local-set-key "\C-cG" 'rdebug-goto-dollarbang-traceback-line)
  (add-hook 'comint-output-filter-functions 'rdebugtrack-track-stack-file)
  (run-mode-hooks 'rdebugtrack-mode-hook))


(defun rdebugtrack-toggle-stack-tracking (arg)
  (interactive "P")
  (if (not (get-buffer-process (current-buffer)))
      (message "No process associated with buffer '%s'" (current-buffer))
    ;;else
    ;; missing or 0 is toggle, >0 turn on, <0 turn off
    (if (or (not arg)
	    (zerop (setq arg (prefix-numeric-value arg))))
	(setq rdebugtrack-do-tracking-p (not rdebugtrack-do-tracking-p))
      (setq rdebugtrack-do-tracking-p (> arg 0)))
    (message "%sabled rdebug's rdebugtrack"
	     (if rdebugtrack-do-tracking-p "En" "Dis"))))


;;;###autoload
(defun turn-on-rdebugtrack-mode ()
  "Turn on rdebugtrack mode.

This function is designed to be added to hooks, for example:
  (add-hook 'comint-mode-hook 'turn-on-rdebugtrack-mode)"
  (interactive)
  (rdebugtrack-mode 1))

(defun turn-off-rdebugtrack ()
  (interactive)
  (setq rdebugtrack-is-tracking-p nil)
  (rdebugtrack-toggle-stack-tracking 0)
  (remove-hook 'comint-output-filter-functions
	       'rdebugtrack-track-stack-file))

;; rdebugtrack

;;-----------------------------------------------------------------------------
;; ALB - annotations support
;;-----------------------------------------------------------------------------

(defvar rdebug--annotation-setup-map
  (progn
    (define-hash-table-test 'str-hash 'string= 'sxhash)
    (let ((map (make-hash-table :test 'str-hash)))
      (puthash "breakpoints" 'rdebug--setup-breakpoints-buffer           map)
      (puthash "stack"       'rdebug--setup-stack-buffer                 map)
      (puthash "variables"   'rdebug--setup-variables-buffer             map)
      (puthash "watch"       'rdebug--setup-watch-buffer                 map)
      (puthash "output"      'rdebug--setup-output-buffer                map)
      (puthash "help"        'rdebug--setup-secondary-window-help-buffer map)
      map)))

(defun rdebug-process-annotation (name contents)
  (rdebug-debug-enter "rdebug-process-annotation"
    ;; Ruby-debug uses the name "starting" for process output (just like
    ;; GDB). However, it's better to present the buffer as "output" to
    ;; the user. Ditto for "display" and "watch".
    (cond ((string= name "starting")
           (setq name "output"))
          ((string= name "display")
           (setq name "watch")))
    (let ((buf (get-buffer-create
                (format "*rdebug-%s-%s*" name gud-target-name)))
          ;; Buffer local, doesn't survive the buffer change.
          (comint-buffer gud-comint-buffer))
      (with-current-buffer buf
        (setq buffer-read-only t)
        (let ((inhibit-read-only t)
              (setup-func (gethash name rdebug--annotation-setup-map)))
          (if (string= name "output")
              (goto-char (point-max))
            (erase-buffer))
          (insert contents)
          (when setup-func (funcall setup-func buf comint-buffer)))))))


;;
;; Window layout
;;

(defun rdebug-window-layout-standard (src-buf name)
  "The default rdebug window layout, see `rdebug' for more information."
  (delete-other-windows)
  (split-window nil ( / ( * (window-height) 3) 4))
  (split-window nil ( / (window-height) 3))
  (split-window-horizontally)
  (other-window 1)
  (set-window-buffer
   (selected-window) (rdebug-get-buffer "variables" name))
  (other-window 1)
  (switch-to-buffer src-buf)
  (split-window-horizontally)
  (other-window 1)
  (set-window-buffer
   (selected-window) (rdebug-get-buffer "output" name))
  (other-window 1)
  (set-window-buffer
   (selected-window) (rdebug-get-buffer "stack" name))
  (split-window-horizontally)
  (other-window 1)
  (set-window-buffer
   (selected-window) (rdebug-get-buffer "breakpoints" name))
  (other-window 1)
  (goto-char (point-max)))

(defun rdebug-window-layout-rocky (src-buf name)
  "Rocky's window layout.

3 windows. The source window is on top 4/5 of height. The
bottom is split between the command windows and a stack window.

See `rdebug' for more information."
  (delete-other-windows)
  (split-window nil ( / ( * (window-height) 4) 5))
  (set-window-buffer
   (selected-window) src-buf)
  (other-window 1)
  (set-window-buffer
   (selected-window) (rdebug-get-buffer "stack" name))
  (split-window-horizontally)
  (set-window-buffer
   (selected-window) (rdebug-get-buffer "cmd" name))
  (goto-char (point-max)))

(defun rdebug-window-layout-conservative (src-buf name)
  "A conservative rdebug window layout with three windows.

This window layout mimics the traditional debugger shell and
source window layout, it only add one secondary window.
Initially, the secondary window displays output of the debugged
process, but any secondary buffer can be displayed, press `?' in
the window for more details."
  (delete-other-windows)
  (split-window-horizontally)
  (other-window 1)
  (switch-to-buffer src-buf)
  (other-window 1)
  (split-window nil 20)
  (set-window-buffer
   (selected-window) (rdebug-get-buffer "output" name))
  (other-window 1))

(defun rdebug-window-layout-stack-of-windows (src-buf name)
  "A rdebug window layout with several secondary windows to the right.
The debugger shell and the source code window is to the left."
  (delete-other-windows)
  (split-window-horizontally)
  (split-window nil 20)
  (set-window-buffer
   (selected-window) (rdebug-get-buffer "cmd" name))
  (other-window 1)
  (switch-to-buffer src-buf)
  (other-window 1)
  (split-window)
  (split-window)
  (set-window-buffer
   (selected-window) (rdebug-get-buffer "variables" name))
  (other-window 1)
  (set-window-buffer
   (selected-window) (rdebug-get-buffer "stack" name))
  (other-window 1)
  (split-window)
  (set-window-buffer
   (selected-window) (rdebug-get-buffer "output" name))
  (other-window 1)
  (set-window-buffer
   (selected-window) (rdebug-get-buffer "breakpoints" name))
  (other-window 1))


(defun rdebug-setup-windows ()
  "Layout the window pattern for `rdebug-many-windows'. This was mostly copied
from `gdb-setup-windows', but simplified."
  (rdebug-debug-enter "rdebug-setup-windows"
    (rdebug-set-window-configuration-state 'debugger t)
    (pop-to-buffer gud-comint-buffer)
    (rdebug-process-annotation "help" "")
    (let ((buf
           (cond (gud-last-last-frame
                  (gud-find-file (car gud-last-last-frame)))
                 (gud-target-name
                  (gud-find-file gud-target-name))
                 (t
                  ;; Put buffer list in window if we
                  ;; can't find a source file.
                  (list-buffers-noselect)))))
      (funcall rdebug-window-layout-function buf gud-target-name))))


(defun rdebug-setup-windows-initially ()
  "Like `rdebug-setup-windows', but erase the content of accumulative windows.
This is called when the debugger starts."
  (let ((buf (get-buffer (format "*rdebug-output-%s*" gud-target-name))))
    (if buf
        (with-current-buffer buf
          (let ((inhibit-read-only t))
            (erase-buffer)))))
  (rdebug-setup-windows))

(defun rdebug-get-buffer (name script-name)
  (get-buffer-create (format "*rdebug-%s-%s*" name script-name)))


(defun rdebug-restore-windows ()
  "Display the initial ruby debugger window layout."
  (interactive)
  (when rdebug-many-windows
    (rdebug-setup-windows)))

(defun rdebug-display-original-window-configuration ()
  "Display the layout of windows prior to starting the ruby debugger."
  (interactive)
  (rdebug-set-window-configuration-state 'original)
  (message
   "Type `M-x rdebug-display-debugger-window-configuration RET' to restore."))

(defun rdebug-display-debugger-window-configuration ()
  "Display the corrent layout of windows of the ruby debugger."
  (interactive)
  (rdebug-set-window-configuration-state 'debugger)
  (message
   "Type `M-x rdebug-display-original-window-configuration RET' to restore."))

(defun rdebug-set-windows (&optional name)
  "Sets window used in multi-window frame and issues
rdebug-restore-windows if rdebug-many-windows is set"
  (interactive "sProgram name: ")
  (when name (setq gud-target-name name)
	(setq gud-comint-buffer (current-buffer)))
  (when gud-last-frame (setq gud-last-last-frame gud-last-frame))
  (rename-buffer (format "*rdebug-cmd-%s*" gud-target-name))
  (when rdebug-many-windows
    (rdebug-setup-windows)))

(defun rdebug-process-sentinel (process event)
  "Restore the original window configuration when the debugger process exits."
  (rdebug-debug-enter "rdebug-process-sentinel"
    (rdebug-debug-message "status=%S event=%S state=%S"
                          (process-status process)
                          event
                          rdebug-window-configuration-state)
    (gud-sentinel process event)
    ;; This will "flush" the last annotation. Especially "output"
    ;; (a.k.a. "starting") annotations don't have an end markers, if
    ;; the last command printed something.
    (if (string= event "finished\n")
        (gud-rdebug-marker-filter "\032\032\n"))
    ;; When the debugger process exited, when the comint buffer has no
    ;; buffer process (nil). When the debugger processes is replaced
    ;; with another process we should not restore the window
    ;; configuration.
    (when (and (or (eq rdebug-restore-original-window-configuration t)
                   (and (eq rdebug-restore-original-window-configuration :many)
                        rdebug-many-windows))
               (or (null (get-buffer-process gud-comint-buffer))
                   (eq process (get-buffer-process gud-comint-buffer)))
               (eq rdebug-window-configuration-state 'debugger)
               (not (eq (process-status process) 'run)))
      (rdebug-set-window-configuration-state 'original))
    ;; This unbinds the special debugger keys of the source buffers.
    (setcdr (assq 'rdebug-debugger-support-minor-mode minor-mode-map-alist)
            rdebug-debugger-support-minor-mode-map-when-deactive)))

;; Fontification and keymaps for secondary buffers (breakpoints, stack)

;; -- General, for all secondary buffers.

;; Note, we re-populate the menus of the different minor and major
;; modes. The reason is that Emacs caches the key bindings, which
;; means that wrong ones are shown when buffers are changed.

;; Remember, all menu items are added in the reverse order!

;; TODO: This is a mix of commands that apply to dedicated buffers,
;; the source buffers and the debugger shell buffer. Things like
;; "delete breakpoint" can be in source and the breakpoints window,
;; but not in the others.

(defun rdebug-populate-debugger-menu (map)
  "Populate the Rdebug 'Debugger' menu."
  (let ((menu (make-sparse-keymap)))
    (define-key map [menu-bar debugger] (cons "Debugger" menu))

    (define-key menu [break-delete]
      '(menu-item "Delete breakpoint" gud-remove))

    (define-key menu [break]
      '(menu-item "Set breakpoint" gud-break))

    (define-key menu [finish] '(menu-item "Step out" gud-finish))
    (define-key menu [step] '(menu-item "Step into" gud-step))
    (define-key menu [next] '(menu-item "Step over" gud-next))
    (define-key menu [cont] '(menu-item "Continue" gud-cont))
    (define-key menu [start] '(menu-item "Start the debugger" rdebug))

    (define-key menu [placeholder] nil)

    ;; --------------------
    ;; The "Window Layout" submeny.
    (let ((submenu (make-sparse-keymap)))
      (define-key menu [layout] (cons "Window Layout" submenu)))

    (define-key map [menu-bar debugger layout initial]
      '(menu-item "Initial Debugger Layout" rdebug-restore-windows))

    (define-key map [menu-bar debugger layout line1] '(menu-item "--"))

    (define-key map [menu-bar debugger layout debugger]
      '(menu-item "Current Debugger Layout"
                  rdebug-display-debugger-window-configuration
                  :button
                  (:radio
                   . (eq rdebug-window-configuration-state 'debugger))))

    (define-key map [menu-bar debugger layout original]
      '(menu-item "Original Layout"
                  rdebug-display-original-window-configuration
                  :button
                  (:radio
                   . (eq rdebug-window-configuration-state 'original))))

    ;; --------------------
    ;; The "View" submeny.
    (let ((submenu (make-sparse-keymap)))
      (define-key menu [view] (cons "View" submenu)))

    (define-key map [menu-bar debugger view output]
      '(menu-item "Output" rdebug-display-output-buffer))

    (define-key map [menu-bar debugger view watch]
      '(menu-item "Watch" rdebug-display-watch-buffer))

    (define-key map [menu-bar debugger view stack]
      '(menu-item "Stack trace" rdebug-display-stack-buffer))

    (define-key map [menu-bar debugger view shell]
      '(menu-item "Debugger Shell" rdebug-display-cmd-buffer))

    (define-key map [menu-bar debugger view variables]
      '(menu-item "Variables" rdebug-display-variables-buffer))

    (define-key map [menu-bar debugger view breakpoints]
      '(menu-item "Breakpoints" rdebug-display-breakpoints-buffer))
    menu))


;; -- Ruby debugger support for other modes.

(defvar rdebug-debugger-support-minor-mode-map-when-deactive
  (let ((map (make-sparse-keymap)))
    (rdebug-populate-debugger-menu map)
    map))

(defvar rdebug-debugger-support-minor-mode-map-when-active
  (let ((map (make-sparse-keymap)))
    (rdebug-populate-common-keys map)
    (rdebug-populate-debugger-menu map)
    map))


(define-minor-mode rdebug-debugger-support-minor-mode
  "Minor mode active in source buffers that use Rdebug."
  :group rdebug
  :global nil
  :init-value nil
  :keymap rdebug-debugger-support-minor-mode-map-when-deactive
  ())


;;;###autoload
(defun rdebug-turn-on-debugger-support ()
  "Enable extra source buffer support for the `rdebug' Ruby debugger.

This includes a 'Debugger' menu and special key bindings when the
debugger is active."
  (rdebug-debugger-support-minor-mode 1))


;; -- Secondary buffer support.

;; TODO: rename no-menu to something like not-top-level.
;;
;; OR: pass two maps, one top level (for menu and common) and one
;; submap for keys.
(defun rdebug-populate-secondary-buffer-map (map &optional no-menu)
  ;; Keys to view other buffers.
  (define-key map "?" 'rdebug-display-secondary-window-help-buffer)
  (define-key map "B" 'rdebug-display-breakpoints-buffer)
  (define-key map "C" 'rdebug-display-cmd-buffer)
  (define-key map "O" 'rdebug-display-output-buffer)
  (define-key map "S" 'gud-source-resync)
  (define-key map "T" 'rdebug-display-stack-buffer)
  (define-key map "V" 'rdebug-display-variables-buffer)
  (define-key map "W" 'rdebug-display-watch-buffer)
  ;; Common debugger commands.
  (define-key map " " 'gud-step)
  (define-key map "+" 'gud-step-plus)
  (define-key map "<" 'gud-up)
  (define-key map ">" 'gud-down)
  ;; (define-key map "a" 'gud-args)
  ;; (define-key map "b" 'gud-break)
  (define-key map "c" 'gud-cont)
  ;; (define-key map "d" 'gud-remove)
  (define-key map "f" 'gud-finish)
  (define-key map "n" 'gud-next)
  (define-key map "p" 'gud-print)
  (define-key map "q" 'rdebug-quit)
  (define-key map "r" 'gud-run)
  (define-key map "s" 'gud-step)
  ;; (define-key map "t" 'gud-tbreak)
  ;; Returns the menu.
  (if no-menu
      nil
    (rdebug-populate-common-keys map)
    (rdebug-populate-debugger-menu map)))


(defun rdebug-display-breakpoints-buffer ()
  "Display the rdebug breakpoints buffer."
  (interactive)
  (rdebug-display-secondary-buffer "breakpoints"))

(defun rdebug-display-cmd-buffer ()
  "Display the rdebug debugger command buffer."
  (interactive)
  (rdebug-display-secondary-buffer "cmd"))

(defun rdebug-display-watch-buffer ()
  "Display the rdebug watch buffer."
  (interactive)
  (rdebug-display-secondary-buffer "watch"))

(defun rdebug-display-output-buffer ()
  "Display the rdebug output buffer."
  (interactive)
  (rdebug-display-secondary-buffer "output"))

(defun rdebug-display-variables-buffer ()
  "Display the rdebug variables buffer."
  (interactive)
  (rdebug-display-secondary-buffer "variables"))

(defun rdebug-display-stack-buffer ()
  "Display the rdebug stack buffer."
  (interactive)
  (rdebug-display-secondary-buffer "stack"))

(defun rdebug-display-secondary-window-help-buffer ()
  "Display the rdebug stack buffer."
  (interactive)
  (rdebug-display-secondary-buffer "help"))


(defun rdebug-display-secondary-buffer (name)
  "Display one of the rdebug secondary buffers.
If the buffer doesn't exist, do nothing."
  (let* ((target-name (or (and gud-comint-buffer
                               (buffer-local-value 'gud-target-name
                                                   gud-comint-buffer))
                          gud-target-name))
         (buf-name (format "*rdebug-%s-%s*" name target-name))
         (buf (get-buffer buf-name)))
    (if (null buf)
        (message "Buffer %s not found" buf-name)
      ;; Find a suitable window to display the buffer in.
      (let ((win (get-buffer-window buf (selected-frame))))
        (cond (win
               ;; Buffer already displayed, switch to it.
               (select-window win))
              (rdebug-secondary-buffer
               ;; This is a secondary window, let's reuse it.
               (switch-to-buffer buf))
              (t
               ;; Pick another window, preferably a secondary window.
               (let* ((windows (window-list (selected-frame)))
                      (candidate (selected-window)))
                 (dolist (win windows)
                   (if (buffer-local-value 'rdebug-secondary-buffer
                                           (window-buffer win))
                       (setq candidate win)))
                 (select-window candidate)
                 (switch-to-buffer buf))))))))


;; -- breakpoints

(defvar rdebug-breakpoints-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map [mouse-2] 'rdebug-goto-breakpoint-mouse)
    (define-key map [mouse-3] 'rdebug-goto-breakpoint-mouse)
    (define-key map "t" 'rdebug-toggle-breakpoint)
    (define-key map [(control m)] 'rdebug-goto-breakpoint)
    (define-key map [?d] 'rdebug-delete-breakpoint)
    (rdebug-populate-secondary-buffer-map map)

    ;; --------------------
    ;; The "Breakpoints window" submeny.
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

(defun rdebug-delete-frame-or-window ()
  "Delete frame if there is only one window.  Otherwise delete the window."
  (interactive)
  (if (one-window-p) (delete-frame)
    (delete-window)))

(defun rdebug-breakpoints-mode ()
  "Major mode for rdebug breakpoints.

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

(defconst rdebug--breakpoint-regexp
  "^\\ +\\([0-9]+\\) \\([yn]\\) +at +\\(.+\\):\\([0-9]+\\)$"
  "Regexp to recognize breakpoint lines in rdebug breakpoint buffers.")

(defun rdebug--setup-breakpoints-buffer (buf comint-buffer)
  "Detects breakpoint lines and sets up keymap and mouse navigation."
  (rdebug-debug-enter "rdebug--setup-breakpoints-buffer"
    (with-current-buffer buf
      (let ((inhibit-read-only t) (flag))
        (rdebug-breakpoints-mode)
        (set (make-local-variable 'gud-comint-buffer) comint-buffer)
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
            (beginning-of-line)))))))

(defun rdebug-goto-breakpoint-mouse (event)
  "Displays the location in a source file of the selected breakpoint."
  (interactive "e")
  (with-current-buffer (window-buffer (posn-window (event-end event)))
    (rdebug-goto-breakpoint (posn-point (event-end event)))))

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

(defun rdebug-goto-traceback-line (pt)
  "Displays the location in a source file of the Ruby traceback line."
  (interactive "d")
  (save-excursion
    (goto-char pt)
    (let ((s (buffer-substring (line-beginning-position) (line-end-position)))
	  (gud-comint-buffer (current-buffer)))
      (when (string-match rdebug-traceback-line-re s)
        (rdebug-display-line
         (substring s (match-beginning 1) (match-end 1))
         (string-to-number (substring s (match-beginning 2) (match-end 2))))
        ))))

(defun rdebug-goto-dollarbang-traceback-line (pt)
  "Displays the location in a source file of the Ruby $! traceback line."
  (interactive "d")
  (save-excursion
    (goto-char pt)
    (let ((s (buffer-substring (line-beginning-position) (line-end-position)))
	  (gud-comint-buffer (current-buffer)))
      (when (string-match rdebug-dollarbang-traceback-line-re s)
        (rdebug-display-line
         (substring s (match-beginning 1) (match-end 1))
         (string-to-number (substring s (match-beginning 2) (match-end 2))))
        ))))

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


;; -- stack

(defvar rdebug-frames-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map [mouse-1] 'rdebug-goto-stack-frame-mouse)
    (define-key map [mouse-2] 'rdebug-goto-stack-frame-mouse)
    (define-key map [mouse-3] 'rdebug-goto-stack-frame-mouse)
    (define-key map [(control m)] 'rdebug-goto-stack-frame)
    (rdebug-populate-secondary-buffer-map map)

    ;; --------------------
    ;; The "Stack window" submeny.
    (let ((submenu (make-sparse-keymap)))
      (define-key-after map [menu-bar debugger stack]
        (cons "Stack window" submenu)
        'placeholder))

    (define-key map [menu-bar debugger stack goto]
      '(menu-item "Goto frame" rdebug-goto-stack-frame))
    map)
  "Keymap to navigate rdebug stack frames.")

(defun rdebug-frames-mode ()
  "Major mode for rdebug frames.

\\{rdebug-frames-mode-map}"
  (interactive "")
  ;; (kill-all-local-variables)
  (setq major-mode 'rdebug-frames-mode)
  (setq mode-name "RDEBUG Stack Frames")
  (set (make-local-variable 'rdebug-secondary-buffer) t)
  (use-local-map rdebug-frames-mode-map)
  ;; (set (make-local-variable 'font-lock-defaults)
  ;;     '(gdb-locals-font-lock-keywords))
  (run-mode-hooks 'rdebug-frames-mode-hook))

(defconst rdebug--stack-frame-1st-regexp
  "^\\(-->\\|   \\) +#\\([0-9]+\\)\\(.*\\)"
  "Regexp to recognize the first line of a a stack frame line in rdebug stack buffers.")

(defconst rdebug--stack-frame-2nd-regexp
  "\s+at line +\\([^:]+\\):\\([0-9]+\\)$"
  "Regexp to recognize the second line of a a stack frame line in rdebug stack buffers.")

(defconst rdebug--stack-frame-regexp
  (concat rdebug--stack-frame-1st-regexp rdebug--stack-frame-2nd-regexp)
  "Regexp to recognize a stack frame line in rdebug stack buffers.")

(defun rdebug--setup-stack-buffer (buf comint-buffer)
  "Detects stack frame lines and sets up mouse navigation."
  (rdebug-debug-enter "rdebug--setup-stack-buffer"
    (with-current-buffer buf
      (let ((inhibit-read-only t)
            ;; position in stack buffer of selected frame
            (current-frame-point nil)
            )
        (rdebug-frames-mode)
        (set (make-local-variable 'gud-comint-buffer) comint-buffer)
        (goto-char (point-min))
        (while (not (eobp))
          (let* ((b (line-beginning-position)) (e (line-end-position))
                 (s (buffer-substring b e)))
            (when (string-match rdebug--stack-frame-1st-regexp s)
              (add-text-properties
               (+ b (match-beginning 2)) (+ b (match-end 2))
               (list 'face font-lock-constant-face
                     'font-lock-face font-lock-constant-face))
              ;; Not all stack frames are on one line.
              ;; handle those that are.
              (when (string-match rdebug--stack-frame-regexp s)
                (add-text-properties
                 (+ b (match-beginning 4)) (+ b (match-end 4))
                 (list 'face font-lock-comment-face
                       'font-lock-face font-lock-comment-face))
                (add-text-properties
                 (+ b (match-beginning 5)) (+ b (match-end 5))
                 (list 'face font-lock-constant-face
                       'font-lock-face font-lock-constant-face)))

              (when (string= (substring s (match-beginning 1) (match-end 1))
                             "-->")
                ;; highlight the currently selected frame
                (add-text-properties b e
                                     (list 'face 'bold
                                           'font-lock-face 'bold))
		(setq overlay-arrow-position (make-marker))
		(set-marker overlay-arrow-position (point))
		(setq current-frame-point (point)))
              (add-text-properties b e
                                   (list 'mouse-face 'highlight
                                         'keymap rdebug-frames-mode-map))
              (let ((fn-str (substring s (match-beginning 3) (match-end 3)))
                    (fn-start (+ b (match-beginning 3))))
                (if (string-match "\\([^(]+\\)(" fn-str)
                    (add-text-properties
                     (+ fn-start (match-beginning 1))
                     (+ fn-start (match-end 1))
                     (list 'face font-lock-function-name-face
                           'font-lock-face font-lock-function-name-face))))))
          ;; remove initial '   '  or '-->'
          (beginning-of-line)
          (delete-char 3)
          (forward-line)
          (beginning-of-line))
        (when current-frame-point (goto-char current-frame-point))))))

(defun rdebug-goto-stack-frame (pt)
  "Show the rdebug stack frame corresponding at PT in the rdebug stack buffer."
  (interactive "d")
  (save-excursion
    (goto-char pt)
    (let ((s (concat "-->" (buffer-substring (line-beginning-position)
                                             (line-end-position))))
	  (s2 (if (= (line-number-at-pos (line-end-position 2))
                     (line-number-at-pos (point-max)))
		  nil
		;;else
                (buffer-substring (line-beginning-position 2)
                                  (line-end-position 2)))))
      (when (or (string-match rdebug--stack-frame-regexp s)
		;; need to match 1st line last to get the match position right
		(and s2 (string-match rdebug--stack-frame-2nd-regexp s2)
		     (string-match rdebug--stack-frame-1st-regexp s)))
        (let ((frame (substring s (match-beginning 2) (match-end 2))))
          (gud-call (concat "frame " frame)))))))

(defun rdebug-goto-stack-frame-mouse (event)
  "Show the rdebug stack frame under the mouse in the rdebug stack buffer."
  (interactive "e")
  (with-current-buffer (window-buffer (posn-window (event-end event)))
    (rdebug-goto-stack-frame (posn-point (event-end event)))))

;; -- variables

(defvar rdebug-variables-mode-map
  (let ((map (make-sparse-keymap)))
    (suppress-keymap map)
    (define-key map "\r" 'rdebug-variables-edit)
    (define-key map "e" 'rdebug-edit-variables-value)
    (define-key map [mouse-2] 'rdebug-variables-edit-mouse)
    (define-key map [mouse-3] 'rdebug-variables-edit-mouse)
    (rdebug-populate-secondary-buffer-map map)

    ;; --------------------
    ;; The "Variables window" submeny.

    (let ((submenu (make-sparse-keymap)))
      (define-key-after map [menu-bar debugger variables]
        (cons "Variables window" submenu)
        'placeholder))

    (define-key map [menu-bar debugger variables edit]
      '(menu-item "Edit" rdebug-variables-edit
                  :enable (eq major-mode 'rdebug-variables-mode)))

    map))

(defun rdebug-variables-mode ()
  "Major mode for rdebug variables.

\\{rdebug-variables-mode-map}"
  (interactive "")
  (kill-all-local-variables)
  (setq major-mode 'rdebug-variables-mode)
  (setq mode-name "RDEBUG Variables")
  (setq buffer-read-only t)
  (set (make-local-variable 'rdebug-secondary-buffer) t)
  (use-local-map rdebug-variables-mode-map)
  ;; (set (make-local-variable 'font-lock-defaults)
  ;;     '(gdb-variables-font-lock-keywords))
  (run-mode-hooks 'rdebug-variables-mode-hook))

(defun rdebug--setup-variables-buffer (buf comint-buffer)
  (rdebug-debug-enter "rdebug--setup-variables-buffer"
    (with-current-buffer buf
      (rdebug-variables-mode)
      (set (make-local-variable 'gud-comint-buffer) comint-buffer))))

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


;; -- watch (the "display" annotation)

(defvar rdebug-watch-mode-map
  (let ((map (make-sparse-keymap)))
    (suppress-keymap map)
    (define-key map "a" 'rdebug-watch-add)
    (define-key map "\C-d" 'rdebug-watch-delete)
    (define-key map "d" 'rdebug-watch-delete)
    (define-key map "e" 'rdebug-watch-edit)
    (define-key map "\r" 'rdebug-watch-edit)
    (rdebug-populate-secondary-buffer-map map)

    ;; --------------------
    ;; The "Watch window" submeny.
    (let ((submenu (make-sparse-keymap)))
      (define-key-after map [menu-bar debugger watch]
        (cons "Watch window" submenu)
        'placeholder))

    (define-key map [menu-bar debugger watch delete]
      '(menu-item "Delete" rdebug-watch-delete
                  :enable (eq major-mode 'rdebug-watch-mode)))
    (define-key map [menu-bar debugger watch goto]
      '(menu-item "Edit" rdebug-watch-edit
                  :enable (eq major-mode 'rdebug-watch-mode)))
    (define-key map [menu-bar debugger watch add]
      '(menu-item "Add" rdebug-watch-add))

    map))

(defun rdebug-watch-mode ()
  "Major mode for rdebug watch window.

\\{rdebug-watch-mode}"
  (interactive)
  (kill-all-local-variables)
  (setq major-mode 'rdebug-watch-mode)
  (setq mode-name "RDEBUG Watch")
  (setq buffer-read-only t)
  (set (make-local-variable 'rdebug-secondary-buffer) t)
  (use-local-map rdebug-watch-mode-map)
  (run-mode-hooks 'rdebug-watch-mode-hook))

(defun rdebug--setup-watch-buffer (buf comint-buffer)
  (rdebug-debug-enter "rdebug--setup-watch-buffer"
    (with-current-buffer buf
      (rdebug-watch-mode)
      (set (make-local-variable 'gud-comint-buffer) comint-buffer))))

(defun rdebug-watch-add (expr)
  (interactive "sRuby expression: ")
  (if (not (string= expr ""))
      (gud-call (format "display %s" expr))))

(defun rdebug-watch-delete ()
  (interactive)
  (save-excursion
    (beginning-of-line)
    (if (looking-at "^\\([0-9]+\\):")
        (gud-call (format "undisplay %s" (match-string 1))))))

(defun rdebug-watch-edit (number expr)
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

;; -- output

(defvar rdebug-output-mode-map
  (let ((map (make-sparse-keymap)))
    (suppress-keymap map)
    (rdebug-populate-secondary-buffer-map map)
    map))

(defun rdebug-output-mode ()
  "Major mode for rdebug output window.

\\{rdebug-output-mode}"
  (interactive)
  (kill-all-local-variables)
  (setq major-mode 'rdebug-output-mode)
  (setq mode-name "RDEBUG Output")
  (setq buffer-read-only t)
  (set (make-local-variable 'rdebug-secondary-buffer) t)
  (use-local-map rdebug-output-mode-map)
  (run-mode-hooks 'rdebug-output-mode-hook))

(defun rdebug--setup-output-buffer (buf comint-buffer)
  (rdebug-debug-enter "rdebug--setup-output-buffer"
    (with-current-buffer buf
      (rdebug-output-mode)
      (set (make-local-variable 'gud-comint-buffer) comint-buffer))))

;; -- help

(defvar rdebug-secondary-window-help-mode-map
  (let ((map (make-sparse-keymap)))
    (suppress-keymap map)
    (rdebug-populate-secondary-buffer-map map)
    map))

(defun rdebug-secondary-window-help-mode ()
  "Major mode for watching help test in secondary buffers.

\\{rdebug-secondary-window-help-mode-map}"
  (interactive)
  (kill-all-local-variables)
  (setq major-mode 'rdebug-secondary-window-help-mode)
  (setq mode-name "RDEBUG Help")
  (setq buffer-read-only t)
  (set (make-local-variable 'rdebug-secondary-buffer) t)
  (use-local-map rdebug-secondary-window-help-mode-map)
  (run-mode-hooks 'rdebug-secondary-window-help-mode-hook))


(defun rdebug--setup-secondary-window-help-buffer (buf comint-buffer)
  (rdebug-debug-enter "rdebug--setup-secondary-window-help-buffer"
    (with-current-buffer buf
      (rdebug-secondary-window-help-mode)
      (set (make-local-variable 'gud-comint-buffer) comint-buffer)
      (insert "\

This is a rdebug secondary window, you can use it to watch a
number of help buffers. Use capital letters to switch between the
available buffers. Lower case letters (and other key
combinations) are used to issue buffer-specific commands.

Press `C-h m' for more help, when the individual buffers are visible.

 B - Breakpoints buffer.
 C - Command buffer (the debugger shell)
 O - Output window
 S - go to source frame
 T - Stack trace buffer
 V - Variables buffer
 W - Watch buffer

 SPC - step (into)
 +   - step+
 c   - continue
 f   - finish (step out)
 n   - next (step over)
 p   - print
 q   - quit
 r   - run
 s   - step (into)

 > - go down frame (with numeric argument goes down that many frames)
 < - go up one frame (with numeric argument goes down that many frames)

 ? - This help text.
"))))

;; -- Reset support

(defadvice gud-reset (before rdebug-reset)
  "rdebug cleanup - remove debugger's internal buffers (frame, breakpoints,
etc.)."
  (dolist (buffer (buffer-list))
    (when (string-match "\\*rdebug-[a-z]+\\*" (buffer-name buffer))
      (let ((w (get-buffer-window buffer)))
        (when w (delete-window w)))
      (kill-buffer buffer))))
(ad-activate 'gud-reset)


(provide 'rdebug-core)

;;; Local variables:
;;; eval:(put 'rdebug-debug-enter 'lisp-indent-hook 1)
;;; End:

;;; rdebug-code.el ends here
