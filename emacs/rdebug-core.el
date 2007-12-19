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
;;  * Window layout with dedicated windows for:
;;      + Local and member variables
;;      + Stack trace
;;      + Display expressions
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

(defcustom rdebug-line-width 120
  "Length of line before truncation occurs.
This value limits output in secondary buffers."
  :type 'integer
  :group 'rdebug)

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

* `rdebug-populate-common-keys-standard' -- Bind according to a widely used
  debugger convention:

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


;; ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
;; NO USER DEFINABLE VARIABLES BEYOND THIS POINT

(defconst gud-rdebug-marker-regexp
  "^\\(\\(?:[a-zA-Z]:\\)?[^:\n]*\\):\\([0-9]*\\).*\n"
  "Regular expression used to find a file location given by rdebug.

Program-location lines look like this:
   /tmp/gcd.rb:29:  gcd
   /tmp/gcd.rb:29
   C:/tmp/gcd.rb:29
   \\sources\\capfilterscanner\\capanalyzer.rb:3:  <module>
")

(defconst rdebug-marker-regexp-file-group 2
  "Group position in rdebug-position-re that matches the file name.")

(defconst rdebug-marker-regexp-line-group 3
  "Group position in rdebug-position-re that matches the line number.")

(defconst rdebug-annotation-start-regexp
  "\\([a-z]+\\)\n"
  "Regular expression to match the start of an annotation.
Note that in contrast to gud-rdebug-marker-regexp, we don't allow a colon.
That's what distinguishes the two." )

(defconst rdebug-annotation-end-regexp
  "\n"
  "Regular expression to match the end of an annotation.")

(defvar rdebug-original-window-configuration nil
  "The window layout rdebug should restore when the debugger exits.")

(defvar rdebug-debugger-window-configuration nil
  "The saved window layout of the debugger.")

;; This is used to ensure that the original frame configuration is
;; restored even when the user re-starts the debugger several times.
(defvar rdebug-window-configuration-state 'original
  "Represent the window layout that currently is in use.
Can be `original' or `debugger'.")

;; Terminology: a "secondary buffer" is the physical emacs buffer,
;; which can be visible or invisible. A "secondary window", is a window
;; that rdebug is reusing to display different secondary buffers.
;;
;; For example, the "secondary-window-help" buffer is named the way it
;; is since it gives help on how the secondary window is used.
(defvar rdebug-secondary-buffer nil
  "Non-nil for rdebug secondary buffers (e.g. the breakpoints buffer).")

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
  "Filter function for process output of the rdebug Ruby debugger."
  (rdebug-debug-enter "gud-rdebug-marker-filter"
    (rdebug-debug-message "GOT: %S" string)
    (setq gud-marker-acc (concat gud-marker-acc string))
    (let ((output "") s s2 (tmp ""))

      ;; ALB first we process the annotations (if any)
      (while (setq s (string-match rdebug-annotation-start-regexp
				   gud-marker-acc))
	(rdebug-debug-message "ACC: %S" gud-marker-acc)
	(let ((name (substring gud-marker-acc
                               (match-beginning 1) (match-end 1)))
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
	    ;; otherwise, save the partial annotation to a temporary,
	    ;; and re-add it to gud-marker-acc after normal output has
	    ;; been processed
	    (setq tmp (substring gud-marker-acc s))
	    (setq gud-marker-acc (substring gud-marker-acc 0 s)))))

      (when (setq s (string-match rdebug-annotation-end-regexp gud-marker-acc))
	;; save the beginning of gud-marker-acc to tmp, remove it and
	;; restore it after normal output has been processed
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

      ;; Display the source file where we want it, gud will only pick
      ;; an arbitrary window.
      (if gud-last-frame
          (rdebug-pick-source-window))

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
  "Pick out the script name from the command line.
Return a list of that and whether the annotate option was set.
Initially annotate should be set to nil."
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

Two states are supported, `original' and `debugger'.

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
  "Bind the basic debugger key layout used by many debuggers.

\\{rdebug-example-map-standard}"
  (define-key map [f5]    'gud-cont)
  (define-key map [S-f5]  'rdebug-quit)
  (define-key map [f9]    'gud-break)   ; TODO: Should be "toggle"
  (define-key map [f10]   'gud-next)
  (define-key map [f11]   'gud-step)
  (define-key map [S-f11] 'gud-finish))


;; TODO: Verify and complement.
(defun rdebug-populate-common-keys-eclipse (map)
  "Bind the basic debugger key layout used by Eclipse.

\\{rdebug-example-map-eclipse}"
  ;;(define-key map []  'gud-cont)
  ;;(define-key map []  'rdebug-quit)
  (define-key map [S-C-b]    'gud-break) ; TODO: Should be "toggle"
  (define-key map [f6]   'gud-next)
  (define-key map [f5]   'gud-step)
  (define-key map [f7] 'gud-finish))


;; TODO: Verify and complement.
(defun rdebug-populate-common-keys-netbeans (map)
  "Bind the basic debugger key layout used by NetBeans.

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
    map)
  "Rdebug Standard common keymap used only in doc-string.")


(defvar rdebug-example-map-eclipse
  (let ((map (make-sparse-keymap)))
    (rdebug-populate-common-keys-eclipse map)
    map)
  "Rdebug Eclipse compatibility common keymap used only in doc-string.")


(defvar rdebug-example-map-netbeans
  (let ((map (make-sparse-keymap)))
    (rdebug-populate-common-keys-netbeans map)
    map)
  "Rdebug NetBeans compatibility common keymap used only in doc-string.")


(defun rdebug-populate-common-keys (map)
  "Define the keys that are used by all debugger windows, even by the source.

The variable `rdebug-populate-common-keys-function' controls the layout."
  (if rdebug-populate-common-keys-function
      (funcall rdebug-populate-common-keys-function map)))


;; -- The debugger

;; -- Digit keys, go to specific entry in some seconday buffers.

(defvar rdebug-goto-entry-acc "")

(defun rdebug-goto-entry-try (str)
  (if (re-search-forward (concat "^[^0-9]*\\(" str "\\)[^0-9]") nil t)
      (progn
        (setq rdebug-goto-entry-acc str)
        (goto-char (match-end 1))
        t)
    nil))


(defun rdebug-goto-entry-n ()
  "Go to an entry number."
  (interactive)
  (let ((keys (this-command-keys)))
    (if (and (stringp keys)
             (= (length keys) 1))
        (progn
          (if (not (eq last-command 'rdebug-goto-entry-n))
              (setq rdebug-goto-entry-acc ""))
          (let ((p nil))
            (save-excursion
              (goto-char (point-min))
              (if (or (rdebug-goto-entry-try (concat rdebug-goto-entry-acc keys))
                      (rdebug-goto-entry-try keys))
                  (setq p (point))))
            (when p
              (goto-char p))))
      (message "`rdebug-goto-entry-n' must be bound to a number key"))))


(defun rdebug-populate-digit-keys (map)
  (define-key map "0" 'rdebug-goto-entry-n)
  (define-key map "1" 'rdebug-goto-entry-n)
  (define-key map "2" 'rdebug-goto-entry-n)
  (define-key map "3" 'rdebug-goto-entry-n)
  (define-key map "4" 'rdebug-goto-entry-n)
  (define-key map "5" 'rdebug-goto-entry-n)
  (define-key map "6" 'rdebug-goto-entry-n)
  (define-key map "7" 'rdebug-goto-entry-n)
  (define-key map "8" 'rdebug-goto-entry-n)
  (define-key map "9" 'rdebug-goto-entry-n))


;; have to bind rdebug-file-queue before installing the kill-emacs-hook
(defvar rdebug-file-queue nil
  "Queue of Makefile temp files awaiting execution.
Currently-active file is at the head of the list.")


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
	  (set (make-local-variable 'current-line-number) (line-number-at-pos))
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
  "Create the debugger user interface window layout.

This function displays the source file (or, in some cases, a
buffer list) and creates the window layout. The variable
`rdebug-window-layout-function' controls the function that is
used to perform the actual layout.

This is only used when `rdebug-many-windows' is non-nil."
  (rdebug-debug-enter "rdebug-setup-windows"
    (rdebug-set-window-configuration-state 'debugger t)
    (pop-to-buffer gud-comint-buffer)
    (maphash
     (lambda (name func)
       (rdebug-process-annotation name ""))
     rdebug--annotation-setup-map)
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
  "Return a rdebug buffer for displaying NAME when debugging SCRIPT-NAME.
If the buffer doesn't exists it is created."
  (get-buffer-create (format "*rdebug-%s-%s*" name script-name)))

(defun rdebug-restore-windows ()
  "Display the initial ruby debugger window layout."
  (interactive)
  (when rdebug-many-windows
    (rdebug-setup-windows)))

(defun rdebug-display-original-window-configuration ()
  "Display the layout of windows prior to starting the rdebug Ruby debugger."
  (interactive)
  (rdebug-set-window-configuration-state 'original)
  (message
   "Type `M-x rdebug-display-debugger-window-configuration RET' to restore."))

(defun rdebug-display-debugger-window-configuration ()
  "Display the current layout of windows of the rdebug Ruby debugger."
  (interactive)
  (rdebug-set-window-configuration-state 'debugger)
  (message
   "Type `M-x rdebug-display-original-window-configuration RET' to restore."))

(defun rdebug-set-windows (&optional name)
  "Rename the processor buffer to correspond to script name NAME.

When `rdebug-many-windows' is non-nil, the initial debugger
window layout is used."
  (interactive "sProgram name: ")
  (when name
    (setq gud-target-name name)
    (setq gud-comint-buffer (current-buffer)))
  (when gud-last-frame
    (setq gud-last-last-frame gud-last-frame))
  (rename-buffer (format "*rdebug-cmd-%s*" gud-target-name))
  (when rdebug-many-windows
    (rdebug-setup-windows)))

;; Fontification and keymaps for secondary buffers (breakpoints, stack)

;; -- General, for all secondary buffers.


(defun rdebug-menu-item (common-map name cmd &rest args)
  "Return a menu item entry with the correct key bindings.

A command can be bound to a number of different key sequences. If
the rdebug common map contains a binding it is displayed in the
menu. (The common map typically contains function key bindings.)"
  (let ((key-binding-list (where-is-internal cmd (list common-map)))
        (hint '()))
    (if key-binding-list
        (setq hint (list :key-sequence (car key-binding-list))))
    (append (list 'menu-item name cmd)
            hint
            args)))


;; Note, we re-populate the menus of the different minor and major
;; modes. The reason is that Emacs caches the key bindings, which
;; means that wrong ones are shown when buffers are changed.

;; Remember, all menu items are added in the reverse order!

(defun rdebug-populate-debugger-menu (map)
  "Populate the Rdebug 'Debugger' menu."
  (let ((menu (make-sparse-keymap))
        (common-map (make-sparse-keymap)))
    ;; Use a simple common map to find the best key sequence to display in menu.
    (rdebug-populate-common-keys common-map)

    (define-key map [menu-bar debugger] (cons "Debugger" menu))

    (define-key menu [break-delete]
      (rdebug-menu-item common-map "Delete breakpoint" 'gud-remove))

    (define-key menu [break]
      (rdebug-menu-item common-map "Set breakpoint" 'gud-break))

    (define-key menu [finish] (rdebug-menu-item common-map "Step out" 'gud-finish))
    (define-key menu [step]   (rdebug-menu-item common-map "Step into" 'gud-step))
    (define-key menu [next]   (rdebug-menu-item common-map "Step over" 'gud-next))
    (define-key menu [cont]   (rdebug-menu-item common-map "Continue" 'gud-cont))
    (define-key menu [start]
      (rdebug-menu-item common-map "Start the debugger" 'rdebug))

    (define-key menu [placeholder] nil)

    ;; --------------------
    ;; The "Window Layout" submeny.
    (let ((submenu (make-sparse-keymap)))
      (define-key menu [layout] (cons "Window Layout" submenu)))

    (define-key map [menu-bar debugger layout initial]
      (rdebug-menu-item common-map
                        "Initial Debugger Layout" 'rdebug-restore-windows))

    (define-key map [menu-bar debugger layout line1] '(menu-item "--"))

    (define-key map [menu-bar debugger layout debugger]
      (rdebug-menu-item common-map "Current Debugger Layout"
                        'rdebug-display-debugger-window-configuration
                        :button
                        '(:radio
                          . (eq rdebug-window-configuration-state 'debugger))))

    (define-key map [menu-bar debugger layout original]
      (rdebug-menu-item common-map "Original Layout"
                        'rdebug-display-original-window-configuration
                        :button
                        '(:radio
                          . (eq rdebug-window-configuration-state 'original))))

    ;; --------------------
    ;; The "View" submeny.
    (let ((submenu (make-sparse-keymap)))
      (define-key menu [view] (cons "View" submenu)))

    (define-key map [menu-bar debugger view output]
      (rdebug-menu-item common-map "Output" 'rdebug-display-output-buffer))

    (define-key map [menu-bar debugger view watch]
      (rdebug-menu-item common-map "Watch" 'rdebug-display-watch-buffer))

    (define-key map [menu-bar debugger view stack]
      (rdebug-menu-item common-map "Stack trace" 'rdebug-display-stack-buffer))

    (define-key map [menu-bar debugger view shell]
      (rdebug-menu-item common-map "Debugger Shell" 'rdebug-display-cmd-buffer))

    (define-key map [menu-bar debugger view variables]
      (rdebug-menu-item common-map "Variables" 'rdebug-display-variables-buffer))

    (define-key map [menu-bar debugger view breakpoints]
      (rdebug-menu-item common-map
                        "Breakpoints" 'rdebug-display-breakpoints-buffer))
    menu))


;; -- Secondary buffer support.

(defun rdebug-populate-secondary-buffer-map-plain (map)
  "Bind the plain keys used in rdebug secondary buffers.

This does not menus or prefix keys."
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
  (define-key map "s" 'gud-step))


(defun rdebug-populate-secondary-buffer-map (map)
  "Bind all common keys and menu used in the rdebug seondary buffers.
This includes the keys bound to `gud-key-prefix' (typically C-x
C-a)."
  (rdebug-populate-secondary-buffer-map-plain map)
  (rdebug-populate-common-keys map)
  (rdebug-populate-debugger-menu map)
  (let ((prefix-map (make-sparse-keymap)))
    (rdebug-populate-secondary-buffer-map-plain prefix-map)
    (define-key map gud-key-prefix prefix-map)))


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
               (let ((windows (window-list (selected-frame)))
                     (candidate (selected-window)))
                 (dolist (win windows)
                   (if (buffer-local-value 'rdebug-secondary-buffer
                                           (window-buffer win))
                       (setq candidate win)))
                 (select-window candidate)
                 (switch-to-buffer buf))))))))


;; Note: The generic `gud' framework contains special code to handle
;; this for GDB (see `gud-display-line') which we, unfortuately can't
;; use. Instead, we call `rdebug-pick-source-window' from
;; `gud-rdebug-marker-filter'. When gud becomes more generic we could
;; hopefully solve this in another way.
;;
;; The machanism is that `rdebug-pick-source-window' displays the
;; source file in the window of our choice, and gud kindly re-uses
;; that window.


(defun rdebug-pick-source-window-categorize-window (win)
  "Return how suiteable this window is to display the source buffer.
The higher score the better."
  (let ((buffer (window-buffer win)))
    (cond ((eq buffer gud-comint-buffer)
           0)
          ((buffer-local-value 'rdebug-secondary-buffer buffer)
           1)
          ((eq (buffer-local-value 'major-mode buffer) 'ruby-mode)
           3)                           ; Pick me! Pick me!
          (t
           2))))


(defun rdebug-pick-source-window ()
  "Display the source file pointed to by `gud-last-frame'"
  (let* ((buffer (gud-find-file (car gud-last-frame)))
         (window
          (or
           ;; Buffer is already visible, re-use the window.
           (get-buffer-window buffer)
           ;; Re-use the last window
           (and gud-last-last-frame
                (get-buffer-window (gud-find-file (car gud-last-last-frame))))
           ;; Find a non-rdebug window.
           (let ((candidate nil)
                 (candidate-score -1))
             (dolist (win (window-list (selected-frame)))
               (let ((score (rdebug-pick-source-window-categorize-window win)))
                 (if (> score candidate-score)
                     (progn
                       (setq candidate       win)
                       (setq candidate-score score)))))
             candidate))))
    (save-selected-window
      (select-window window)
      (switch-to-buffer buffer))))


;; -- breakpoints

(defvar rdebug-breakpoints-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map [mouse-2] 'rdebug-goto-breakpoint-mouse)
    (define-key map [mouse-3] 'rdebug-goto-breakpoint-mouse)
    (define-key map "t" 'rdebug-toggle-breakpoint)
    (rdebug-populate-digit-keys map)
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

(defconst rdebug--breakpoint-regexp
  "^\\ +\\([0-9]+\\) \\([yn]\\) +at +\\(.+\\):\\([0-9]+\\)$"
  "Regexp to recognize breakpoint lines in rdebug breakpoint buffers.")

(defun rdebug--setup-breakpoints-buffer (buf comint-buffer)
  "Detects breakpoint lines and sets up keymap and mouse navigation."
  (rdebug-debug-enter "rdebug--setup-breakpoints-buffer"
    (with-current-buffer buf
      (let ((inhibit-read-only t) 
	    (old-line-number (buffer-local-value 'current-line-number buf))
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
    (rdebug-populate-digit-keys map)
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
 "Major mode for displaying the stack trace in the `rdebug' Ruby debugger.

\\{rdebug-frames-mode-map}"
  (interactive)
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
  "Regexp to match the first line of a stack frame in rdebug stack buffers.")

(defconst rdebug--stack-frame-2nd-regexp
  "\s+at line +\\([^:]+\\):\\([0-9]+\\)$"
  "Regexp to match the second line of a stack frame in rdebug stack buffers.")

(defconst rdebug--stack-frame-regexp
  (concat rdebug--stack-frame-1st-regexp rdebug--stack-frame-2nd-regexp)
  "Regexp to recognize a stack frame line in rdebug stack buffers.")

(defun rdebug--setup-stack-buffer (buf comint-buffer)
  "Detects stack frame lines and sets up mouse navigation."
  (rdebug-debug-enter "rdebug--setup-stack-buffer"
    (with-current-buffer buf
      (let ((inhibit-read-only t)
            ;; position in stack buffer of selected frame
            (current-frame-point nil))
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
        (when current-frame-point
          (goto-char current-frame-point))))))

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

    map)
  "Keymap used in the variables buffer in the `rdebug' Ruby debugger.")

(defvar rdebug-variables-font-lock-keywords
  '(("@[a-zA-Z0-9_]+" 0 font-lock-variable-name-face)
    ("\\<\\(nil\\|true\\|false\\)\\>" 0 font-lock-constant-face)
    ("#<\\([a-zA-Z0-9_]+\\):\\([0-9a-fx]*\\)"
     (1 font-lock-type-face)
     (2 font-lock-constant-face)))
  "Font-lock rules for the variables and watch windows in `rdebug'.")

(defun rdebug-variables-mode ()
  "Major mode for the variables buffer in the `rdebug' Ruby debugger.

\\{rdebug-variables-mode-map}"
  (interactive)
  (kill-all-local-variables)
  (setq major-mode 'rdebug-variables-mode)
  (setq mode-name "RDEBUG Variables")
  (setq buffer-read-only t)
  (setq truncate-lines t)
  (set (make-local-variable 'rdebug-secondary-buffer) t)
  (set (make-local-variable 'font-lock-defaults)
       '(rdebug-variables-font-lock-keywords))
  (use-local-map rdebug-variables-mode-map)
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
    (rdebug-populate-digit-keys map)
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

    map)
  "Keymap used in the watch buffer in the `rdebug' Ruby debugger.")

(defun rdebug-watch-mode ()
  "Major mode for displaying watched expressions in the `rdebug' Ruby debugger.

\\{rdebug-watch-mode}"
  (interactive)
  (kill-all-local-variables)
  (setq major-mode 'rdebug-watch-mode)
  (setq mode-name "RDEBUG Watch")
  (setq buffer-read-only t)
  (setq truncate-lines t)
  (set (make-local-variable 'rdebug-secondary-buffer) t)
  (set (make-local-variable 'font-lock-defaults)
       '(rdebug-variables-font-lock-keywords))
  (use-local-map rdebug-watch-mode-map)
  (run-mode-hooks 'rdebug-watch-mode-hook))

(defun rdebug--setup-watch-buffer (buf comint-buffer)
  (rdebug-debug-enter "rdebug--setup-watch-buffer"
    (with-current-buffer buf
      (rdebug-watch-mode)
      (set (make-local-variable 'gud-comint-buffer) comint-buffer))))

(defun rdebug-watch-add (expr)
  "Add an expression to watch in the `rdebug' Ruby debugger."
  (interactive "sRuby expression: ")
  (if (not (string= expr ""))
      (gud-call (format "display %s" expr))))

(defun rdebug-watch-delete ()
  "Delete a watched expression in the `rdebug' Ruby debugger."
  (interactive)
  (save-excursion
    (beginning-of-line)
    (if (looking-at "^\\([0-9]+\\):")
        (gud-call (format "undisplay %s" (match-string 1))))))

(defun rdebug-watch-edit (number expr)
  "Edit a watched expression in the `rdebug' Ruby debugger."
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
    map)
  "Keymap used in the output buffer in the `rdebug' Ruby debugger.")

(defun rdebug-output-mode ()
  "Major mode for displaying the script output in the `rdebug' Ruby debugger.

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
    map)
  "Keymap used in the help buffer in the `rdebug' Ruby debugger.")

(defun rdebug-secondary-window-help-mode ()
  "Major mode for the secondary buffer help text in the `rdebug' Ruby debugger.

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




;; -- Ruby debugger support for other modes.

(defvar rdebug-debugger-support-minor-mode-map-when-deactive
  (let ((map (make-sparse-keymap))
        (prefix-map (make-sparse-keymap)))
    (rdebug-populate-debugger-menu map)
    (rdebug-populate-secondary-buffer-map-plain prefix-map)
    (define-key map gud-key-prefix prefix-map)
    map)
  "Keymap used by rdebugs support minor mode when the debugger is active.")

(defvar rdebug-debugger-support-minor-mode-map-when-active
  (let ((map (make-sparse-keymap))
        (prefix-map (make-sparse-keymap)))
    (rdebug-populate-debugger-menu map)
    (rdebug-populate-secondary-buffer-map-plain prefix-map)
    (define-key map gud-key-prefix prefix-map)
    (rdebug-populate-common-keys map)
    map)
  "Keymap used by rdebugs support minor mode when the debugger not active.")


(define-minor-mode rdebug-debugger-support-minor-mode
  "Minor mode active in source buffers that use the `rdebug' Ruby debugger."
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


;; -- The `rdebug' command and support functions.

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


;; -- Reset support

(defadvice gud-reset (before rdebug-reset)
  "rdebug cleanup - remove debugger's internal buffers (frame, breakpoints,
etc.)."
  (dolist (buffer (buffer-list))
    (when (string-match "\\*rdebug-[a-z]+\\*" (buffer-name buffer))
      (let ((w (get-buffer-window buffer)))
        (when w
          (delete-window w)))
      (kill-buffer buffer))))
(ad-activate 'gud-reset)

;;;###autoload
(defun rdebug (command-line)
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
           (rdebug-buffer (get-buffer rdebug-buffer-name)))

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
	    (gud-call (format "set width %d" rdebug-line-width))
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
      (let ((prefix-map (make-sparse-keymap)))
        (define-key (current-local-map) gud-key-prefix prefix-map)
        (rdebug-populate-secondary-buffer-map-plain prefix-map))

      (rdebug-populate-common-keys (current-local-map))
      (rdebug-populate-debugger-menu (current-local-map))

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


(provide 'rdebug-core)

;;; Local variables:
;;; eval:(put 'rdebug-debug-enter 'lisp-indent-hook 1)
;;; End:

;;; rdebug-core.el ends here
