;; Copyright (C) 2006, 2007 Free Software Foundation, Inc.
;; This file is (not yet) part of GNU Emacs.

;; GNU Emacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

(require 'gud)


;; user definable variables
;; vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv

(defcustom gud-rdebug-command-name "rdebug --no-control --annotate=1"
  "File name for executing the Ruby debugger.
This should be an executable on your path, or an absolute file name."
  :type 'string
  :group 'gud)

(defcustom rdebug-temp-directory
  (let ((ok '(lambda (x)
	       (and x
		    (setq x (expand-file-name x)) ; always true
		    (file-directory-p x)
		    (file-writable-p x)
		    x))))
    (or (funcall ok (getenv "TMPDIR"))
	(funcall ok "/usr/tmp")
	(funcall ok "/tmp")
	(funcall ok "/var/tmp")
	(funcall ok  ".")
	(error
	 "Couldn't find a usable temp directory -- set `rdebug-temp-directory'")))
  "*Directory used for temporary files created by a *Python* process.
By default, the first directory from this list that exists and that you
can write into: the value (if any) of the environment variable TMPDIR,
/usr/tmp, /tmp, /var/tmp, or the current directory."
  :type 'string
  :group 'rdebug)

(defgroup rdebugtrack nil
  "Rdebug file tracking by watching the prompt."
  :prefix "rdebug-rdebugtrack-"
  :group 'shell)

(defcustom rdebug-rdebugtrack-do-tracking-p nil
  "*Controls whether the rdebugtrack feature is enabled or not.
When non-nil, rdebugtrack is enabled in all comint-based buffers,
e.g. shell buffers and the *Python* buffer.  When using rdebug to debug a
Python program, rdebugtrack notices the rdebug prompt and displays the
source file and line that the program is stopped at, much the same way
as gud-mode does for debugging C programs with gdb."
  :type 'boolean
  :group 'rdebug)
(make-variable-buffer-local 'rdebug-rdebugtrack-do-tracking-p)

(defcustom rdebug-many-windows t
  "*If non-nil, display secondary rdebug windows, in a layout similar to `gdba'.
However only set to the multi-window display if the rdebug
command invocation has an annotate options (\"--annotate 1\"."
  :type 'boolean
  :group 'rdebug)

(defcustom rdebug-rdebugtrack-minor-mode-string " RDEBUG"
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
   /tmp/gcd.py:29:  gcd
   /tmp/gcd.py:29
   \\sources\\capfilterscanner\\capanalyzer.py:3:  <module>
")

(defconst rdebug-marker-regexp-file-group 2
  "Group position in rdebug-position-re that matches the file name.")

(defconst rdebug-marker-regexp-line-group 3
 "Group position in rdebug-position-re that matches the line number.")

(defconst rdebug-annotation-start-regexp
  "^\\([a-z]+\\)\n"
  "Start of an annotation. Note that in contrast to
gud-rdebug-marker-regexp, we don't allow a colon. That's what
distinguishes the two."  )
(defconst rdebug-annotation-end-regexp
  "\n")

;; rdebugtrack constants
(defconst rdebug-rdebugtrack-stack-entry-regexp
      "^(\\([-a-zA-Z0-9_/.]*\\):\\([0-9]+\\)):[ \t]?\\(.*\n\\)"
  "Regular expression rdebugtrack uses to find a stack trace entry.")

(defconst rdebug-rdebugtrack-input-prompt "\n(+rdb:\\([0-9]+\\|post-mortem\\))+ *"
  "Regular expression rdebugtrack uses to recognize a rdebug prompt.")

(defconst rdebug-rdebugtrack-track-range 10000
  "Max number of characters from end of buffer to search for stack entry.")

(defun gud-rdebug-massage-args (file args)
  args)

;; There's no guarantee that Emacs will hand the filter the entire
;; marker at once; it could be broken up across several strings.  We
;; might even receive a big chunk with several markers in it.  If we
;; receive a chunk of text which looks like it might contain the
;; beginning of a marker, we save it here between calls to the
;; filter.
(defun gud-rdebug-marker-filter (string)
  ;(message "GOT: %s" string)
  (setq gud-marker-acc (concat gud-marker-acc string))
  (let ((output "") s s2 (tmp ""))

    ;; ALB first we process the annotations (if any)
    (while (setq s (string-match rdebug-annotation-start-regexp
                                 gud-marker-acc))
      ;(message "ACC: %s" gud-marker-acc)
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

    output))

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
       ;else
	 (list nil annotate-p)))
      ((string-match "^-[a-zA-z]" arg) (rdebug-get-script-name args annotate-p))
      ((string-match "^--[a-zA-z]+" arg) (rdebug-get-script-name args annotate-p))
      ((string-match "^rdebug" arg) (rdebug-get-script-name args annotate-p))
     ; found script name (or nil
      (t (list arg annotate-p)))))

; From Emacs 23
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

;;;###autoload
(defun rdebug (command-line)
  "Run rdebug on program FILE in buffer *rdebug-cmd-FILE*.
The directory containing FILE becomes the initial working directory
and source-file directory for your debugger.

The custom variable `gud-rdebug-command-name' sets the pattern used
to invoke pydb.

If `rdebug-many-windows' is nil (the default value) then pydb just
starts with two windows: one displaying the GUD buffer and the
other with the source file with the main routine of the inferior.

If `rdebug-many-windows' is t, regardless of the value of the layout
below will appear.

+----------------------------------------------------------------------+
|                               GDB Toolbar                            |
+-----------------------------------+----------------------------------+
| GUD buffer (I/O of rdebug)        | Locals buffer                    |
|                                   |                                  |
|                                   |                                  |
|                                   |                                  |
+-----------------------------------+----------------------------------+
| Source buffer                                                        |
|                                                                      |
+-----------------------------------+----------------------------------+
| Stack buffer                      | Breakpoints buffer               |
| RET rdebug-goto-stack-frame       | SPC  rdebug-toggle-breakpoint    |
|                                   | RET  rdebug-goto-breakpoint      |
|                                   | D    rdebug-delete-breakpoint    |
+-----------------------------------+----------------------------------+
"
  (interactive
   (list (gud-query-cmdline 'rdebug)))

  ; Parse the command line and pick out the script name and whether --annotate
  ; has been set.
  (let* ((words (split-string-and-unquote command-line))
	(script-name-annotate-p (rdebug-get-script-name 
			       (gud-rdebug-massage-args "1" words) nil))
	(gud-target-name (file-name-nondirectory (car script-name-annotate-p)))
	(annotate-p (cadr script-name-annotate-p))
	(rdebug-buffer-name (format "*rdebug-cmd-%s*" gud-target-name))
	(rdebug-buffer (get-buffer rdebug-buffer-name))
	)

    ;; `gud-rdebug-massage-args' needs whole `command-line'.
    ;; command-line is refered through dyanmic scope.
    (gud-common-init command-line 'gud-rdebug-massage-args
		     'gud-rdebug-marker-filter 'gud-rdebug-find-file)

    ; gud-common-init sets the rdebug process buffer name incorrectly, because
    ; it can't parse the command line properly to pick out the script name.
    ; So we'll do it here and rename that buffer. The buffer we want to rename
    ; happens to be the current buffer.
    (setq gud-target-name (file-name-nondirectory (car script-name-annotate-p)))
    (when rdebug-buffer (kill-buffer rdebug-buffer))
    (rename-buffer rdebug-buffer-name)

    (set (make-local-variable 'gud-minor-mode) 'rdebug)

    (gud-def gud-args   "info args" "a"
	     "Show arguments of current stack.")
    (gud-def gud-break  "break %d%f:%l""\C-b"
	     "Set breakpoint at current line.")
    (gud-def gud-cont   "continue"   "\C-r" 
	     "Continue with display.")
    (gud-def gud-down   "down %p"     ">"
	     "Down N stack frames (numeric arg).")
    (gud-def gud-finish "finish"      "f\C-f"
	     "Finish executing current function.")
    (gud-def gud-next   "next %p"     "\C-n"
	     "Step one line (skip functions).")
    (gud-def gud-print  "p %e"        "\C-p"
	     "Evaluate ruby expression at point.")
    (gud-def gud-remove "clear %d%f:%l" "\C-d"
	     "Remove breakpoint at current line")
    (gud-def gud-run    "run"       "R"
	     "Restart the Ruby script.")
    (gud-def gud-statement "eval %e" "\C-e"
	     "Execute Ruby statement at point.")
    (gud-def gud-step   "step %p"       "\C-s"
	     "Step one source line with display.")
    (gud-def gud-tbreak "tbreak %d%f:%l"  "\C-t"
	     "Set temporary breakpoint at current line.")
    (gud-def gud-up     "up %p"
	     "<" "Up N stack frames (numeric arg).")
    (gud-def gud-where   "where"
	     "T" "Show stack trace.")
    (local-set-key "\C-i" 'gud-gdb-complete-command)
    (setq comint-prompt-regexp "^(rdb:-) ")
    (setq paragraph-start comint-prompt-regexp)
    
    ;; Update GUD menu bar
    (define-key gud-menu-map [args]      '("Show arguments of current stack" . 
					   gud-args))
    (define-key gud-menu-map [down]      '("Down Stack" . gud-down))
    (define-key gud-menu-map [eval]      '("Execute Ruby statement at point" 
					   . gud-statement))
    (define-key gud-menu-map [finish]    '("Finish Function" . gud-finish))
    (define-key gud-menu-map [run]       '("Restart the Ruby Script" . 
					   gud-run))
    (define-key gud-menu-map [stepi]     'undefined)
    (define-key gud-menu-map [tbreak]    '("Temporary break" . gud-tbreak))
    (define-key gud-menu-map [up]        '("Up Stack" . gud-up))
    (define-key gud-menu-map [where]     '("Show stack trace" . gud-where))
    
    (local-set-key [menu-bar debug finish] '("Finish Function" . gud-finish))
    (local-set-key [menu-bar debug up] '("Up Stack" . gud-up))
    (local-set-key [menu-bar debug down] '("Down Stack" . gud-down))
    
    (setq comint-prompt-regexp "^(rdb:-) ")
    (setq paragraph-start comint-prompt-regexp)
    
    (setq paragraph-start comint-prompt-regexp)
    (when rdebug-many-windows (rdebug-setup-windows))
    
    (run-hooks 'rdebug-mode-hook))
  )


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

(defvar rdebug-rdebugtrack-is-tracking-p t)


;; Constants

(defconst rdebug-position-re 
  "\\(\\)\\([-a-zA-Z0-9_/.]*\\):\\([0-9]+\\)"
  "Regular expression for a rdebug position")

(defconst rdebug-traceback-line-re
  "^#[0-9]+[ \t]+\\((\\([a-zA-Z-.]+\\) at (\\(\\([a-zA-Z]:\\)?[^:\n]*\\):\\([0-9]*\\)).*\n"
  "Regular expression that describes tracebacks.")

(defun rdebug-rdebugtrack-overlay-arrow (activation)
  "Activate or de arrow at beginning-of-line in current buffer."
  ;; This was derived/simplified from edebug-overlay-arrow
  (cond (activation
	 (setq overlay-arrow-position (make-marker))
	 (setq overlay-arrow-string "=>")
	 (set-marker overlay-arrow-position (point) (current-buffer))
	 (setq rdebug-rdebugtrack-is-tracking-p t))
	(rdebug-rdebugtrack-is-tracking-p
	 (setq overlay-arrow-position nil)
	 (setq rdebug-rdebugtrack-is-tracking-p nil))
	))

(defun rdebug-rdebugtrack-track-stack-file (text)
  "Show the file indicated by the rdebug stack entry line, in a separate window.
Activity is disabled if the buffer-local variable
`rdebug-rdebugtrack-do-tracking-p' is nil.

We depend on the rdebug input prompt matching `rdebug-rdebugtrack-input-prompt'
at the beginning of the line.
" 
  ;; Instead of trying to piece things together from partial text
  ;; (which can be almost useless depending on Emacs version), we
  ;; monitor to the point where we have the next rdebug prompt, and then
  ;; check all text from comint-last-input-end to process-mark.
  ;;
  ;; Also, we're very conservative about clearing the overlay arrow,
  ;; to minimize residue.  This means, for instance, that executing
  ;; other rdebug commands wipe out the highlight.  You can always do a
  ;; 'where' (aka 'w') command to reveal the overlay arrow.
  (let* ((origbuf (current-buffer))
	 (currproc (get-buffer-process origbuf)))

    (if (not (and currproc rdebug-rdebugtrack-do-tracking-p))
        (rdebug-rdebugtrack-overlay-arrow nil)
      ;else 
      (let* ((procmark (process-mark currproc))
             (block-str (buffer-substring (max comint-last-input-end
                                           (- procmark
                                              rdebug-rdebugtrack-track-range))
                                      procmark))
             target target_fname target_lineno target_buffer)

        (if (not (string-match rdebug-rdebugtrack-input-prompt block-str))
            (rdebug-rdebugtrack-overlay-arrow nil)
	  ;else 
          
          (setq target (rdebug-rdebugtrack-get-source-buffer block-str))

          (if (stringp target)
              (message "rdebugtrack: %s" target)
	    ; else
	    (gud-rdebug-marker-filter block-str)
            (setq target_lineno (car target))
            (setq target_buffer (cadr target))
            (setq target_fname (buffer-file-name target_buffer))
            (switch-to-buffer-other-window target_buffer)
            (goto-line target_lineno)
            (message "rdebugtrack: line %s, file %s" target_lineno target_fname)
            (rdebug-rdebugtrack-overlay-arrow t)
            (pop-to-buffer origbuf t)

            )))))
  )

(defun rdebug-rdebugtrack-get-source-buffer (block-str)
  "Return line number and buffer of code indicated by block-str's traceback 
text.

We look first to visit the file indicated in the trace.

Failing that, we look for the most recently visited python-mode buffer
with the same name or having 
having the named function.

If we're unable find the source code we return a string describing the
problem as best as we can determine."

  (if (not (string-match rdebug-position-re block-str))
      "line number cue not found"
    ;else
    (let* ((filename (match-string rdebug-marker-regexp-file-group block-str))
           (lineno (string-to-number
		    (match-string rdebug-marker-regexp-line-group block-str)))
           funcbuffer)

      (cond ((file-exists-p filename)
             (list lineno (find-file-noselect filename)))

            ((= (elt filename 0) ?\<)
             (format "(Non-file source: '%s')" filename))

            (t (format "Not found: %s" filename)))
      )
    )
  )


;;; Subprocess commands



;; rdebugtrack functions
(defun rdebug-rdebugtrack-toggle-stack-tracking (arg)
  (interactive "P")
  (if (not (get-buffer-process (current-buffer)))
      (error "No process associated with buffer '%s'" (current-buffer)))
  ;; missing or 0 is toggle, >0 turn on, <0 turn off
  (if (or (not arg)
	  (zerop (setq arg (prefix-numeric-value arg))))
      (setq rdebug-rdebugtrack-do-tracking-p (not rdebug-rdebugtrack-do-tracking-p))
    (setq rdebug-rdebugtrack-do-tracking-p (> arg 0)))
  (message "%sabled rdebug's rdebugtrack"
           (if rdebug-rdebugtrack-do-tracking-p "En" "Dis")))

(defun turn-on-rdebugtrack ()
  (interactive)
  (rdebug-rdebugtrack-toggle-stack-tracking 1)
  (setq rdebug-rdebugtrack-is-tracking-p t)
  (add-hook 'comint-output-filter-functions 'rdebug-rdebugtrack-track-stack-file)
  ; remove other py-pdbtrack if which gets in the way
  (remove-hook 'comint-output-filter-functions 'py-pdbtrack-track-stack-file)
  (remove-hook 'comint-output-filter-functions 'py-pydbtrack-track-stack-file)
  (remove-hook 'comint-output-filter-functions 'bashdb-bashdbtrack-track-stack-file)
)

(defun turn-off-rdebugtrack ()
  (interactive)
  (rdebug-rdebugtrack-toggle-stack-tracking 0)
  (setq rdebug-rdebugtrack-is-tracking-p nil)
  (remove-hook 'comint-output-filter-functions 
	       'rdebug-rdebugtrack-track-stack-file) )

;; Add a designator to the minor mode strings if we are tracking
(or (assq 'rdebug-rdebugtrack-minor-mode-string minor-mode-alist)
    (add-to-list 'minor-mode-alist 
		 '(rdebug-rdebugtrack-is-tracking-p
		   rdebug-rdebugtrack-minor-mode-string)))
;; rdebugtrack

;;-----------------------------------------------------------------------------
;; ALB - annotations support
;;-----------------------------------------------------------------------------

(defvar rdebug--annotation-setup-map
  (progn
    (define-hash-table-test 'str-hash 'string= 'sxhash)
    (let ((map (make-hash-table :test 'str-hash)))
      (puthash "breakpoints" 'rdebug--setup-breakpoints-buffer map)
      (puthash "stack" 'rdebug--setup-stack-buffer map)
      (puthash "locals" 'rdebug--setup-locals-buffer map)
      map)))

(defun rdebug-process-annotation (name contents)
  (let ((buf (get-buffer-create (format "*rdebug-%s-%s*" name gud-target-name))))
    (with-current-buffer buf
      (setq buffer-read-only t)
      (let ((inhibit-read-only t)
            (setup-func (gethash name rdebug--annotation-setup-map)))
        (erase-buffer)
        (insert contents)
        (when setup-func (funcall setup-func buf))))))

(defun rdebug-setup-windows ()
  "Layout the window pattern for `rdebug-many-windows'. This was mostly copied
from `gdb-setup-windows', but simplified."
  (pop-to-buffer gud-comint-buffer)
  (let ((script-name gud-target-name))
    (delete-other-windows)
    (split-window nil ( / ( * (window-height) 3) 4))
    (split-window nil ( / (window-height) 3))
    (split-window-horizontally)
    (other-window 1)
    (set-window-buffer 
     (selected-window) 
     (get-buffer-create (format "*rdebug-locals-%s*" script-name)))
    (other-window 1)
    (switch-to-buffer
     (if gud-last-last-frame
	 (gud-find-file (car gud-last-last-frame))
       ;; Put buffer list in window if we
       ;; can't find a source file.
       (list-buffers-noselect)))
    (other-window 1)
    (set-window-buffer 
     (selected-window) 
     (get-buffer-create (format "*rdebug-stack-%s*" script-name)))
    (split-window-horizontally)
    (other-window 1)
    (set-window-buffer 
     (selected-window) 
     (get-buffer-create (format "*rdebug-breakpoints-%s*" script-name)))
    (other-window 1)
    (goto-char (point-max))))
  
(defun rdebug-restore-windows ()
  "Equivalent of `gdb-restore-windows' for rdebug."
  (interactive)
  (when rdebug-many-windows
    (rdebug-setup-windows)))

(defun rdebug-set-windows (&optional name)
  "Sets window used in multi-window frame and issues
rdebug-restore-windows if rdebug-many-windows is set"
  (interactive "sProgram name: ")
  (when name (setq gud-target-name name)
	(setq gud-comint-buffer (current-buffer)))
  (when gud-last-frame (setq gud-last-last-frame gud-last-frame))
  (when rdebug-many-windows
    (rdebug-setup-windows)))

;; Fontification and keymaps for secondary buffers (breakpoints, stack)

;; -- breakpoints

(defvar rdebug--breakpoints-map
  (let ((map (make-sparse-keymap)))
    (define-key map [mouse-2] 'rdebug-goto-breakpoint-mouse)
    ; (define-key map [? ] 'rdebug-toggle-breakpoint)
    (define-key map [(control m)] 'rdebug-goto-breakpoint)
    (define-key map [?d] 'rdebug-delete-breakpoint)
    map)
  "Keymap to navigate/set/enable rdebug breakpoints.")

(defconst rdebug--breakpoint-regexp
  "^\\ +\\([0-9]+\\) +at +\\(.+\\):\\([0-9]+\\)$"
  "Regexp to recognize breakpoint lines in rdebug breakpoint buffers.")

(defun rdebug--setup-breakpoints-buffer (buf)
  "Detects breakpoint lines and sets up mouse navigation."
  (with-current-buffer buf
    (let ((inhibit-read-only t))
      (setq mode-name "RDEBUG Breakpoints")
      (goto-char (point-min))
      (while (not (eobp))
        (let ((b (point-at-bol)) (e (point-at-eol)))
          (when (string-match rdebug--breakpoint-regexp
                              (buffer-substring b e))
            (add-text-properties b e
                                 (list 'mouse-face 'highlight
                                       'keymap rdebug--breakpoints-map))
            (add-text-properties
             (+ b (match-beginning 1)) (+ b (match-end 1))
             (list 'face font-lock-constant-face
                   'font-lock-face font-lock-constant-face))
            (add-text-properties
             (+ b (match-beginning 2)) (+ b (match-end 2))
             (list 'face font-lock-comment-face
                   'font-lock-face font-lock-comment-face))
            (add-text-properties
             (+ b (match-beginning 3)) (+ b (match-end 3))
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
        (beginning-of-line))))))

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
    (let ((s (buffer-substring (point-at-bol) (point-at-eol))))
      (when (string-match rdebug--breakpoint-regexp s)
        (rdebug-display-line
         (substring s (match-beginning 2) (match-end 2))
         (string-to-number (substring s (match-beginning 3) (match-end 3))))
        ))))

;;; (defun rdebug-toggle-breakpoint (pt)
;;;   "Toggles the breakpoint at PT in the breakpoints buffer."
;;;   (interactive "d")
;;;   (save-excursion
;;;     (goto-char pt)
;;;     (let ((s (buffer-substring (point-at-bol) (point-at-eol))))
;;;       (when (string-match rdebug--breakpoint-regexp s)
;;;         (let* ((enabled
;;;                 (string= (substring s (match-beginning 3) (match-end 3)) "y"))
;;;                (cmd (if enabled "disable" "enable"))
;;;                (bpnum (substring s (match-beginning 1) (match-end 1))))
;;;           (gud-call (format "%s %s" cmd bpnum)))))))

(defun rdebug-delete-breakpoint (pt)
  "Deletes the breakpoint at PT in the breakpoints buffer."
  (interactive "d")
  (save-excursion
    (goto-char pt)
    (let ((s (buffer-substring (point-at-bol) (point-at-eol))))
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
    (define-key map "q" 'kill-this-buffer)
    (define-key map [mouse-1] 'rdebug-goto-stack-frame-mouse)
    (define-key map [mouse-2] 'rdebug-goto-stack-frame-mouse)
    (define-key map [(control m)] 'rdebug-goto-stack-frame)
    map)
  "Keymap to navigate rdebug stack frames.")

(defconst rdebug--stack-frame-regexp
  "^\\(-->\\|##\\|  \\) +#\\([0-9]+\\) +\\(.*\\)at line +\\([^:]+\\):\\([0-9]+\\)$"
  "Regexp to recognize stack frame lines in rdebug stack buffers.")

(defun rdebug--setup-stack-buffer (buf)
  "Detects stack frame lines and sets up mouse navigation."
  (with-current-buffer buf
    (let ((inhibit-read-only t))
      (setq mode-name "RDEBUG Stack Frames")
      (goto-char (point-min))
      (while (not (eobp))
        (let* ((b (point-at-bol)) (e (point-at-eol))
               (s (buffer-substring b e)))
          (when (string-match rdebug--stack-frame-regexp s)
            (add-text-properties
             (+ b (match-beginning 2)) (+ b (match-end 2))
             (list 'face font-lock-constant-face
                   'font-lock-face font-lock-constant-face))
            (add-text-properties
             (+ b (match-beginning 4)) (+ b (match-end 4))
             (list 'face font-lock-comment-face
                   'font-lock-face font-lock-comment-face))
            (add-text-properties
             (+ b (match-beginning 5)) (+ b (match-end 5))
             (list 'face font-lock-constant-face
                   'font-lock-face font-lock-constant-face))
            (when (string= (substring s (match-beginning 1) (match-end 1)) 
			 "-->")
                ;; highlight the currently selected frame
                (add-text-properties b e
                                     (list 'face 'bold
                                           'font-lock-face 'bold))
		(setq overlay-arrow-position (make-marker))
		(set-marker overlay-arrow-position (point))
		(setq frame-point (point)))
	    (add-text-properties b e
				 (list 'mouse-face 'highlight
				       'keymap rdebug-frames-mode-map))
	    (let ((fn-str (substring s (match-beginning 3) (match-end 3)))
		  (fn-start (+ b (match-beginning 3))))
	      (if (string-match "\\([^(]+\\)(" fn-str)
		  (add-text-properties
		   (+ fn-start (match-beginning 1)) (+ fn-start (match-end 1))
		   (list 'face font-lock-function-name-face
			 'font-lock-face font-lock-function-name-face))))))
	;; remove initial ###  or -->
	(beginning-of-line)
	(delete-char 3)
        (forward-line)
        (beginning-of-line)))))

(defun rdebug-goto-stack-frame (pt)
  "Show the rdebug stack frame corresponding at PT in the rdebug stack buffer."
  (interactive "d")
  (save-excursion
    (goto-char pt)
    (let ((s (buffer-substring (point-at-bol) (point-at-eol))))
      (when (string-match rdebug--stack-frame-regexp s)
        (let ((frame (substring s (match-beginning 2) (match-end 2))))
          (gud-call (concat "frame " frame)))))))

(defun rdebug-goto-stack-frame-mouse (event)
  "Show the rdebug stack frame under the mouse in the rdebug stack buffer."
  (interactive "e")
  (with-current-buffer (window-buffer (posn-window (event-end event)))
    (rdebug-goto-stack-frame (posn-point (event-end event)))))

;; -- locals

(defvar rdebug-locals-mode-map
  (let ((map (make-sparse-keymap)))
    (suppress-keymap map)
    (define-key map "\r" 'rdebug-edit-locals-value)
    (define-key map "e" 'rdebug-edit-locals-value)
    (define-key map [mouse-2] 'rdebug-edit-locals-value)
    (define-key map "q" 'kill-this-buffer)
     map))

(defun rdebug-locals-mode ()
  "Major mode for rdebug locals.

\\{rdebug-locals-mode-map}"
  ; (kill-all-local-variables)
  (setq major-mode 'rdebug-locals-mode)
  (setq mode-name "RDEBUG Locals")
  (setq buffer-read-only t)
  (use-local-map rdebug-locals-mode-map)
  ; (set (make-local-variable 'font-lock-defaults)
  ;     '(gdb-locals-font-lock-keywords))
  (run-mode-hooks 'rdebug-locals-mode-hook))

(defun rdebug--setup-locals-buffer (buf)
  (with-current-buffer buf
    (rdebug-locals-mode)))

(defun rdebug-edit-locals-value (&optional event)
  "Assign a value to a variable displayed in the locals buffer."
  (interactive (list last-input-event))
  (save-excursion
    (if event (posn-set-point (event-end event)))
    (beginning-of-line)
    (let* ((var (current-word))
	   (value (read-string (format "New value (%s): " var))))
      (gud-call (format "p %s=%s" var value)))))

;;-----------------------------------------------------------------------------
;; ALB - redefinition of gud-reset for our own purposes

(defvar rdebug--orig-gud-reset (symbol-function 'gud-reset))

(defun gud-reset ()
  "Redefinition of `gud-reset' to take care of rdebug cleanup."
  (funcall rdebug--orig-gud-reset)
  (dolist (buffer (buffer-list))
    (when (string-match "\\*rdebug-[a-z]+\\*" (buffer-name buffer))
      (let ((w (get-buffer-window buffer)))
        (when w (delete-window w)))
      (kill-buffer buffer))))


(provide 'rdebug)


