;;; rdebug-core.el --- Ruby debugger user interface.

;; Copyright (C) 2006, 2007, 2008 Rocky Bernstein (rocky@gnu.org)
;; Copyright (C) 2007, 2008 Anders Lindgren

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

;; -------------------------------------------------------------------
;; Consistency checks.
;;

(if (< emacs-major-version 22)
    (error
     "This version of rdebug.el needs at least Emacs 22 or greater - you have version %d."
     emacs-major-version))


;; -------------------------------------------------------------------
;; Dependencies.
;;

(require 'gud)
(require 'cl)

(require 'rdebug)
(require 'rdebug-dbg)
(require 'rdebug-gud)
(require 'rdebug-layouts)
(require 'rdebug-source)
(require 'rdebug-regexp)
(require 'rdebug-vars)


;; -------------------------------------------------------------------
;; Interface to gud.
;;

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
    (rdebug-debug-message "TOT: %S" string)
    (let ((output "")		    ; Output to debugger shell window.
          (tmp ""))

      (while (string-match rdebug-annotation-start-regexp gud-marker-acc)
        (rdebug-debug-message "ACC: %S" gud-marker-acc)
        (if (not (equal (match-beginning 0) 0))
            ;; Spawn off plain text going to the debugger shell window.
            (progn
              (setq output (concat output (substring gud-marker-acc
                                                     0 (match-beginning 0))))
              (setq gud-marker-acc (substring gud-marker-acc
                                              (match-beginning 0))))
          (let* ((s (match-beginning 0))
                 (end (match-end 0))
                 (name (or (match-string 2 gud-marker-acc)
                           "source"))
                 (end-regexp (cond ((string= name "starting")
                                    "\\(stopped\\|exited\\)\n")
                                   ((string= name "pre-prompt")
                                    ;; TODO: The extra "\n" is probably
                                    ;; a bug in processor.rb.
                                    "\nprompt\n")
                                   ((string= name "source")
                                    "\n")
                                   (t rdebug-annotation-end-regexp))))
            (if (string-match end-regexp gud-marker-acc end)
                ;; ok, annotation complete, process it and remove it
                (let ((contents (substring
                                 gud-marker-acc end (match-beginning 0)))
                      (end2 (match-end 0)))
                  (cond ((string= name "pre-prompt")
                         (setq output (concat output contents)))
                        ((string= name "source")
                         (if (string-match gud-rdebug-marker-regexp
                                           gud-marker-acc)
                             ;; Extract the frame position from the marker.
                             (setq gud-last-frame
                                   (cons (match-string 1 gud-marker-acc)
                                         (string-to-number
                                          (match-string 2 gud-marker-acc))))))
                        (t (rdebug-process-annotation name contents)))
                  (setq gud-marker-acc
                        (concat (substring gud-marker-acc 0 s)
                                (substring gud-marker-acc end2))))
              ;; otherwise, save the partial annotation to a temporary,
              ;; and re-add it to gud-marker-acc after normal output has
              ;; been processed
              (setq tmp (substring gud-marker-acc s))
              (setq gud-marker-acc (substring gud-marker-acc 0 s))))))

      ;; TODO: end -> start?
      (when (string-match rdebug-annotation-end-regexp gud-marker-acc)
	;; save the beginning of gud-marker-acc to tmp, remove it and
	;; restore it after normal output has been processed
	(setq tmp (substring gud-marker-acc 0 (match-beginning 0)))
	(setq gud-marker-acc (substring gud-marker-acc (match-beginning 0))))

      ;; Display the source file where we want it, gud will only pick
      ;; an arbitrary window.
      (if gud-last-frame
          (rdebug-pick-source-window))

      (rdebug-local-short-key-mode-on)

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
	(setq output (concat output gud-marker-acc))
        (setq gud-marker-acc tmp))

      (rdebug-debug-message "REM: %S" gud-marker-acc)

      output)))

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
     ((equal "-emacs" arg)
      (rdebug-get-script-name (cdr args) t))
     ((member arg '("-h" "--host" "-p" "--port"
		    "-I" "--include" "-r" "--require"))
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
                        (with-no-warnings
                          (split-string-and-unquote (substring string (cdr rfs))
                                                    sep))))))))
  )


;; -------------------------------------------------------------------
;; Window configuration state support.
;;

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


;; have to bind rdebug-file-queue before installing the kill-emacs-hook
(defvar rdebug-file-queue nil
  "Queue of Makefile temp files awaiting execution.
Currently-active file is at the head of the list.")


;; -------------------------------------------------------------------
;; Annotation and secondary buffers.
;;

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

(defvar rdebug-current-line-number 1
  "The line number in a secondary window that you were in. We need to save
  this value because secondary windows get recreated a lot")

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
      (if (and (string= name "output") (not rdebug-use-separate-io-buffer))
	  (insert contents)
        ;;else
	(with-current-buffer buf
	  (setq buffer-read-only t)
	  (let ((inhibit-read-only t)
		(setup-func (gethash name rdebug--annotation-setup-map)))
	    (set (make-local-variable 'rdebug-current-line-number) 
		 (line-number-at-pos))
	    (if (string= name "output")
		(goto-char (point-max))
	      (erase-buffer))
	    (insert contents)
	    (when setup-func (funcall setup-func buf comint-buffer))))))))


;; -------------------------------------------------------------------
;; Windows.
;;

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


(defun rdebug-restore-windows ()
  "Display the initial ruby debugger window layout."
  (interactive)
  (when rdebug-many-windows
    (rdebug-setup-windows)))

(defun rdebug-display-debugger-window-configuration ()
  "Display the current layout of windows of the rdebug Ruby debugger.
See also `rdebug-display-original-window-configuration'"
  (interactive)
  (rdebug-set-window-configuration-state 'debugger)
  (message
   "Type `M-x rdebug-display-original-window-configuration RET' to restore."))

(defun rdebug-display-original-window-configuration ()
  "Display the layout of windows prior to starting the rdebug Ruby debugger.

This function is called upon quitting the debugger and
`rdebug-many-windows' is not nil. See also
`rdebug-display-debugger-window-configuration'."
  (interactive)
  (rdebug-set-window-configuration-state 'original)
  (message
   "Type `M-x rdebug-display-debugger-window-configuration RET' to restore."))


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

;; -------------------------------------------------------------------
;; Secondary buffers.
;;

(require 'rdebug-secondary)
(require 'rdebug-breaks)
(require 'rdebug-frames)
(require 'rdebug-help)
(require 'rdebug-output)
(require 'rdebug-varbuf)
(require 'rdebug-watch)


;; -------------------------------------------------------------------
;; Source short key mode.
;;
;; When this minor mode is active and the debugger is running, the
;; source window displaying the current debugger frame is marked as
;; read-only and the short keys of the secondary windows can be used,
;; for example, you can use the space-bar to single-step the program.

;; Implementation note:
;;
;; This is presented to the user as one global minor mode. However,
;; under the surface the real work is done by another, non-global,
;; minor mode named "local short key mode". This is activated and
;; deactivated appropriately by the Rdebug filter functions.

(defvar rdebug-local-short-key-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "b" 'gud-break)
    (define-key map "t" 'rdebug-toggle-source-breakpoint-enabled)
    (define-key map [insert] 'rdebug-local-short-key-mode-off)
    (define-key map "p" 'gud-print)
    (rdebug-populate-secondary-buffer-map-plain map)
    map)
  "Keymap used in `rdebug-local-short-key-mode'.")


;; Implementation node: This is the mode that does all the work, it's
;; local to the buffer that is affected.
(define-minor-mode rdebug-local-short-key-mode
  "Minor mode with short keys for source buffers for the `rdebug' debugger.
The buffer is read-only when the minor mode is active.

Please use the global `rdebug-short-key-mode' to automatically
activate this mode when the `rdebug' debugger is used.

\\{rdebug-local-short-key-mode-map}"
  :group 'rdebug
  :global nil
  :init-value nil
  :keymap rdebug-local-short-key-mode-map
  (if rdebug-local-short-key-mode
      (setq buffer-read-only t)
    (setq buffer-read-only nil)))


(defun rdebug-turn-on-short-key-mode ()
  "Turn on `rdebug-short-key-mode'.

This function is designed to be used in a user hook, for example:

    (add-hook 'rdebug-mode-hook 'rdebug-turn-on-short-key-mode)"
  (interactive)
  (rdebug-short-key-mode 1))


(defun rdebug-short-key-mode-maybe-activate ()
  (if rdebug-short-key-mode
      (rdebug-local-short-key-mode-on)
    (rdebug-local-short-key-mode-off)))


(defun rdebug-local-short-key-mode-off ()
  "Turn off `rdebug-local-short-key-mode' in all buffers."
  (interactive)
  (rdebug-debug-enter "rdebug-local-short-key-mode-off"
    (save-current-buffer
      (dolist (buf (buffer-list))
        (set-buffer buf)
        (if rdebug-local-short-key-mode
            (rdebug-local-short-key-mode -1))))))

(defun rdebug-buffer-killed-p (buffer)
  "Return t if BUFFER is killed."
  (not (buffer-name buffer)))

(defun rdebug-local-short-key-mode-on ()
  "Turn on `rdebug-local-short-key-mode' in the current debugger frame."
  (interactive)
  (rdebug-debug-enter "rdebug-local-short-key-mode-on"
    (save-current-buffer
      (if (and gud-comint-buffer
               (not (rdebug-buffer-killed-p gud-comint-buffer)))
          (set-buffer gud-comint-buffer))
      (let ((frame (or gud-last-frame
                       gud-last-last-frame)))
        (if (and frame
                 rdebug-short-key-mode)
            (ignore-errors
              ;; `gud-find-file' calls `error' if it doesn't find the file.
              (let ((buffer (gud-find-file (car frame))))
                (save-current-buffer
                  (set-buffer buffer)
                  (rdebug-local-short-key-mode 1)))))))))


;; -------------------------------------------------------------------
;; The `rdebug' command and support functions.
;;

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
      (rdebug-local-short-key-mode-off)
      (rdebug-set-window-configuration-state 'original)
      ;; This unbinds the special debugger keys of the source buffers.
      (setcdr (assq 'rdebug-debugger-support-minor-mode minor-mode-map-alist)
              rdebug-debugger-support-minor-mode-map-when-deactive))))


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


;; Common setup to `rdebug' and `rdebug-set-windows' (the shell entry point).
(defun rdebug-common-initialization ()
  "Common initialization to `rdebug' and `rdebug-set-windows'."

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
  (gud-def gud-print  "p %e"        "\C-p"
           "Evaluate Ruby expression at point.")
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
  (local-set-key "\C-i" 'gud-gdb-complete-command))


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
    (let* ((words (with-no-warnings
                    (split-string-and-unquote command-line)))
           (script-name-annotate-p (rdebug-get-script-name
                                    (gud-rdebug-massage-args "1" words) nil))
           (target-name (file-name-nondirectory (car script-name-annotate-p)))
           (annotate-p (cadr script-name-annotate-p))
           (rdebug-buffer-name (format "*rdebug-cmd-%s*" target-name))
           (rdebug-buffer (get-buffer rdebug-buffer-name)))

      ;; `gud-rdebug-massage-args' needs whole `command-line'.
      ;; command-line is refered through dynamic scope.
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

      (rdebug-common-initialization)

      ;; Setup exit callback so that the original frame configuration
      ;; can be restored.
      (let ((process (get-buffer-process gud-comint-buffer)))
        (when process
          (unless (equal rdebug-line-width 120)
	    (gud-call (format "set width %d" rdebug-line-width)))
          (set-process-sentinel process
                                'rdebug-process-sentinel)))


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
      (when rdebug-many-windows
        (rdebug-setup-windows-initially))

      (run-hooks 'rdebug-mode-hook))))


;; Implementation note: If Emacs could talk directly to the Ruby
;; debugger, this would be rougly "Debugger.breakpoints". Since we
;; currently can't do that we parse the content of the breakpoints
;; window.
;;
;; Note: The :function kind is not yet implemented.
(defun rdebug-all-breakpoints ()
  "Return a list of all breakpoints.

Each entry in the list is on the form:

    (:file number enabled file line)

or

    (:function number enabled class function)"
  (let* ((target-name (or (and gud-comint-buffer
                               (buffer-local-value 'gud-target-name
                                                   gud-comint-buffer))
                          gud-target-name))
         (buf (rdebug-get-buffer "breakpoints" target-name)))
    (and buf
         (save-current-buffer
           (set-buffer buf)
           (save-excursion
             (let ((res '()))
               (goto-char (point-min))
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
               (nreverse res)))))))


(defun rdebug-breakpoints-on-line (file line)
  "Return a list of the breakpoint on the current source line."
  (let ((res '()))
    (dolist (entry (rdebug-all-breakpoints))
      (if (and (eq (nth 0 entry) :file)
               (string= (nth 3 entry) file)
               (equal (nth 4 entry) line))
          (push entry res)))
    res))


(defun rdebug-file-and-line-arg ()
  (save-excursion
    (beginning-of-line)
    (list (buffer-file-name) (+ 1 (count-lines (point-min) (point))))))

(defun rdebug-toggle-source-breakpoint (file line)
  "Toggle break point on current source line."
  (interactive (rdebug-file-and-line-arg))
  (let ((bps (rdebug-breakpoints-on-line file line)))
    (if bps
        (gud-call (format "delete %s" (nth 1 (car bps))))
      (gud-call (format "break %s:%d" file line)))))


(defun rdebug-toggle-source-breakpoint-enabled (file line)
  "Enable or disable a break point on the current source line."
  (interactive (rdebug-file-and-line-arg))
  (let ((bps (rdebug-breakpoints-on-line file line)))
    (if bps
        ;; Note: If the line contains more than one simply use the
        ;; first one.
        (let ((entry (car bps)))
          (if (nth 2 entry)
              (gud-call (format "disable %s" (nth 1 entry)))
            (gud-call (format "enable %s" (nth 1 entry)))))
      (gud-call (format "break %s:%d" file line)))))


(defun rdebug-customize ()
  "Use `customize' to edit the settings of the `rdebug' debugger."
  (interactive)
  (customize-group 'rdebug))


;; -------------------------------------------------------------------
;; The end.
;;

(provide 'rdebug-core)

;;; Local variables:
;;; eval:(put 'rdebug-debug-enter 'lisp-indent-hook 1)
;;; End:

;;; rdebug-core.el ends here
