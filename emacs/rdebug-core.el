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

;; See the manual and the file `rdebug.el' for more information.

;; This file implements the core of the debugger.

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

;; Should go somehwere else
(defun chomp(string)
  "Remove trailing \n if it's there"
  (let ((s string)) 
    (if (string= "\n" (substring s -1)) 
	(substring s 0 -1)
      s)))

;; -------------------------------------------------------------------
;; Interface to gud.
;;

(defvar rdebug-non-annotated-text-is-output nil)

;; Problem: What happens if we receive "^Z^Zfoo\n^Z". Should we wait
;; until we know that the trailing ^Z is part of a new annotation or
;; not?

(defun rdebug-marker-filter-next-item (string)
  "The next item for the rdebug marker filter to process.

Return (item . rest) or nil."
  (rdebug-debug-message "ACC: %S" string)
  (cond
   ;; Empty line, we're done.
   ((equal (length string) 0)
    nil)
   ;; A single ^Z, this could become a new annotation, so lets stop here.
   ((string= string "\032")
    nil)
   (t
    (let ((split-point
           (cond ((string-match "\032\032" string)
                  (let ((beg (match-beginning 0)))
                    (if (equal beg 0)
                        (if (string-match "\032\032" string 3)
                            (match-beginning 0)
                          (length string))
                      beg)))
                 ((eq (elt string (- (length string) 1)) ?\32)
                  -1)
                 (t
                  (length string)))))
      (cons (substring string 0 split-point) (substring string split-point))))))


;; There's no guarantee that Emacs will hand the filter the entire
;; marker at once; it could be broken up across several strings.  We
;; might even receive a big chunk with several markers in it.  If we
;; receive a chunk of text which looks like it might contain the
;; beginning of a marker, we save it here between calls to the
;; filter.
(defun gud-rdebug-marker-filter (string)
  "Filter function for process output of the rdebug Ruby debugger."
  (rdebug-debug-enter "gud-rdebug-marker-filter:"
    (rdebug-debug-message "GOT: %S" string)
    (if rdebug-non-annotated-text-is-output
        (rdebug-debug-message "  Output is default"))
    (setq gud-marker-acc (concat gud-marker-acc string))
    (rdebug-debug-message "TOT: %S" gud-marker-acc)
    (let ((shell-output "")         ; Output to debugger shell window.
          (done nil)
          item)
      ;; The following loop peels of one "item" at a time. An item is
      ;; a un-annotated section or an annotation. (This is taken care
      ;; of by the `rdebug-marker-filter-next-item' function.)
      ;;
      ;; An Annotation can be a one-liner (where anything following
      ;; the annotation is treated as un-annotated text) or a full
      ;; annotation (which stretches to the next annotation).
      ;;
      ;; The concept of one-liners (no phun intended) is to allow
      ;; continuous output, a "starting" annotation simply sets up the
      ;; environment for sending lines to the output window, any text
      ;; following it right now, or in later chunks of data, is
      ;; redirected to the output window.
      (while (and (not done)
                  (let ((pair (rdebug-marker-filter-next-item gud-marker-acc)))
                    (rdebug-debug-message "Next item: %S" pair)
                    (and pair
                         (progn
                           (setq item (car pair))
                           (setq gud-marker-acc (cdr pair))
                           t))))
        ;; Note: Regexp:s are greedy, i.e. the char parts wins over
        ;; the .* part.
        (if (not (string-match "^\032\032\\([-a-z]*\\).*\n" item))
            ;; Non-annotated text (or the content of one-liners) goes
            ;; straight into the debugger shell window, or to the
            ;; output window.
            (if (and rdebug-non-annotated-text-is-output
                     rdebug-use-separate-io-buffer)
                (rdebug-process-annotation "starting" item)
              (setq shell-output (concat shell-output item)))
          ;; Handle annotation.
          (let* ((line-end (match-end 0))
                 (name (match-string 1 item))
                 ;; "prompt" is needed to handle "quit" in the shell correctly.
                 (one-liner
                  (member name
                          '("" "exited" "source" "prompt" "starting")))
                 (next-annotation (string-match "\032\032"
                                                gud-marker-acc)))
            ;; For one-liners, shuffle some text back to the accumulator.
            (when one-liner
              (setq gud-marker-acc (concat (substring item line-end)
                                           gud-marker-acc))
              (setq item (substring item 0 line-end)))
            (if (or next-annotation
                    one-liner)
                ;; ok, annotation complete, process it and remove it
                (let* ((contents (substring item line-end)))
                  (rdebug-debug-message "Name: %S Content: %S" name contents)
                  ;; This is a global state flag, this allows us to
                  ;; redirect any further text to the output buffer.
                  (set
                   (make-local-variable 'rdebug-non-annotated-text-is-output)
                   (string= name "starting"))
                  (cond ((string= name "pre-prompt")
                         ;; Strip of the trailing \n (this is probably
                         ;; a bug in processor.rb).
                         (if (string= (substring contents -1) "\n")
                             (setq contents (substring contents 0 -1)))
                         (setq shell-output (concat shell-output contents)))
                        ((string= name "source")
                         (if (string-match gud-rdebug-marker-regexp item)
                             ;; Extract the frame position from the marker.
                             (setq gud-last-frame
                                   (cons (match-string 1 item)
                                         (string-to-number
                                          (match-string 2 item))))))
                        (t (rdebug-process-annotation name contents))))
              ;; This is not a one-liner, and we haven't seen the next
              ;; annotation, so we have to treat this as a partial
              ;; annotation. Save it and hope that the we can process
              ;; it the next time we're called.
              (setq gud-marker-acc (concat item gud-marker-acc))
              (setq done t)))))

      ;; Display the source file where we want it, gud will only pick
      ;; an arbitrary window.
      (if gud-last-frame
          (rdebug-pick-source-window))

      (rdebug-internal-short-key-mode-on)

      (unless (string= shell-output "")
        (rdebug-debug-message "Output: %S" shell-output))
      (rdebug-debug-message "REM: %S" gud-marker-acc)

      shell-output)))

(defun rdebug-get-script-name (args)
  "Pick out the script name from the command line.
Return a list of that and whether the annotate option was set.
Initially annotate should be set to nil."
  ;; Parse the following:
  ;;
  ;;  [ruby ruby-options] rdebug rdebug-options script-name script-options
  (and args
       (let ((name nil)
             (annotate-p nil))
         ;; Strip of optional "ruby" or "ruby182" etc.
         (when (string-match "^ruby[0-9]*$"
                             (file-name-sans-extension
                              (file-name-nondirectory (car args))))
           (pop args)
           (while (and args
                       (string-match "^-" (car args)))
             (if (member (car args) '("-e" "-r" "-I" "-C" "-F" "-K"))
                 (pop args))
             (pop args)))
         ;; Remove "rdebug" from "rdebug --rdebug-options script
         ;; --script-options"
         (pop args)
         ;; Skip to the first non-option argument.
         (while (and args
                     (not name))
           (let ((arg (pop args)))
             (cond
              ;; Annotation or emacs option with level number.
              ((or (member arg '("--annotate" "-A"))
		   (equal arg "--emacs"))
               (setq annotate-p t)
               (pop args))
              ;; Combined annotation and level option.
              ((string-match "^--annotate=[0-9]" arg)
               (setq annotate-p t))
              ;; Options with arguments.
              ((member arg '("-h" "--host" "-p" "--port"
                             "-I" "--include" "-r" "--require"))
               (pop args))
              ((string-match "^-" arg)
               nil)
              (t
               (setq name arg)))))
         (and name
              (list name annotate-p)))))

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
      (puthash "frame"       'rdebug--setup-frame-buffer                 map)
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
           (setq name "watch"))
          ((string= name "stack")
           (setq name "frame"))
          ((string= name "error-begin")
           (setq name "error")))
    (let ((buf (get-buffer-create
                (format "*rdebug-%s-%s*" name gud-target-name)))
          ;; Buffer local, doesn't survive the buffer change.
          (comint-buffer gud-comint-buffer))
;;; FIXME: figure out how to get rdebug-use-separate-io-buffer working
;;;       (if (and (string= name "output") (not rdebug-use-separate-io-buffer))
;;; 	  (insert contents)
      ;;else
      (with-current-buffer buf
	(setq buffer-read-only t)
	(let ((inhibit-read-only t)
	      (setup-func (gethash name rdebug--annotation-setup-map)))
	  (set (make-local-variable 'rdebug-current-line-number) 
	       (line-number-at-pos))
	  (cond ((string= name "output")
		 (goto-char (point-max)))
		((string= name "error")
		 (goto-char (point-max))
		 (message (chomp contents)))
		(t (erase-buffer)))
	  (insert contents)
	  (when setup-func (funcall setup-func buf comint-buffer)))))))

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
       (unless (rdebug-get-buffer name gud-target-name)
         (rdebug-process-annotation name "")))
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


(defun rdebug-restore-debugger-window-layout ()
  "Restore the initial ruby debugger window layout."
  (interactive)
  (when rdebug-many-windows
    (rdebug-setup-windows)))

(defun rdebug-display-debugger-window-configuration ()
  "Switch from the \"original\" to the \"debugger\" window layout.

The rdebug debugger remembers, and can switch between, two window layouts:
 * original -- the window layout when the debugger was started.
 * debugger -- the window layout of the debugger, plus all changes made
               since the debugger started.

The check-marks in the \"Window Layout\" menu indicates the
active window layout.

The function `rdebug-display-original-window-configuration'
switch to the \"original\" window configuration.

The function `rdebug-restore-debugger-window-layout' restores the
window layout to the state it was when the debugger started."
  (interactive)
  (rdebug-set-window-configuration-state 'debugger)
  (message
   "Type `M-x rdebug-display-original-window-configuration RET' to restore."))


;;This function is called upon quitting the debugger and
;;`rdebug-many-windows' is not nil. See also
;;`rdebug-display-debugger-window-configuration'."

(defun rdebug-display-original-window-configuration ()
  "Switch from the \"debugger\" to the \"original\" window layout.

The rdebug debugger remembers, and can switch between, two window layouts:
 * original -- the window layout when the debugger was started.
 * debugger -- the window layout of the debugger, plus all changes made
               since the debugger started.

The check-marks in the \"Window Layout\" menu indicates the
active window layout.

The function `rdebug-display-debugger-window-configuration'
switch to the \"debugger\" window configuration."
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
(require 'rdebug-error)
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

(defvar rdebug-internal-short-key-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "b" 'gud-break)
    (define-key map "t" 'rdebug-toggle-source-breakpoint-enabled)
    (define-key map [insert] 'rdebug-short-key-mode)
    (define-key map "p" 'gud-print)
    (rdebug-populate-secondary-buffer-map-plain map)
    map)
  "Keymap used in `rdebug-internal-short-key-mode'.")

(defvar rdebug-original-read-only :off
  "The value `buffer-read-only' should be restored to after short key mode.

When :off, the mode is not active.")

;; Implementation note: This is the mode that does all the work, it's
;; local to the buffer that is affected.
(define-minor-mode rdebug-internal-short-key-mode
  "Minor mode with short keys for source buffers for the `rdebug' debugger.
The buffer is read-only when the minor mode is active.

Note that this is for internal use only, please use the global
mode `rdebug-short-key-mode'.

\\{rdebug-internal-short-key-mode-map}"
  :group 'rdebug
  :global nil
  :init-value nil
  :lighter " ShortKeys"
  :keymap rdebug-internal-short-key-mode-map
  (make-local-variable 'rdebug-original-read-only)
  ;; Note, without the third state, :off, activating the mode more
  ;; than once would overwrite the real original value.
  (if rdebug-internal-short-key-mode
      (progn
        (if (eq rdebug-original-read-only :off)
            (setq rdebug-original-read-only buffer-read-only))
        (setq buffer-read-only t))
    (setq buffer-read-only rdebug-original-read-only)
    (setq rdebug-original-read-only :off)))


(defun rdebug-turn-on-short-key-mode ()
  "Turn on `rdebug-short-key-mode'.

This function is designed to be used in a user hook, for example:

    (add-hook 'rdebug-mode-hook 'rdebug-turn-on-short-key-mode)"
  (interactive)
  (rdebug-short-key-mode 1))


(defun rdebug-short-key-mode-maybe-activate ()
  (if rdebug-short-key-mode
      (rdebug-internal-short-key-mode-on)
    (rdebug-internal-short-key-mode-off)))


(defun rdebug-internal-short-key-mode-off ()
  "Turn off `rdebug-internal-short-key-mode' in all buffers."
  (rdebug-debug-enter "rdebug-internal-short-key-mode-off"
    (save-current-buffer
      (dolist (buf (buffer-list))
        (set-buffer buf)
        (if rdebug-internal-short-key-mode
            (rdebug-internal-short-key-mode -1))))))

(defun rdebug-buffer-killed-p (buffer)
  "Return t if BUFFER is killed."
  (not (buffer-name buffer)))

(defun rdebug-internal-short-key-mode-on ()
  "Turn on `rdebug-internal-short-key-mode' in the current debugger frame."
  (rdebug-debug-enter "rdebug-internal-short-key-mode-on"
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
                  (rdebug-internal-short-key-mode 1)))))))))


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
      (rdebug-internal-short-key-mode-off)
      (rdebug-set-window-configuration-state 'original)
      ;; This unbinds the special debugger keys of the source buffers.
      (setcdr (assq 'rdebug-debugger-support-minor-mode minor-mode-map-alist)
              rdebug-debugger-support-minor-mode-map-when-deactive))))

(defun rdebug-command-initialization ()
  "Initialization of command buffer common to `rdebug' and
`rdebug-track-attach'."

  ;; This opens up "Gud" menu, which isn't used since we've got our
  ;; own "Debugger" menu.

  ;; (set (make-local-variable 'gud-minor-mode) 'rdebug)

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
| Stack Frame buffer                | Breakpoints buffer               |
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
   (let ((init (buffer-file-name)))
     (setq init (and init
                     (file-name-nondirectory init)))
     (list (gud-query-cmdline 'rdebug init))))
  (rdebug-debug-enter "rdebug"
    (rdebug-set-window-configuration-state 'debugger t)
    ;; Parse the command line and pick out the script name and whether
    ;; --annotate has been set.
    (let* ((words (with-no-warnings
                    (split-string-and-unquote command-line)))
           (script-name-annotate-p (rdebug-get-script-name
                                    (gud-rdebug-massage-args "1" words)))
           (target-name (file-name-nondirectory (car script-name-annotate-p)))
           (annotate-p (cadr script-name-annotate-p))
           (rdebug-buffer-name (format "*rdebug-cmd-%s*" target-name))
           (rdebug-buffer (get-buffer rdebug-buffer-name))
           (gud-chdir-before-run nil))

      ;; `gud-rdebug-massage-args' needs whole `command-line'.
      ;; command-line is refered through dynamic scope.
      (gud-common-init command-line 'gud-rdebug-massage-args
                       'gud-rdebug-marker-filter 'gud-rdebug-find-file)
      (setq comint-process-echoes t)

      ;; gud-common-init sets the rdebug process buffer name
      ;; incorrectly, because it can't parse the command line properly
      ;; to pick out the script name. So we'll do it here and rename
      ;; that buffer. The buffer we want to rename happens to be the
      ;; current buffer.
      (setq gud-target-name target-name)
      (when rdebug-buffer (kill-buffer rdebug-buffer))
      (rename-buffer rdebug-buffer-name)

      (rdebug-command-initialization)

      ;; Setup exit callback so that the original frame configuration
      ;; can be restored.
      (let ((process (get-buffer-process gud-comint-buffer)))
        (when process
          (unless (equal rdebug-line-width 120)
	    (gud-call (format "set width %d" rdebug-line-width)))
          (set-process-sentinel process
                                'rdebug-process-sentinel)))


      ;; Add the buffer-displaying commands to the Gud buffer,
      ;; FIXME: combine with code in rdebug-track.el; make common 
      ;; command buffer mode map.
      (let ((prefix-map (make-sparse-keymap)))
        (define-key (current-local-map) gud-key-prefix prefix-map)
	(define-key prefix-map "t" 'rdebug-goto-traceback-line)
	(define-key prefix-map "!" 'rdebug-goto-dollarbang-traceback-line)
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
  (cond ((eq major-mode 'rdebug-breakpoints-mode)
         (rdebug-delete-breakpoint))
        ((null file)
         ;; Do nothing.
         )
        (t
         (let ((bps (rdebug-breakpoints-on-line file line)))
           (if bps
               (gud-call (format "delete %s" (nth 1 (car bps))))
             (gud-call (format "break %s:%d" file line)))))))


(defun rdebug-toggle-source-breakpoint-enabled (file line)
  "Enable or disable a break point on the current source line."
  (interactive (rdebug-file-and-line-arg))
  (cond ((eq major-mode 'rdebug-breakpoints-mode)
         (rdebug-toggle-breakpoint))
        ((null file)
         ;; Do nothing.
         )
        (t
         (let ((bps (rdebug-breakpoints-on-line file line)))
           (if bps
               ;; Note: If the line contains more than one simply use the
               ;; first one.
               (let ((entry (car bps)))
                 (if (nth 2 entry)
                     (gud-call (format "disable %s" (nth 1 entry)))
                   (gud-call (format "enable %s" (nth 1 entry)))))
             (gud-call (format "break %s:%d" file line)))))))


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
