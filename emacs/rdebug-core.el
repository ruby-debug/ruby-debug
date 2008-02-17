;;; rdebug-core.el --- Core parts of the Ruby debugger user
;;; interface. It pulls in other parts of the debugger.

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

;;; Code:

;; -------------------------------------------------------------------
;; Consistency checks.
;;

(if (< emacs-major-version 22)
    (error
     "This version of rdebug.el needs at least Emacs 22 or greater - you have version %d"
     emacs-major-version))


;; -------------------------------------------------------------------
;; Dependencies.
;;

(require 'gud)
(require 'cl)

(require 'rdebug)
(require 'rdebug-annotate)
(require 'rdebug-dbg)
(require 'rdebug-cmd)
(require 'rdebug-layouts)
(require 'rdebug-source)
(require 'rdebug-regexp)
(require 'rdebug-vars)

(defun rdebug-get-script-name (args)
  "Pick out the script name from the command line.
Return a list of that and whether the annotate option was set.
Initially annotate should be set to nil.
Argument ARGS contains a tokenized list of the command line."
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
;; Windows.
;;

(defun rdebug-setup-windows (&optional erase)
  "Create the debugger user interface window layout.

If ERASE is non-nil, the content of the windows are erased
\(this does not apply to accumulative windows).

This function displays the source file (or, in some cases, a
buffer list) and creates the window layout.  The variable
`rdebug-window-layout-function' controls the function that is
used to perform the actual layout.

This is only used when `rdebug-many-windows' is non-nil."
  (rdebug-debug-enter "rdebug-setup-windows"
    (rdebug-set-window-configuration-state 'debugger t)
    (pop-to-buffer gud-comint-buffer)
    (maphash
     (lambda (name func)
       (if erase
           (let ((buf (rdebug-get-existing-buffer name gud-target-name)))
             (if buf
                 (with-current-buffer buf
                   (let ((inhibit-read-only t))
                     (erase-buffer))))))
       (rdebug-process-annotation name ""))
     rdebug-annotation-setup-map)
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
  (rdebug-setup-windows t))


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

      (setq comint-prompt-regexp (concat "^" rdebug-input-prompt-regexp))
      (setq paragraph-start comint-prompt-regexp)

      (setcdr (assq 'rdebug-debugger-support-minor-mode minor-mode-map-alist)
              rdebug-debugger-support-minor-mode-map-when-active)
      (when rdebug-many-windows
        (rdebug-setup-windows-initially))

      (run-hooks 'rdebug-mode-hook))))


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
