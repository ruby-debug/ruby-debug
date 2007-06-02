;; Copyright (C) 2006 Free Software Foundation, Inc.
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
;; ======================================================================
;;; rdebugtrack --- tracking rdebug debugger in an Emacs shell window
;;; Modified from  python-mode in particular the part:
;; pdbtrack support contributed by Ken Manheimer, April 2001.

(require 'gud)
(provide 'rdebug-track)
(require 'comint)
(require 'custom)
(require 'cl)
(require 'compile)
(require 'shell)

(defgroup rdebugtrack nil
  "Rdebug file tracking by watching the prompt."
  :prefix "rdebug-rdebugtrack-"
  :group 'shell)


;; user definable variables
;; vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv

(defcustom rdebug-rdebugtrack-do-tracking-p t
  "*Controls whether the rdebugtrack feature is enabled or not.
When non-nil, rdebugtrack is enabled in all comint-based buffers,
e.g. shell buffers and the *Python* buffer.  When using rdebug to debug a
Python program, rdebugtrack notices the rdebug prompt and displays the
source file and line that the program is stopped at, much the same way
as gud-mode does for debugging C programs with gdb."
  :type 'boolean
  :group 'rdebug)
(make-variable-buffer-local 'rdebug-rdebugtrack-do-tracking-p)

(defcustom rdebug-rdebugtrack-minor-mode-string " RDEBUG"
  "*String to use in the minor mode list when rdebugtrack is enabled."
  :type 'string
  :group 'rdebug)

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


;; ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
;; NO USER DEFINABLE VARIABLES BEYOND THIS POINT

;; have to bind rdebug-file-queue before installing the kill-emacs-hook
(defvar rdebug-file-queue nil
  "Queue of Makefile temp files awaiting execution.
Currently-active file is at the head of the list.")

(defvar rdebug-rdebugtrack-is-tracking-p t)


;; Constants

(defconst rdebug-position-re 
  "\\(\\)\\([-a-zA-Z0-9_/.]*\\):\\([0-9]+\\)"
  "Regular expression for a rdebug position")

(defconst rdebug-marker-regexp-file-group 2
  "Group position in rdebug-position-re that matches the file name.")

(defconst rdebug-marker-regexp-line-group 3
 "Group position in rdebug-position-re that matches the line number.")

(defconst rdebug-traceback-line-re
  "^#[0-9]+[ \t]+\\((\\([a-zA-Z-.]+\\) at (\\(\\([a-zA-Z]:\\)?[^:\n]*\\):\\([0-9]*\\)).*\n"
  "Regular expression that describes tracebacks.")

;; rdebugtrack contants
(defconst rdebug-rdebugtrack-stack-entry-regexp
      "^(\\([-a-zA-Z0-9_/.]*\\):\\([0-9]+\\)):[ \t]?\\(.*\n\\)"
  "Regular expression rdebugtrack uses to find a stack trace entry.")

(defconst rdebug-rdebugtrack-input-prompt "\n(+rdb:\\([0-9]+\\|post-mortem\\))+ *"
  "Regular expression rdebugtrack uses to recognize a rdebug prompt.")

(defconst rdebug-rdebugtrack-track-range 10000
  "Max number of characters from end of buffer to search for stack entry.")


;; Utilities
(defmacro rdebug-safe (&rest body)
  "Safely execute BODY, return nil if an error occurred."
  (` (condition-case nil
	 (progn (,@ body))
       (error nil))))


;;;###autoload

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

      (let* ((procmark (process-mark currproc))
             (block (buffer-substring (max comint-last-input-end
                                           (- procmark
                                              rdebug-rdebugtrack-track-range))
                                      procmark))
             target target_fname target_lineno target_buffer)

        (if (not (string-match rdebug-rdebugtrack-input-prompt block))
            (rdebug-rdebugtrack-overlay-arrow nil)

          (setq target (rdebug-rdebugtrack-get-source-buffer block))

          (if (stringp target)
              (message "rdebugtrack: %s" target)

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

(defun rdebug-rdebugtrack-get-source-buffer (block)
  "Return line number and buffer of code indicated by block's traceback text.

We look first to visit the file indicated in the trace.

Failing that, we look for the most recently visited python-mode buffer
with the same name or having 
having the named function.

If we're unable find the source code we return a string describing the
problem as best as we can determine."

  (if (not (string-match rdebug-position-re block))

      "line number cue not found"

    (let* ((filename (match-string rdebug-marker-regexp-file-group block))
           (lineno (string-to-int 
		    (match-string rdebug-marker-regexp-line-group block)))
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
  (remove-hook 'comint-output-filter-functions 'bashdb-bashdbtrack-track-stack-file))


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


