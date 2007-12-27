;;; rdebug-track.el --- Tracking the Ruby debugger from a shell
;; $Id$
;; Copyright (C) 2006, 2007 Rocky Bernstein (rocky@gnu.org)
;;; Modified from  python-mode in particular the part:
;; pdbtrack support contributed by Ken Manheimer, April 2001.

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
;;  `rdebug-track-mode' allows access to full debugger user interface
;;   for Ruby debugger sessions started in a standard shell window.
;;   `turn-on-rdebug-track-mode' turns the mode on and
;;   `turn-off-rdebug-track-mode' turns it off.
;; 
;;; Customization:
;;  `rdebug-track' sets whether file tracking is done by the shell prompt.
;;  `rdebug-track-minor-mode-string' sets the mode indicator to show that
;;  tracking is in effect.
;;

(defgroup rdebug-track nil
  "Rdebug file tracking by watching the shell prompt."
  :prefix "rdebug-track"
  :group 'shell)

(defcustom rdebug-track-do-tracking-p nil
  "*Controls whether the rdebug-track feature is enabled or not.
When non-nil, rdebug-track is enabled in all comint-based buffers,
e.g. shell buffers and the *Ruby* buffer.  When using rdebug to debug a
Ruby program, rdebug-track notices the rdebug prompt and displays the
source file and line that the program is stopped at, much the same way
as gud-mode does for debugging C programs with gdb."
  :type 'boolean
  :group 'rdebug)
(make-variable-buffer-local 'rdebug-track-do-tracking-p)

(defcustom rdebug-track-minor-mode-string " rdebug"
  "*String to use in the minor mode list when rdebug-track is enabled."
  :type 'string
  :group 'rdebug)

;; ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
;; NO USER DEFINABLE VARIABLES BEYOND THIS POINT

(defvar gud-rdebug-history nil
  "History of argument lists passed to rdebug.")

;; rdebug-track constants
(defconst rdebug-track-stack-entry-regexp
  "^(\\([-a-zA-Z0-9_/.]*\\):\\([0-9]+\\)):[ \t]?\\(.*\n\\)"
  "Regular expression rdebug-track uses to find a stack trace entry.")

(defconst rdebug-track-input-prompt "\n(+rdb:\\([0-9]+\\|post-mortem\\))+ *"
  "Regular expression rdebug-track uses to recognize a rdebug prompt.")

(defconst rdebug-track-track-range 10000
  "Max number of characters from end of buffer to search for stack entry.")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'comint)
(require 'custom)
(require 'cl)
(require 'compile)
(require 'shell)
(require 'rdebug-core)

(defvar rdebug-track-is-tracking-p t)

(defun rdebug-track-overlay-arrow (activation)
  "Activate or de arrow at beginning-of-line in current buffer."
  ;; This was derived/simplified from edebug-overlay-arrow
  (cond (activation
	 (setq overlay-arrow-position (make-marker))
	 (setq overlay-arrow-string "=>")
	 (set-marker overlay-arrow-position (point) (current-buffer))
	 (setq rdebug-track-is-tracking-p t))
	(rdebug-track-is-tracking-p
	 (setq overlay-arrow-position nil)
	 (setq rdebug-track-is-tracking-p nil))
	))

(defun rdebug-track-track-stack-file (text)
  "Show the file indicated by the rdebug stack entry line, in a separate window.
Activity is disabled if the buffer-local variable
`rdebug-track-do-tracking-p' is nil.

We depend on the rdebug input prompt matching `rdebug-track-input-prompt'
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
  (rdebug-debug-enter "rdebug-track-track-stack-file"
    (let* ((origbuf (current-buffer))
           (currproc (get-buffer-process origbuf)))

      (if (not (and currproc rdebug-track-do-tracking-p))
          (rdebug-track-overlay-arrow nil)
        ;;else
        (let* ((procmark (process-mark currproc))
               (block-start (max comint-last-input-end
                                 (- procmark rdebug-track-track-range)))
               (block-str (buffer-substring block-start procmark))
               target target_fname target_lineno target_buffer)

          (if (not (string-match rdebug-track-input-prompt block-str))
              (rdebug-track-overlay-arrow nil)
            ;;else
            (setq target (rdebug-track-get-source-buffer block-str))

            (if (stringp target)
                (rdebug-debug-message "rdebug-track: %s" target)
              ;;else
              (gud-rdebug-marker-filter block-str)
              (setq target_lineno (car target))
              (setq target_buffer (cadr target))
              (setq target_fname (buffer-file-name target_buffer))
              (switch-to-buffer-other-window target_buffer)
              (goto-line target_lineno)
              (rdebug-debug-message "rdebug-track: line %s, file %s"
                                    target_lineno target_fname)
              (rdebug-track-overlay-arrow t)
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

(defun rdebug-track-get-source-buffer (block-str)
  "Return line and buffer of code indicated by block-str's traceback text.

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


;; rdebug-track
(defcustom rdebug-track-mode-text " rdebug"
  "*String to display in the mode line when rdebug-track mode is active.

\(When the string is not empty, make sure that it has a leading space.)"
  :tag "rdebug mode text"           ; To separate it from `global-...'
  :group 'rdebug
  :type 'string)

(define-minor-mode rdebug-track-mode ()
  "Minor mode for tracking ruby debugging inside a process shell."
  :init-value nil
  ;; The indicator for the mode line.
  :lighter rdebug-track-mode-text
  ;; The minor mode bindings.
  :global nil
  :group 'rdebug
  (rdebug-track-toggle-stack-tracking 1)
  (setq rdebug-track-is-tracking-p t)
  (local-set-key "\C-cg" 'rdebug-goto-traceback-line)
  (local-set-key "\C-cG" 'rdebug-goto-dollarbang-traceback-line)
  (add-hook 'comint-output-filter-functions 'rdebug-track-track-stack-file)
  (run-mode-hooks 'rdebug-track-mode-hook))


(defun rdebug-track-toggle-stack-tracking (arg)
  (interactive "P")
  (if (not (get-buffer-process (current-buffer)))
      (message "No process associated with buffer '%s'" (current-buffer))
    ;;else
    ;; missing or 0 is toggle, >0 turn on, <0 turn off
    (if (or (not arg)
	    (zerop (setq arg (prefix-numeric-value arg))))
	(setq rdebug-track-do-tracking-p (not rdebug-track-do-tracking-p))
      (setq rdebug-track-do-tracking-p (> arg 0)))
    (message "%sabled rdebug's rdebug-track"
	     (if rdebug-track-do-tracking-p "En" "Dis"))))


;;;###autoload
(defun turn-on-rdebug-track-mode ()
  "Turn on rdebug-track mode.

This function is designed to be added to hooks, for example:
  (add-hook 'comint-mode-hook 'turn-on-rdebug-track-mode)"
  (interactive)
  (set (make-local-variable 'gud-last-last-frame) nil)
  (set (make-local-variable 'gud-last-frame) nil)
  (rdebug-track-mode 1))

(defun rdebug-track-attach (&optional name)
  "Do things to make the current process buffer work like a
rdebug command buffer. In particular, the buffer is renamed,
gud-mode is set, and rdebug-track-mode is turned on, among other
things. When `rdebug-many-windows' is non-nil, the initial debugger
window layout is used."
  (interactive "sProgram name: ")
  (rdebug-debug-enter "rdebug-set-windows"
    (rdebug-set-window-configuration-state 'debugger t)

    ;; from gud-common-init
    (gud-mode)
    (set (make-local-variable 'gud-marker-filter) 'gud-rdebug-marker-filter)
    (set (make-local-variable 'gud-find-file) 'gud-rdebug-find-file)
    (setq gud-last-last-frame nil)
    (set-process-filter (get-buffer-process (current-buffer)) 'gud-filter)
    (set-process-sentinel (get-buffer-process (current-buffer)) 'gud-sentinel)
    (gud-set-buffer)
    ;;   

    (set (make-local-variable 'gud-last-last-frame) nil)
    (set (make-local-variable 'gud-last-frame) nil)
    (rdebug-track-mode 1)
    (rdebug-common-initialization)
    (when name
      (setq gud-target-name name)
      (setq gud-comint-buffer (current-buffer)))
    (when gud-last-frame
      (setq gud-last-last-frame gud-last-frame))
    (rename-buffer (format "*rdebug-cmd-%s*" gud-target-name))

    (setcdr (assq 'rdebug-debugger-support-minor-mode minor-mode-map-alist)
            rdebug-debugger-support-minor-mode-map-when-active)
    (when rdebug-many-windows
      (rdebug-setup-windows))))


(defun turn-off-rdebug-track-mode ()
  "Turn off rdebug-track mode."
  (interactive)
  (setq rdebug-track-is-tracking-p nil)

  (rdebug-track-toggle-stack-tracking 0)
  (if (local-variable-p 'gud-last-frame)
      (setq 'gud-last-frame nil))
  (remove-hook 'comint-output-filter-functions
	       'rdebug-track-track-stack-file))


(provide 'rdebug-track)
;; rdebug-track
