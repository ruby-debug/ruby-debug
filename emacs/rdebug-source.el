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
(require 'rdebug-vars)
(require 'rdebug-regexp)
(require 'rdebug-cmd)


;; -------------------------------------------------------------------
;; Internal debug trace support.
;;

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


;; -------------------------------------------------------------------
;; Interface to gud.
;;

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


;; -------------------------------------------------------------------
;; Key bindings
;;

(defun rdebug-populate-common-keys-standard (map)
  "Bind the basic debugger key layout used by many debuggers.

\\{rdebug-example-map-standard}"
  (define-key map [f5]    'gud-cont)
  (define-key map [S-f5]  'rdebug-quit)
  (define-key map [f9]    'rdebug-toggle-source-breakpoint)
  (define-key map [C-f9]  'rdebug-toggle-source-breakpoint-enabled)
  (define-key map [f10]   'rdebug-next)
  (define-key map [f11]   'rdebug-step)
  (define-key map [S-f11] 'gud-finish))


;; TODO: Verify and complement.
(defun rdebug-populate-common-keys-eclipse (map)
  "Bind the basic debugger key layout used by Eclipse.

\\{rdebug-example-map-eclipse}"
  ;;(define-key map []  'gud-cont)
  ;;(define-key map []  'rdebug-quit)
  (define-key map [S-C-b] 'rdebug-toggle-source-breakpoint)
  (define-key map [f6]    'rdebug-next)
  (define-key map [f5]    'rdebug-step)
  (define-key map [f7]    'gud-finish))


;; TODO: Verify and complement.
(defun rdebug-populate-common-keys-netbeans (map)
  "Bind the basic debugger key layout used by NetBeans.

\\{rdebug-example-map-netbeans}"
  ;;(define-key map []  'gud-cont)
  ;;(define-key map []  'rdebug-quit)
  ;; F4 - Run to cursor.
  (define-key map [S-f8]   'rdebug-toggle-source-breakpoint)
  (define-key map [f8]     'rdebug-next)
  (define-key map [f7]     'rdebug-step)
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
  (define-key map "\C-x\C-a\C-q" 'rdebug-short-key-mode)
  (if rdebug-populate-common-keys-function
      (funcall rdebug-populate-common-keys-function map)))


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
;; Window layout.
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


;; This function is intended for the Options submenu.
(defun rdebug-set-window-layout (func)
  "Set and, if the debugger is running, display the window layout."
  (interactive "aWindow layout function: ")
  (setq rdebug-window-layout-function func)
  (if gud-comint-buffer
      (rdebug-setup-windows)))


(defun rdebug-get-buffer (name script-name)
  "Return a rdebug buffer for displaying NAME when debugging SCRIPT-NAME.
If the buffer doesn't exists it is created."
  (get-buffer-create (format "*rdebug-%s-%s*" name script-name)))

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


;; -------------------------------------------------------------------
;; Menu support.
;;


;; Note: We want the key binding to show in the menu. However, our
;; situation is a little bit complex:
;;
;; 1) We want the binding of the `common' man (i.e. the function key
;;    the user has selected.)
;;
;; 2) We want this even when the menu is disabled and the key isn't
;;    bound, typically when the debugger isn't running.
;;
;; This has been solved by setting up an explicit ":keys" properly.
(defun rdebug-menu-item (common-map name cmd &rest args)
  "Return a menu item entry with the correct key bindings.

A command can be bound to a number of different key sequences. If
the rdebug common map contains a binding it is displayed in the
menu. (The common map typically contains function key bindings.)"
  (let ((key-binding (where-is-internal cmd (list common-map) t))
        (hint '()))
    (if key-binding
        (setq hint (list :keys (key-description key-binding))))
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
    ;; Use a simple common map to find the best key sequence to
    ;; display in menu.
    (rdebug-populate-common-keys common-map)

    (define-key map [menu-bar debugger] (cons "Debugger" menu))

    (define-key menu [break-delete]
      (rdebug-menu-item common-map "Enable/disable breakpoint"
                        'rdebug-toggle-source-breakpoint-enabled
                        :enable '(get-buffer-process gud-comint-buffer)))

    (define-key menu [break]
      (rdebug-menu-item common-map "Toggle breakpoint"
                        'rdebug-toggle-source-breakpoint
                        :enable '(get-buffer-process gud-comint-buffer)))

    (define-key menu [finish]
      (rdebug-menu-item common-map "Step out" 'gud-finish
                        :enable '(get-buffer-process gud-comint-buffer)))

    (define-key menu [step]
      (rdebug-menu-item common-map "Step into" 'rdebug-step
                        :enable '(get-buffer-process gud-comint-buffer)))

    (define-key menu [next]
      (rdebug-menu-item common-map "Step over" 'rdebug-next
                        :enable '(get-buffer-process gud-comint-buffer)))

    (define-key menu [cont]
      (rdebug-menu-item common-map "Continue" 'rdebug-continue
                        :enable '(get-buffer-process gud-comint-buffer)))

    (define-key map [menu-bar debugger line1] '(menu-item "--"))

    (define-key menu [stop]
      (rdebug-menu-item
       common-map "Stop the debugger" 'rdebug-quit
       :enable '(get-buffer-process gud-comint-buffer)))

    (define-key menu [start]
      (rdebug-menu-item common-map "Start the debugger" 'rdebug))

    (define-key map [menu-bar debugger line2] '(menu-item "--"))

    ;; --------------------
    ;; The "Options" submenu.

    (let ((submenu (make-sparse-keymap)))
      (define-key menu [options] (cons "Options" submenu)))

    (define-key map [menu-bar debugger options customize]
      (rdebug-menu-item common-map
                        "Customize Rdebug" 'rdebug-customize))

    (define-key map [menu-bar debugger options line1] '(menu-item "--"))


    ;; ----------------
    ;; The "Window Layout" submenu.


    ;; TODO: The following is a somewhat clumsy implementation. Maybe we can
    ;; automatically generate the entries, or use the `dynamic' menu kind?
    ;;
    ;; Also, there might be other situations where the list might be
    ;; handy, e.g. completion.
    (let ((subsubmenu (make-sparse-keymap)))
      (define-key menu [options layout] (cons "Window Layout" subsubmenu)))

    (let ((predefined '(rdebug-window-layout-standard
                        rdebug-window-layout-conservative
                        rdebug-window-layout-stack-of-windows
                        rdebug-window-layout-rocky)))

      (define-key map [menu-bar debugger options layout other]
        (rdebug-menu-item
         common-map
         "Other"
         'rdebug-set-window-layout
         :button
         `(:radio
           . (not (memq rdebug-window-layout-function (quote ,predefined))))))

      (define-key map [menu-bar debugger options layout rocky]
        (rdebug-menu-item
         common-map
         "Rocky's Own"
         (lambda ()
           (interactive)
           (rdebug-set-window-layout 'rdebug-window-layout-rocky))
         :button
         '(:radio
           . (eq rdebug-window-layout-function
                 'rdebug-window-layout-rocky))))

      (define-key map [menu-bar debugger options layout stack]
        (rdebug-menu-item
         common-map
         "Stack of Windows"
         (lambda ()
           (interactive)
           (rdebug-set-window-layout 'rdebug-window-layout-stack-of-windows))
         :button
         '(:radio
           . (eq rdebug-window-layout-function
                 'rdebug-window-layout-stack-of-windows))))

      (define-key map [menu-bar debugger options layout conservative]
        (rdebug-menu-item
         common-map
         "Conservative"
         (lambda ()
           (interactive)
           (rdebug-set-window-layout 'rdebug-window-layout-conservative))
         :button
         '(:radio
           . (eq rdebug-window-layout-function
                 'rdebug-window-layout-conservative))))

      (define-key map [menu-bar debugger options layout standard]
        (rdebug-menu-item
         common-map
         "Standard"
         (lambda ()
           (interactive)
           (rdebug-set-window-layout 'rdebug-window-layout-standard))
         :button
         '(:radio
           . (eq rdebug-window-layout-function
                 'rdebug-window-layout-standard)))))

    ;; ----------------
    ;; The "short key" toggle.

    (define-key map [menu-bar debugger options short-key-mode]
      (rdebug-menu-item common-map
                        "Short keys in source" 'rdebug-short-key-mode
                        :button
                        '(:toggle
                          . rdebug-short-key-mode)))

    ;; --------------------
    ;; The optional secondary windows submenu.


    ;; Placeholder used when populating the menu of the secondary buffers.
    (define-key menu [placeholder] nil)

    ;; --------------------
    ;; The "Window Layout" submenu.
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
    ;; The "View" submenu.
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

    (define-key map [menu-bar debugger view source]
      (rdebug-menu-item common-map
                        "Source" 'rdebug-display-source-buffer))
    menu))


;; -----------------------------------------------
;; Key bindings and menu for secondary buffers.
;;

(defun rdebug-populate-secondary-buffer-map-plain (map)
  "Bind the plain keys used in rdebug secondary buffers.

This does not menus or prefix keys."
  ;; Keys to view other buffers.
  (define-key map "?" 'rdebug-display-secondary-window-help-buffer)
  (define-key map "B" 'rdebug-display-breakpoints-buffer)
  (define-key map "C" 'rdebug-display-cmd-buffer)
  (define-key map "O" 'rdebug-display-output-buffer)
  (define-key map "S" 'rdebug-display-source-buffer)
  (define-key map "T" 'rdebug-display-stack-buffer)
  (define-key map "V" 'rdebug-display-variables-buffer)
  (define-key map "W" 'rdebug-display-watch-buffer)
  ;; Common debugger commands.
  (define-key map " " 'rdebug-step)
  (define-key map "_" 'rdebug-set-stepping-prefix)
  (define-key map "+" 'rdebug-set-stepping-prefix)
  (define-key map "-" 'rdebug-set-stepping-prefix)
  (define-key map "<" 'gud-up)
  (define-key map ">" 'gud-down)
  ;; (define-key map "a" 'gud-args)
  ;; (define-key map "b" 'gud-break)
  (define-key map "c" 'rdebug-continue)
  ;; (define-key map "d" 'gud-remove)
  (define-key map "f" 'gud-finish)
  (define-key map "n" 'rdebug-next)
  (define-key map "p" 'gud-print)
  (define-key map "q" 'rdebug-quit)
  (define-key map "r" 'rdebug-restart)
  (define-key map "R" 'rdebug-restart)
  (define-key map "s" 'rdebug-step)
  )


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
  "Display the rdebug help buffer."
  (interactive)
  (rdebug-display-secondary-buffer "help"))



(defun rdebug-pick-secondary-window-categorize (win name orig-win)
  "Return how suiteable this window is to display the a secondary buffer.
The higher score the better."
  (let ((buffer (window-buffer win)))
    (save-current-buffer
      (set-buffer buffer)
      (cond (rdebug-secondary-buffer
             (cond ((eq win orig-win)
                    ;; If the user issued the command inside a
                    ;; secondary window, use that window.
                    5)
                   ((and (member name '("variables" "watch"))
                         (memq major-mode '(rdebug-variables-mode
                                            rdebug-watch-mode)))
                    ;; Let "Watch" and "Variables" switch content.
                    4)
                   (t
                    ;; Any other secondary window.
                    3)))
            ((eq major-mode 'ruby-mode)
             ;; Avoid source windows.
             0)
            ((eq major-mode 'gud-mode)
             ;; Avoid the debugger shell window.
             1)
            (t
             ;; Just any other window.
             2)))))


(defun rdebug-display-secondary-buffer (name)
  "Display one of the rdebug secondary buffers.
If the buffer doesn't exist, do nothing. If the buffer is already
displayed, switch to it. Otherwise if the current buffer is a
secondary buffer, bury it replacing with the requested
buffer. Failing that, if there is secondary buffer visible, that
is replaced instead.  And finally failing all of the preceding,
we'll just pick a visible buffer to bury and replace."
  (let* ((target-name (or (and gud-comint-buffer
                               (buffer-local-value 'gud-target-name
                                                   gud-comint-buffer))
                          gud-target-name))
         (buf-name (format "*rdebug-%s-%s*" name target-name))
         (buf (get-buffer buf-name))
         (orig-win (selected-window)))
    (if (null buf)
        (message "Buffer %s not found" buf-name)
      ;; Find a suitable window to display the buffer in.
      (let ((win (get-buffer-window buf (selected-frame))))
        (if win
            ;; Buffer already displayed, switch to it.
            (select-window win)
          ;;
          (let ((candidate nil)
                (candidate-score -1))
            (dolist (win (window-list (selected-frame)))
              (let ((score (rdebug-pick-secondary-window-categorize
                            win name orig-win)))
                (if (> score candidate-score)
                    (progn
                      (setq candidate       win)
                      (setq candidate-score score)))))
            (select-window candidate)))))
    (switch-to-buffer buf)))


;; Note: The generic `gud' framework contains special code to handle
;; this for GDB (see `gud-display-line') which we, unfortuately can't
;; use. Instead, we call `rdebug-pick-source-window' from
;; `gud-rdebug-marker-filter'. When gud becomes more generic we could
;; hopefully solve this in another way.
;;
;; The machanism is that `rdebug-pick-source-window' displays the
;; source file in the window of our choice, and gud kindly re-uses
;; that window.


(defun rdebug-display-source-window-categorize (win)
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

(defun rdebug-frame-source-buffer (frame)
  "Return the buffer corresponding to guds frame, or nil"
  (and frame
       gud-comint-buffer
       (save-current-buffer
         (set-buffer gud-comint-buffer)
         (gud-find-file (car frame)))))


(defun rdebug-current-source-buffer ()
  "Return the latest source buffer, or nil"
  (or (rdebug-frame-source-buffer gud-last-frame)
      (rdebug-frame-source-buffer gud-last-last-frame)))


(defun rdebug-display-source-buffer ()
  "Display the current source buffer."
  (interactive)
  (let ((buffer (rdebug-current-source-buffer))
        (last-buffer (rdebug-frame-source-buffer gud-last-last-frame)))
    (if buffer
        (let ((window
               (or
                ;; Buffer is already visible, re-use the window.
                (get-buffer-window buffer)
                ;; Re-use the last window
                (and last-buffer
                     (get-buffer-window last-buffer))
                ;; Find a non-rdebug window.
                (let ((candidate nil)
                      (candidate-score -1))
                  (dolist (win (window-list (selected-frame)))
                    (let ((score
                           (rdebug-display-source-window-categorize win)))
                      (if (> score candidate-score)
                          (progn
                            (setq candidate       win)
                            (setq candidate-score score)))))
                  candidate))))
          (select-window window)
          (switch-to-buffer buffer)))))


(defun rdebug-pick-source-window ()
  "Display the source file, but do not switch window."
  (save-selected-window
    (rdebug-display-source-buffer)))


(defun rdebug-display-source-buffer-resync ()
  "Resync output and display the source buffer."
  (interactive)
  (call-interactively 'gud-source-resync)
  (rdebug-display-source-buffer))


;; -------------------------------------------------------------------
;; Secondary buffers.
;;

;; -----------------------------------------------
;; The breakpoints buffer.
;;

(defvar rdebug-breakpoints-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map [mouse-2] 'rdebug-goto-breakpoint-mouse)
    (define-key map [mouse-3] 'rdebug-goto-breakpoint-mouse)
    (define-key map "t" 'rdebug-toggle-breakpoint)
    (define-key map "\C-x\C-e" 'gud-print) ; FIXME show expresion in msg area
    (rdebug-populate-digit-keys map)
    (define-key map [(control m)] 'rdebug-goto-breakpoint)
    (define-key map [?d] 'rdebug-delete-breakpoint)
    (rdebug-populate-secondary-buffer-map map)

    ;; --------------------
    ;; The "Breakpoints window" submenu.
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

(defun rdebug--setup-breakpoints-buffer (buf comint-buffer)
  "Detects breakpoint lines and sets up keymap and mouse navigation."
  (rdebug-debug-enter "rdebug--setup-breakpoints-buffer"
    (with-current-buffer buf
      (let ((inhibit-read-only t) 
	    (old-line-number (buffer-local-value 'rdebug-current-line-number 
						 buf))
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


;; -----------------------------------------------
;; The stack frame buffer.
;;

(defvar rdebug-frames-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map [mouse-1] 'rdebug-goto-stack-frame-mouse)
    (define-key map [mouse-2] 'rdebug-goto-stack-frame-mouse)
    (define-key map [mouse-3] 'rdebug-goto-stack-frame-mouse)
    (define-key map [(control m)] 'rdebug-goto-stack-frame)
    (define-key map "0" 'rdebug-goto-frame-n)
    (define-key map "1" 'rdebug-goto-frame-n)
    (define-key map "2" 'rdebug-goto-frame-n)
    (define-key map "3" 'rdebug-goto-frame-n)
    (define-key map "4" 'rdebug-goto-frame-n)
    (define-key map "5" 'rdebug-goto-frame-n)
    (define-key map "6" 'rdebug-goto-frame-n)
    (define-key map "7" 'rdebug-goto-frame-n)
    (define-key map "8" 'rdebug-goto-frame-n)
    (define-key map "9" 'rdebug-goto-frame-n)
    (rdebug-populate-secondary-buffer-map map)

    ;; --------------------
    ;; The "Stack window" submenu.
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


;; -----------------------------------------------
;; The variables buffer.
;;

(defvar rdebug-variables-mode-map
  (let ((map (make-sparse-keymap)))
    (suppress-keymap map)
    (define-key map "\r" 'rdebug-variables-edit)
    (define-key map "e" 'rdebug-edit-variables-value)
    (define-key map [mouse-2] 'rdebug-variables-edit-mouse)
    (define-key map [mouse-3] 'rdebug-variables-edit-mouse)
    (rdebug-populate-secondary-buffer-map map)

    ;; --------------------
    ;; The "Variables window" submenu.

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


;; -----------------------------------------------
;; The watch (a.k.a display) buffer.
;;

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
    ;; The "Watch window" submenu.
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


;; -----------------------------------------------
;; The output buffer.
;;

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


;; -----------------------------------------------
;; The secondary window help buffer.
;;

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
 +   - set for step+ and next+
 -   - set for step- and next-
 _   - set to remove +/-
 c   - continue
 f   - finish (step out)
 n   - next (step over)
 p   - print
 q   - quit
 r   - run (restart)
 R   - run (restart)
 s   - step (into)

 > - go down frame (with numeric argument goes down that many frames)
 < - go up one frame (with numeric argument goes down that many frames)

 ? - This help text.
"))))


;; -------------------------------------------------------------------
;; The source buffer rdebug support mode.
;;
;; This is a minor mode that is active in Ruby source buffers. It
;; provides the menu and, when the debugger is active, the debugger
;; key bindings.

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


;; Implementation note: This is the user-level command. It only
;; controls if `rdebug-local-short-key-mode' should be activated or
;; not.
(define-minor-mode rdebug-short-key-mode
  "When enabled, short keys can be used in source buffers in `rdebug'."
  :group 'rdebug
  :global t
  :init-value nil
  (rdebug-short-key-mode-maybe-activate))


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
  (rdebug-debug-enter "rdebug-local-short-key-mode-off"
    (save-current-buffer
      (dolist (buf (buffer-list))
        (set-buffer buf)
        (if rdebug-local-short-key-mode
            (rdebug-local-short-key-mode -1))))))

(defun buffer-killed-p (buffer)
       "Return t if BUFFER is killed."
       (not (buffer-name buffer)))

(defun rdebug-local-short-key-mode-on ()
  "Turn on `rdebug-local-short-key-mode' in the current debugger frame."
  (rdebug-debug-enter "rdebug-local-short-key-mode-off"
    (save-current-buffer
      (if (and gud-comint-buffer (not (buffer-killed-p gud-comint-buffer)))
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
    (let* ((words (split-string-and-unquote command-line))
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
          (gud-call (format "set width %d" rdebug-line-width))
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
