(require 'rubydb)

(defconst gud-rubydb-marker-regexp
  "\032\032\\([^:\n]*\\):\\([0-9]*\\):.*\n"
  "Regular expression used to find a file location given by rubydb.
")

(defcustom gud-rubydb-command-name "rdebug --annotate=1"
  "File name for executing the Ruby debugger.
This should be an executable on your path, or an absolute file name."
  :type 'string
  :group 'gud)

;;-----------------------------------------------------------------------------
;; ALB - annotations support
;;--------------------------------------------------------------------------

(defcustom rubydb-many-windows t
  "*If non-nil, display secondary rubydb windows, in a layout similar to `gdba'."
  :type 'boolean
  :group 'rubydb)

(defconst rubydb-annotation-start-regexp
  "^\\([a-z]+\\)\n")
(defconst rubydb-annotation-end-regexp
  "^\n")

;; There's no guarantee that Emacs will hand the filter the entire
;; marker at once; it could be broken up across several strings.  We
;; might even receive a big chunk with several markers in it.  If we
;; receive a chunk of text which looks like it might contain the
;; beginning of a marker, we save it here between calls to the
;; filter.
(defun gud-rubydb-marker-filter (string)
  ;;(message "GOT: %s" string)
  (setq gud-marker-acc (concat gud-marker-acc string))
  ;;(message "ACC: %s" gud-marker-acc)
  (let ((output "") s s2 (tmp ""))

    ;; ALB first we process the annotations (if any)
    (while (setq s (string-match rubydb-annotation-start-regexp
                                 gud-marker-acc))
      (let ((name (substring gud-marker-acc (match-beginning 1) (match-end 1)))
            (end (match-end 0)))
        (if (setq s2 (string-match rubydb-annotation-end-regexp
                                   gud-marker-acc end))
            ;; ok, annotation complete, process it and remove it
            (let ((contents (substring gud-marker-acc end s2))
                  (end2 (match-end 0)))
              (rubydb-process-annotation name contents)
              (setq gud-marker-acc
                    (concat (substring gud-marker-acc 0 s)
                            (substring gud-marker-acc end2))))
          ;; otherwise, save the partial annotation to a temporary, and re-add
          ;; it to gud-marker-acc after normal output has been processed
          (setq tmp (substring gud-marker-acc s))
          (setq gud-marker-acc (substring gud-marker-acc 0 s)))))
    
    (when (setq s (string-match rubydb-annotation-end-regexp gud-marker-acc))
      ;; save the beginning of gud-marker-acc to tmp, remove it and restore it
      ;; after normal output has been processed
      (setq tmp (substring gud-marker-acc 0 s))
      (setq gud-marker-acc (substring gud-marker-acc s)))
           
    ;; Process all the complete markers in this chunk.
    ;; Format of line looks like this:
    ;;   (/etc/init.d/ntp.init:16):
    ;; but we also allow DOS drive letters
    ;;   (d:/etc/init.d/ntp.init:16):
    (while (string-match gud-rubydb-marker-regexp gud-marker-acc)
      (setq

       ;; Extract the frame position from the marker.
       gud-last-frame
       (cons (substring gud-marker-acc 
			(match-beginning gud-rubydb-marker-regexp-file-group) 
			(match-end gud-rubydb-marker-regexp-file-group))
	     (string-to-number
	      (substring gud-marker-acc
			 (match-beginning gud-rubydb-marker-regexp-line-group)
			 (match-end gud-rubydb-marker-regexp-line-group))))

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
    (if (string-match "\032.*\\'" gud-marker-acc)
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



;;-----------------------------------------------------------------------------
;; ALB - annotations support
;;-----------------------------------------------------------------------------

(defvar rubydb--annotation-setup-map
  (progn
    (define-hash-table-test 'str-hash 'string= 'sxhash)
    (let ((map (make-hash-table :test 'str-hash)))
      (puthash "breakpoints" 'rubydb--setup-breakpoints-buffer map)
      (puthash "stack" 'rubydb--setup-stack-buffer map)
      (puthash "locals" 'rubydb--setup-locals-buffer map)
      map)))

(defun rubydb-process-annotation (name contents)
  (let ((buf (get-buffer-create (format "*rubydb-%s*" name))))
    (with-current-buffer buf
      (setq buffer-read-only t)
      (let ((inhibit-read-only t)
            (setup-func (gethash name rubydb--annotation-setup-map)))
        (erase-buffer)
        (insert contents)
        (when setup-func (funcall setup-func buf))))))

(defun rubydb-setup-windows ()
  "Layout the window pattern for `rubydb-many-windows'. This was mostly copied
from `gdb-setup-windows', but simplified."
  (pop-to-buffer gud-comint-buffer)
  (delete-other-windows)
  (split-window nil ( / ( * (window-height) 3) 4))
  (split-window nil ( / (window-height) 3))
  (split-window-horizontally)
  (other-window 1)
  (set-window-buffer (selected-window) (get-buffer-create "*rubydb-locals*"))
  (other-window 1)
  (switch-to-buffer
       (if gud-last-last-frame
	   (gud-find-file (car gud-last-last-frame))
         ;; Put buffer list in window if we
         ;; can't find a source file.
         (list-buffers-noselect)))
  (other-window 1)
  (set-window-buffer (selected-window) (get-buffer-create "*rubydb-stack*"))
  (split-window-horizontally)
  (other-window 1)
  (set-window-buffer (selected-window) (get-buffer-create "*rubydb-breakpoints*"))
  (other-window 1)
  (goto-char (point-max)))

(defun rubydb-restore-windows ()
  "Equivalent of `gdb-restore-windows' for rubydb."
  (interactive)
  (when rubydb-many-windows
    (rubydb-setup-windows)))

;; ALB fontification and keymaps for secondary buffers (breakpoints, stack)

;; -- breakpoints

(defvar rubydb--breakpoints-map
  (let ((map (make-sparse-keymap)))
    (define-key map [mouse-2] 'rubydb-goto-breakpoint-mouse)
    (define-key map [? ] 'rubydb-toggle-breakpoint)
    (define-key map [(control m)] 'rubydb-goto-breakpoint)
    (define-key map [?d] 'rubydb-delete-breakpoint)
    map)
  "Keymap to navigate/set/enable rubydb breakpoints.")

(defconst rubydb--breakpoint-regexp
  "^\\([0-9]+\\) +breakpoint +\\([a-z]+\\) +\\([a-z]+\\) +at +\\(.+\\):\\([0-9]+\\)$"
  "Regexp to recognize breakpoint lines in rubydb breakpoints buffers.")

(defun rubydb--setup-breakpoints-buffer (buf)
  "Detects breakpoint lines and sets up mouse navigation."
  (with-current-buffer buf
    (let ((inhibit-read-only t))
      (setq mode-name "RUBYDB Breakpoints")
      (goto-char (point-min))
      (while (not (eobp))
        (let ((b (point-at-bol)) (e (point-at-eol)))
          (when (string-match rubydb--breakpoint-regexp
                              (buffer-substring b e))
            (add-text-properties b e
                                 (list 'mouse-face 'highlight
                                       'keymap rubydb--breakpoints-map))
            ;; fontify "keep/del"
            (let ((face (if (string= "keep" (buffer-substring
                                             (+ b (match-beginning 2))
                                             (+ b (match-end 2))))
                            compilation-info-face
                          compilation-warning-face)))
              (add-text-properties
               (+ b (match-beginning 2)) (+ b (match-end 2))
               (list 'face face 'font-lock-face face)))
            ;; fontify "enabled"
            (when (string= "y" (buffer-substring (+ b (match-beginning 3))
                                                 (+ b (match-end 3))))
              (add-text-properties
               (+ b (match-beginning 3)) (+ b (match-end 3))
               (list 'face compilation-error-face
                     'font-lock-face compilation-error-face))))
        (forward-line)
        (beginning-of-line))))))

(defun rubydb-goto-breakpoint-mouse (event)
  "Displays the location in a source file of the selected breakpoint."
  (interactive "e")
  (with-current-buffer (window-buffer (posn-window (event-end event)))
    (rubydb-goto-breakpoint (posn-point (event-end event)))))

(defun rubydb-goto-breakpoint (pt)
  "Displays the location in a source file of the selected breakpoint."
  (interactive "d")
  (save-excursion
    (goto-char pt)
    (let ((s (buffer-substring (point-at-bol) (point-at-eol))))
      (when (string-match rubydb--breakpoint-regexp s)
        (rubydb-display-line
         (substring s (match-beginning 4) (match-end 4))
         (string-to-number (substring s (match-beginning 5) (match-end 5))))
        ))))

(defun rubydb-toggle-breakpoint (pt)
  "Toggles the breakpoint at PT in the breakpoints buffer."
  (interactive "d")
  (save-excursion
    (goto-char pt)
    (let ((s (buffer-substring (point-at-bol) (point-at-eol))))
      (when (string-match rubydb--breakpoint-regexp s)
        (let* ((enabled
                (string= (substring s (match-beginning 3) (match-end 3)) "y"))
               (cmd (if enabled "disable" "enable"))
               (bpnum (substring s (match-beginning 1) (match-end 1))))
          (gud-call (format "%s %s" cmd bpnum)))))))

(defun rubydb-delete-breakpoint (pt)
  "Deletes the breakpoint at PT in the breakpoints buffer."
  (interactive "d")
  (save-excursion
    (goto-char pt)
    (let ((s (buffer-substring (point-at-bol) (point-at-eol))))
      (when (string-match rubydb--breakpoint-regexp s)
        (let ((bpnum (substring s (match-beginning 1) (match-end 1))))
          (gud-call (format "delete %s" bpnum)))))))

(defun rubydb-display-line (file line &optional move-arrow)
  (let ((oldpos (and gud-overlay-arrow-position
                     (marker-position gud-overlay-arrow-position)))
        (oldbuf (and gud-overlay-arrow-position
                     (marker-buffer gud-overlay-arrow-position))))
    (gud-display-line file line)
    (unless move-arrow
      (when gud-overlay-arrow-position
        (set-marker gud-overlay-arrow-position oldpos oldbuf)))))


;; -- stack

(defvar rubydb--stack-frame-map
  (let ((map (make-sparse-keymap)))
    (define-key map [mouse-1] 'rubydb-goto-stack-frame-mouse)
    (define-key map [mouse-2] 'rubydb-goto-stack-frame-mouse)
    (define-key map [(control m)] 'rubydb-goto-stack-frame)
    map)
  "Keymap to navigate rubydb stack frames.")

(defconst rubydb--stack-frame-regexp
  "^\\(->\\|##\\|  \\) +\\([0-9]+\\) +\\([^ (]+\\).+$"
  "Regexp to recognize stack frame lines in rubydb stack buffers.")

(defun rubydb--setup-stack-buffer (buf)
  "Detects stack frame lines and sets up mouse navigation."
  (with-current-buffer buf
    (let ((inhibit-read-only t))
      (setq mode-name "RUBYDB Stack Frames")
      (goto-char (point-min))
      (while (not (eobp))
        (let* ((b (point-at-bol)) (e (point-at-eol))
               (s (buffer-substring b e)))
          (when (string-match rubydb--stack-frame-regexp s)
            (add-text-properties
             (+ b (match-beginning 3)) (+ b (match-end 3))
             (list 'face font-lock-function-name-face
                   'font-lock-face font-lock-function-name-face))
            (if (string= (substring s (match-beginning 1) (match-end 1)) "->")
                ;; highlight the currently selected frame
                (add-text-properties b e
                                     (list 'face 'bold
                                           'font-lock-face 'bold))
              ;; remove the trailing ## 
              (beginning-of-line)
              (delete-char 2)
              (insert "  "))
            (add-text-properties b e
                                 (list 'mouse-face 'highlight
                                       'keymap rubydb--stack-frame-map))))
        (forward-line)
        (beginning-of-line)))))

(defun rubydb-goto-stack-frame (pt)
  "Show the rubydb stack frame correspoding at PT in the rubydb stack buffer."
  (interactive "d")
  (save-excursion
    (goto-char pt)
    (let ((s (buffer-substring (point-at-bol) (point-at-eol))))
      (when (string-match rubydb--stack-frame-regexp s)
        (let ((frame (substring s (match-beginning 2) (match-end 2))))
          (gud-call (concat "frame " frame)))))))

(defun rubydb-goto-stack-frame-mouse (event)
  "Show the rubydb stack frame under the mouse in the rubydb stack buffer."
  (interactive "e")
  (with-current-buffer (window-buffer (posn-window (event-end event)))
    (rubydb-goto-stack-frame (posn-point (event-end event)))))

;; -- locals

(defun rubydb--setup-locals-buffer (buf)
  (with-current-buffer buf
    (setq mode-name "RUBYDB Locals")))

;;;###autoload
(defun rubydb (command-line)
  "Run rubydb on program FILE in buffer `*gud-FILE*'.
The directory containing FILE becomes the initial working directory
and source-file directory for your debugger."
  (interactive
   (list (gud-query-cmdline 'rubydb)))

  (gud-common-init command-line 'gud-rubydb-massage-args
		   'gud-rubydb-marker-filter 'gud-rubydb-find-file)
  (set (make-local-variable 'gud-minor-mode) 'rubydb)

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
	   "Evaluate bash expression at point.")
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

  ; remove other py-pdbtrack if which gets in the way
  (remove-hook 'comint-output-filter-functions 'py-pdbtrack-track-stack-file)

  (setq paragraph-start comint-prompt-regexp)
  (when rubydb-many-windows (rubydb-setup-windows))

  (run-hooks 'rubydb-mode-hook))

;;-----------------------------------------------------------------------------
;; ALB - redefinition of gud-reset for our own purposes

(defvar rubydb--orig-gud-reset (symbol-function 'gud-reset))

(defun gud-reset ()
  "Redefinition of `gud-reset' to take care of rubydb cleanup."
  (funcall rubydb--orig-gud-reset)
  (dolist (buffer (buffer-list))
    (when (string-match "\\*rubydb-[a-z]+\\*" (buffer-name buffer))
      (let ((w (get-buffer-window buffer)))
        (when w (delete-window w)))
      (kill-buffer buffer))))


(provide 'rubydb-extra)

