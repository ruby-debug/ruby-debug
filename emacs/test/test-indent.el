;; -*- emacs-lisp -*-
;; This program has to be run from the directory it is currently in and
;; the rdebug code has to be in the parent directory
(load-file "./elk-test.el")

;; -------------------------------------------------------------------
;; Check source code indentation
;;

(put 'rdebug-debug-enter 'lisp-indent-hook 1)

(defun rdebug-test-reindent-one-file (file)
  (let ((buf (generate-new-buffer "testing"))
        (res nil))
    (save-excursion
      (switch-to-buffer buf)
      (insert-file file)
      (emacs-lisp-mode)
      (set-buffer-modified-p nil)
      (undo-boundary)
      (indent-region (point-min) (point-max))
      (if (buffer-modified-p)
          (setq res "Reindentation failed")))
    (kill-buffer buf)
    res))

(deftest "rdebug-indent-files"
  (mapcar (lambda (lisp-file)
	    (message lisp-file)
	    (assert-nil (rdebug-test-reindent-one-file lisp-file)))
	  '("../rdebug.el" 
	    "../rdebug-breaks.el"
	    "../rdebug-core.el"
	    "../rdebug-gud.el"
	    "../rdebug-help.el"
	    "../rdebug-output.el"
	    "../rdebug-source.el"
	    "../rdebug-track.el"
	    "../rdebug-regexp.el"
	    "../rdebug-varbuf.el"
	    "../rdebug-vars.el"
	    "../rdebug-watch.el"
	    "./test-cmd.el"
	    "./test-core.el"
	    "./test-indent.el"
	    "./test-regexp.el"
	    )))

(run-elk-test "rdebug-indent-files"
              "test indentation of Lisp files")
