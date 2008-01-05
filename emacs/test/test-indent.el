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
  (assert-nil (rdebug-test-reindent-one-file "../rdebug.el"))
  (assert-nil (rdebug-test-reindent-one-file "../rdebug-cmd.el"))
  (assert-nil (rdebug-test-reindent-one-file "../rdebug-core.el"))
  (assert-nil (rdebug-test-reindent-one-file "../rdebug-source.el"))
  (assert-nil (rdebug-test-reindent-one-file "../rdebug-track.el"))
  (assert-nil (rdebug-test-reindent-one-file "../rdebug-regexp.el"))
  (assert-nil (rdebug-test-reindent-one-file "../rdebug-vars.el"))
  (assert-nil (rdebug-test-reindent-one-file "./test-cmd.el"))
  (assert-nil (rdebug-test-reindent-one-file "./test-core.el"))
  (assert-nil (rdebug-test-reindent-one-file "./test-indent.el"))
  (assert-nil (rdebug-test-reindent-one-file "./test-regexp.el")))

(run-elk-test "rdebug-indent-files"
              "test indentation of Lisp files")
