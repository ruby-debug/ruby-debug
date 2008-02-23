;; -*- emacs-lisp -*-
;; This program has to be run from the directory it is currently in and
;; the rdebug code has to be in the parent directory
(load-file "./elk-test.el")

(setq load-path (cons ".." load-path))
(require 'rdebug-fns)
(setq load-path (cdr load-path))

;; -------------------------------------------------------------------

(deftest "test-chomp"
  (assert-equal "" (chomp ""))
  (assert-equal "hi" (chomp "hi"))
  (assert-equal "hi" (chomp "hi\n"))
  (assert-equal "hi\n" (chomp "hi\n\n"))
  (assert-equal "hi" (chomp "hi\n\n" t)))

(deftest "test-dead-process-p"
  (assert-equal t (rdebug-dead-process-p))
  (let ((gud-comint-buffer nil))
    (assert-equal t (rdebug-dead-process-p))))

;; -------------------------------------------------------------------
;; Build and run the test suite.
;;

(build-suite "rdebug-gud-suite"
	     "test-chomp"
	     "test-dead-process-p")

(run-elk-test "rdebug-gud-suite"
              "test some rdebug-error code")
