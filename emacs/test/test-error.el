;; -*- emacs-lisp -*-
;; This program has to be run from the directory it is currently in and
;; the rdebug code has to be in the parent directory
(load-file "./elk-test.el")

(setq load-path (cons ".." load-path))
(require 'rdebug-error)
(setq load-path (cdr load-path))

;; -------------------------------------------------------------------

(deftest "test-chomp"
  (assert-equal "" (chomp ""))
  (assert-equal "hi" (chomp "hi"))
  (assert-equal "hi" (chomp "hi\n")))

;; -------------------------------------------------------------------
;; Build and run the test suite.
;;

(build-suite "rdebug-gud-suite"
	     "test-chomp")

(run-elk-test "rdebug-gud-suite"
              "test some rdebug-error code")
