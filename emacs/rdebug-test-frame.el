;; -*- emacs-lisp -*-
(load-file "./elk-test.el")

;; FIXME? Should we use "require 'rdebug" here.
;; Would have to prepend . to load-path. 
(load-file "./rdebug.el")
(load-file "./rdebug-core.el")

(defvar last-gud-call nil
  "Value of the last gud-call")

;; Redefine functions to make them harmless for testing
(defun gud-call (command)
  (setq last-gud-call command))

(deftest "rdebug-goto-frame-test"
  (setq rdebug-goto-entry-acc "")
  (rdebug-goto-frame-n-internal "1")
  (assert-equal "frame 1" last-gud-call)
  (rdebug-goto-frame-n-internal "0")
  (assert-equal "frame 10" last-gud-call))

;; -------------------------------------------------------------------
;; Build and run the test suite.
;;

(build-suite "rdebug-core-suite" 
	     "rdebug-goto-frame-test" )
(run-elk-test "rdebug-core-suite"
              "test some rdebug-core code")

