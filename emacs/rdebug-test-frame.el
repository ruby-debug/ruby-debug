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
  (let ((buf (generate-new-buffer "testing")))
    (save-excursion
      (switch-to-buffer buf)
      (insert "#0 ERB.result(b#Binding) at line /usr/lib/ruby/1.8/erb.rb:736\n")
      (insert "#1 Listings.build at line erbtest.rb:24\n")
      (insert "#2 at line erbtest.rb:33\n")
      (insert "#10 Listings.build at line erbtest.rb:23")
      (goto-char (point-min))
      (setq rdebug-goto-entry-acc "")
      (rdebug-goto-frame-n-internal "5")
      (assert-equal nil last-gud-call)
      (rdebug-goto-frame-n-internal "1")
      (assert-equal "frame 1" last-gud-call)
      (rdebug-goto-frame-n-internal "0")
      (assert-equal "frame 10" last-gud-call))
    (kill-buffer buf)))

;; -------------------------------------------------------------------
;; Build and run the test suite.
;;

(build-suite "rdebug-core-suite" 
	     "rdebug-goto-frame-test" )
(run-elk-test "rdebug-core-suite"
              "test some rdebug-core code")

