;; -*- emacs-lisp -*-
;; This program has to be run from the directory it is currently in and
;; the rdebug code has to be in the parent directory
(load-file "./elk-test.el")

;; FIXME? Should we use "require 'rdebug" here.
;; Would have to prepend . to load-path. 
(load-file "../rdebug-shortkey.el")

(deftest "rdebug-shortkey-mode-test"
  (let ((buf (generate-new-buffer "shortkey readwrite")))
    (with-current-buffer buf
      (setq buffer-read-only nil)
      ;; turning on short-key-mode make buffer read-only
      (rdebug-short-key-mode) 
      (assert-equal t buffer-read-only)
      
      ;; turning off short-key-mode should make buffer read-write again
      (rdebug-short-key-mode) 
      (assert-equal nil buffer-read-only))
    (kill-buffer buf))

  (let ((buf (generate-new-buffer "shortkey readonly")))
    (with-current-buffer buf
      (setq buffer-read-only t)

      ;; turning on short-key-mode keep buffer read-only
      (rdebug-short-key-mode) 
      (assert-equal t buffer-read-only)

      ;; turning off short-key-mode should make buffer read-write again
      (rdebug-short-key-mode) 
      (assert-equal nil buffer-read-only)
      (kill-buffer buf))))

;; -------------------------------------------------------------------
;; Build and run the test suite.
;;

(build-suite "rdebug-suite" 
	     "rdebug-shortkey-mode-test")
(run-elk-test "rdebug-suite"
              "test things in rdebug-shortkey.el")
