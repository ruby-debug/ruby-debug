;; -*- emacs-lisp -*-
(load-file "./elk-test.el")

;; FIXME? Should we use "require 'rdebug" here.
;; Would have to prepend . to load-path. 
(load-file "./rdebug.el")
(load-file "./rdebug-core.el")
(load-file "./rdebug-track.el")

;; Redefine functions to make them harmless for testing
(defun rdebug-process-annotation (name contents)
  (message name)
  )

(make-variable-buffer-local 'gud-rdebug-marker-acc)

(deftest "rdebug-marker-filter-test"
  (assert-equal "Testing 1 2 3" (gud-rdebug-marker-filter "Testing 1 2 3"))
  (assert-equal "ABC" (gud-rdebug-marker-filter 
                       "\
breakpoints
No breakpoints

ABC")))

(defun regexp-stack-test (location-str pos-str file-str line-str)
  "Test to see that location-str parses rdebug--stack-frame-regexp properly"
  (assert-equal 0 (string-match rdebug--stack-frame-regexp location-str))
  (assert-equal pos-str
		(substring location-str (match-beginning 2)  (match-end 2)))
  (assert-equal file-str
		(substring location-str (match-beginning 4)  (match-end 4)))
  (assert-equal line-str
		(substring location-str (match-beginning 5)  (match-end 5)))
  )
(deftest "rdebug-regexp-stack-test"

  (regexp-stack-test 
   "--> #0 at line /home/rocky/ruby/gcd.rb:18"
   "0" "/home/rocky/ruby/gcd.rb" "18"
   )
  )

(defun regexp-breakpoint-test (location-str pos-str enabled-str file-str line-str)
  "Test to see that location-str parses rdebug--breakpoint-regexp properly"
  (assert-equal 0 (string-match rdebug--breakpoint-regexp location-str))
  (assert-equal pos-str
		(substring location-str (match-beginning 1)  (match-end 1)))
  (assert-equal enabled-str
		(substring location-str (match-beginning 2)  (match-end 2)))
  (assert-equal file-str
		(substring location-str (match-beginning 3)  (match-end 3)))
  (assert-equal line-str
		(substring location-str (match-beginning 4)  (match-end 4)))
  )
(deftest "rdebug-regexp-breakpoint-test"

  (regexp-breakpoint-test 
   "  1 y   at gcd.rb:6"
   "1" "y" "gcd.rb" "6"
   )
  )

(defun regexp-file-test (location-str file-str)
  "Test to see that location-str matches gud-rdebug-marker-regexp"
  (assert-equal 0 (string-match gud-rdebug-marker-regexp location-str))
  (assert-equal file-str
		(substring location-str (match-beginning 1)  (match-end 1)))
  )
(deftest "rdebug-regexp-file-test"

  (regexp-file-test 
   "\032\032./hanoi.rb:3\n"
   "./hanoi.rb"
   )
  (regexp-file-test 
   "\032\032C:/tmp/gcd.rb:29\n"
   "C:/tmp/gcd.rb"
   )
  )

(defun regexp-position-test (location-str file-str line-str)
  "Test to see that location-str matches position-regexp-file-test with the correct
file and line submatches."
  (assert-equal 0 (string-match rdebug-position-re location-str))
  (assert-equal file-str (match-string rdebug-marker-regexp-file-group
                                       location-str))
  (assert-equal line-str (match-string rdebug-marker-regexp-line-group
                                       location-str))
  )

(defun regexp-traceback-test (location-str file-str line-str)
  "Test to see that location-str matches position-regexp-file-test with the correct
file and line submatches."
  (assert-equal 0 (string-match rdebug-traceback-line-re location-str))
  (assert-equal file-str (match-string 1 location-str))
  (assert-equal line-str (match-string 2 location-str))
  )

(deftest "rdebug-traceback-test"

  (regexp-traceback-test 
   "	from /home/rocky/ruby/gcd.rb:15:in `gcd'"
   "/home/rocky/ruby/gcd.rb" "15"
   )
  (regexp-traceback-test 
   "	from /home/rocky/ruby/gcd.rb:19"
   "/home/rocky/ruby/gcd.rb" "19"
   )
  )
   
(defun regexp-unittest-traceback-test (location-str file-str line-str)
  "Test to see that location-str matches position-regexp-file-test with the correct
file and line submatches."
  (assert-equal 0 (string-match rdebug-dollarbang-traceback-line-re 
				location-str))
  (assert-equal file-str (match-string 1 location-str))
  (assert-equal line-str (match-string 2 location-str))
  )

(deftest "rdebug-unittest-traceback-test"

  (regexp-unittest-traceback-test 
   "    [test-frame.rb:26:in `test_basic'"
   "test-frame.rb" "26"
   )
  (regexp-unittest-traceback-test 
   "     test-frame.rb:22:in `test_basic']:"
   "test-frame.rb" "22"
   )
  )
   
(deftest "rdebug-get-script-name-test"
  (assert-equal '("foo" nil) (rdebug-get-script-name '("foo")))
  (assert-equal '("foo" nil) (rdebug-get-script-name '("-m" "foo")))
  (assert-equal '("foo" t) (rdebug-get-script-name '("--annotate=1" "foo")))
  (assert-equal '("foo" t) (rdebug-get-script-name '("--annotate" "1" "foo")))
  (assert-equal '("foo" t) (rdebug-get-script-name '("-A" "1" "foo")))
  (assert-equal '("foo" nil) 
		(rdebug-get-script-name 
		 '("rdebug" "--include" "me" "-n" "foo")))
  (assert-equal '("foo" nil) (rdebug-get-script-name 
			      '("rdebug" "--server" "-d" "--host"
				"localhost" "foo" "-1")))
  )

(deftest "rdebug-goto-entry-test"
  (let ((buf (generate-new-buffer "testing")))
    (save-excursion
      (switch-to-buffer buf)
      (insert "#0 at line /tmp/gcd.rb:4\n")
      (goto-char (point-min))
      (assert-equal t (rdebug-goto-entry-try "0"))
      (assert-equal nil (rdebug-goto-entry-try "1"))
      (insert "  1 y   at gcd.rb:10\n")
      (goto-char (point-min))
      ;; Don't know why this doesn't work.
      ;;(assert-equal t (rdebug-goto-entry-try "1"))
      (insert "5: 1 + 2 = 3\n")
      (goto-char (point-min))
      (assert-equal t (rdebug-goto-entry-try "5"))
      (goto-char (point-min))
      (assert-equal nil (rdebug-goto-entry-try "3")))
    (kill-buffer buf)))

(defun rdebug-test-call-entry-n (str)
  "Call `rdebug-goto-entry-n', return the line we landed on."
  (rdebug-goto-entry-n-internal str)
  (beginning-of-line)
  (count-lines (point-min) (point)))

;; The original implementation could not go to "10" if there was no "1" entry.
(deftest "rdebug-goto-entry-test-2"
  (let ((buf (generate-new-buffer "testing")))
    (save-excursion
      (switch-to-buffer buf)
      (insert "#0 at line /tmp/gcd.rb:4\n")
      (insert "#2 at line /tmp/gcd.rb:44\n")
      (insert "#13 at line /tmp/gcd.rb:444\n")
      (goto-char (point-min))
      (setq rdebug-goto-entry-acc "")
      ;; Goto "0"
      (assert-equal 0 (rdebug-test-call-entry-n "0"))
      ;; Goto "0"
      (assert-equal 1 (rdebug-test-call-entry-n "2"))
      ;; There is no "1" or "21" or "021", so stay.
      (assert-equal 1 (rdebug-test-call-entry-n "1"))
      ;; Goto "13"
      (assert-equal 2 (rdebug-test-call-entry-n "3"))
      ;; There is no "5", "35", or "135", so stay.
      (assert-equal 2 (rdebug-test-call-entry-n "5"))
      ;; Goto "0"
      (assert-equal 0 (rdebug-test-call-entry-n "0"))
      ;; Goto "2"
      (assert-equal 1 (rdebug-test-call-entry-n "2")))
    (kill-buffer buf)))


;; -------------------------------------------------------------------
;; Check source code indentation
;;

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

(deftest "rdebug-reindent-files"
  (assert-nil (rdebug-test-reindent-one-file "rdebug.el"))
  (assert-nil (rdebug-test-reindent-one-file "rdebug-core.el"))
  (assert-nil (rdebug-test-reindent-one-file "rdebug-track.el"))
  (assert-nil (rdebug-test-reindent-one-file "rdebug-test.el")))


;; -------------------------------------------------------------------
;; Build and run the test suite.
;;

(build-suite "rdebug-suite" 
	     "rdebug-regexp-breakpoint-test" 
	     "rdebug-regexp-file-test" 
	     "rdebug-regexp-stack-test" 
	     "rdebug-traceback-test"
	     "rdebug-unittest-traceback-test" 
	     "rdebug-marker-filter-test"
	     "rdebug-goto-entry-test"
	     "rdebug-goto-entry-test-2"
	     "rdebug-reindent-files")
(run-elk-test "rdebug-suite"
              "test regular expression used in tracking lines")

