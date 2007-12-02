; -*- emacs-lisp -*-
(load-file "./elk-test.el")
(load-file "./rdebug.el")

; Redefine functions to make them harmless for testing
(defun rdebug-process-annotation (name contents)
  (message name)
)

(make-variable-buffer-local 'gud-rdebug-marker-acc)

(deftest "rdebug-marker-filter-test"
  (assert-equal "Testing 1 2 3" (gud-rdebug-marker-filter "Testing 1 2 3"))
  (assert-equal "ABC" (gud-rdebug-marker-filter 
"breakpoints
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

(build-suite "rdebug-suite" 
	     "rdebug-regexp-breakpoint-test" 
	     "rdebug-regexp-file-test" 
	     "rdebug-regexp-stack-test" 
	     "rdebug-traceback-test"
	     "rdebug-unittest-traceback-test" 
	     "rdebug-marker-filter-test") 
(run-elk-test "rdebug-suite"
              "test regular expression used in tracking lines")

