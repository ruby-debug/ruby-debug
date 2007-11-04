; -*- emacs-lisp -*-
(load-file "./elk-test.el")
(load-file "./rdebug-track.el")

(defun regexp-stack-test (location-str pos-str file-str line-str)
  "Test to see that location-str matches gud-rdebug-marker-regexp"
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

(deftest "rdebug-regexp-position-test"

  (position-regexp-file-test 
   "\032\032./hanoi.rb:3:\n"
   "./hanoi.rb" "3"
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

(build-suite "rdebug-suite" "rdebug-regexp-file-test" "rdebug-regexp-position-test")
(run-elk-test "rdebug-suite"
              "test regular expression used in tracking lines")

