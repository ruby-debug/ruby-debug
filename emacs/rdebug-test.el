; -*- emacs-lisp -*-
(load-file "./elk-test.el")
(load-file "./rdebug-extra.el")
(load-file "./rdebug-track.el")

(defun regexp-test (location-str file-str)
  "Test to see that location-str matches gud-rubydb-marker-regexp"
  (assert-equal 0 (string-match gud-rubydb-marker-regexp location-str))
  (assert-equal file-str
		(substring location-str (match-beginning 1)  (match-end 1)))
)
(deftest "rdebug-marker-regexp-test"

  (regexp-test 
   "\032\032./hanoi.rb:3:\n"
   "./hanoi.rb"
   )
)

(defun position-regexp-test (location-str file-str line-str)
  "Test to see that location-str matches position-regexp-test with the correct
file and line submatches."
  (assert-equal 0 (string-match rdebug-position-re location-str))
  (assert-equal file-str (match-string rdebug-marker-regexp-file-group
                                       location-str))
  (assert-equal line-str (match-string rdebug-marker-regexp-line-group
                                       location-str))
  )
(deftest "rdebug-position-re-test"

  (position-regexp-test 
   "\032\032./hanoi.rb:3:\n"
   "./hanoi.rb" "3"
   )
)  
   
(build-suite "rdebug-suite" "rdebug-marker-regexp-test" "rdebug-position-re-test")
(run-elk-test "rdebug-suite"
              "test regular expression used in tracking lines")

