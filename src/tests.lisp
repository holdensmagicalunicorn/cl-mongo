(in-package :cl-mongo-tim)

(define-test pair-constructor
  (let* ((pair-1 (make-pair "hello" "world"))
         (compound-pair (make-pair "abc" (make-pair "1" 2)))
         (simplified-pair (m-pair "abc" (m-pair "aoeu" "bad!"))))
    (assert-equal "hello" (pair-key pair-1))
    (assert-equal "world" (pair-value pair-1))
    (assert-equal "abc" (pair-key compound-pair))
    (assert-equal "1" (pair-key (pair-value compound-pair)))
    (assert-equal 2 (pair-value (pair-value compound-pair)))
    (assert-equal "bad!" (pair-value (pair-value simplified-pair)))))

(define-test print-pair
  (let* ((pair-1 (make-pair "hello" "world"))
         (simplified-pair (m-pair "abc" (m-pair "aoeu" 1))))
    (assert-equal "(\"hello\" : \"world\")" (mongo-print pair-1 nil))
    (assert-equal "(\"abc\" : (\"aoeu\" : 1))"
                  (mongo-print simplified-pair nil))))

(define-test get-pair-slot
  (let* ((complex-pair (m-pair "comments" (m-pair "person_1"
                                                  (m-pair "data"
                                                          "No comments")))))
    (assert-equal "comments" (get-pair-slot complex-pair 1))
    (assert-equal "person_1" (get-pair-slot complex-pair 2 1))
    (assert-equal "data" (get-pair-slot complex-pair 2 2 1))))

(define-test make-document
  (let* ((doc-1 (make-document (m-pair "hello" "world")
                               (m-pair "the" "World"))))
   (assert-equal "world" (get-element doc-1 "hello"))
   (assert-equal "World" (get-element doc-1 "the"))))

(defun test-cl-mongo-tim ()
  (run-all-tests :cl-mongo-tim))
