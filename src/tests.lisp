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
    (assert-equal "bad!" (pair-value (pair-value simplified-pair))))

  )

(define-test print-pair
  (let* ((pair-1 (make-pair "hello" "world")))
    (assert-equal "\"hello\", \"world\"" (mongo-print pair-1 nil))
    )
  )

(defun test-cl-mongo-tim ()
  (run-all-tests :cl-mongo-tim))
