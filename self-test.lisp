;;;; Parachute's own regression suite.
;;;;
;;;; The subjects package holds tests that are deliberately odd: one fails, one
;;;; stands itself down. They are never run as part of this suite. Instead each
;;;; is run in isolation by the suite proper, which then asserts on the markers
;;;; the subject left behind. A marker is present only if the form that pushes
;;;; it actually evaluated, so the suite can assert on absence as well as
;;;; presence.

(defpackage #:org.shirakumo.parachute.test.subject
  (:use #:cl #:parachute)
  (:export
   #:*ran*
   #:plain-body
   #:unhandled-error
   #:bare-skip
   #:skip-with-body
   #:skip-on-matching
   #:skip-on-not-matching))
(in-package #:org.shirakumo.parachute.test.subject)

(defvar *ran* ()
  "Markers pushed by the subject tests, newest first.")

;; Positive control. Every marker fires, so a missing marker elsewhere is a real
;; observation rather than a broken instrument.
(define-test plain-body
  (push :before *ran*)
  (push :after *ran*))

;; Negative control. An unhandled error is already known to stand the rest of a
;; test body down, so this proves absence is detectable at all.
(define-test unhandled-error
  (push :before *ran*)
  (error "An expected error.")
  (push :after *ran*))

(define-test bare-skip
  (push :before *ran*)
  (skip "Standing the test down.")
  (push :after *ran*))

(define-test skip-with-body
  (push :before *ran*)
  (skip "Standing the body down."
    (push :body *ran*))
  (push :after *ran*))

(define-test skip-on-matching
  (push :before *ran*)
  (skip-on (:common-lisp) "Standing the body down."
    (push :body *ran*))
  (push :after *ran*))

(define-test skip-on-not-matching
  (push :before *ran*)
  (skip-on (:parachute-feature-that-does-not-exist) "Not standing anything down."
    (push :body *ran*))
  (push :after *ran*))

(defpackage #:org.shirakumo.parachute.test
  (:use #:cl #:parachute #:org.shirakumo.parachute.test.subject))
(in-package #:org.shirakumo.parachute.test)

(defun run-subject (name)
  "Run the named subject test on its own and return its markers in evaluation
order, plus the report it produced. *PARENT* and *CONTEXT* are unbound from the
surrounding run so the subject's results do not leak into the suite's own."
  (let ((*ran* ())
        (*parent* NIL)
        (*context* NIL))
    (let ((report (test name :report 'quiet)))
      (values (reverse *ran*) report))))

(defun markers (name)
  (values (run-subject name)))

(define-test control-a-plain-body-runs-to-the-end
  (is equal '(:before :after) (markers 'plain-body)))

(define-test control-an-unhandled-error-stands-the-rest-down
  (is equal '(:before) (markers 'unhandled-error)))

(define-test a-bare-skip-stands-the-enclosing-test-down
  (is equal '(:before) (markers 'bare-skip)))

(define-test a-stood-down-test-is-not-a-failure
  (multiple-value-bind (markers report) (run-subject 'bare-skip)
    (declare (ignore markers))
    (false (results-with-status :failed report))))

(define-test a-skip-with-a-body-stands-only-the-body-down
  (is equal '(:before :after) (markers 'skip-with-body)))

(define-test skip-on-stands-only-the-body-down
  (is equal '(:before :after) (markers 'skip-on-matching)))

(define-test skip-on-runs-the-body-when-no-feature-matches
  (is equal '(:before :body :after) (markers 'skip-on-not-matching)))
