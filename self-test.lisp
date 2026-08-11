;;;; Parachute's own regression suite.
;;;;
;;;; The subjects package holds tests that are deliberately odd: some fail, some
;;;; skip themselves. They are never run as part of this suite. Instead each
;;;; is run in isolation by the suite proper, which then asserts on the markers
;;;; the subject left behind. A marker is present only if the form that pushes
;;;; it actually evaluated, so the suite can assert on absence as well as
;;;; presence.

(defpackage #:zebra.test.subject
  (:use #:cl #:zebra)
  (:export
   #:*ran*
   #:plain-body
   #:unhandled-error
   #:bare-skip
   #:skip-with-body
   #:skip-on-matching
   #:skip-on-not-matching
   #:skip-on-with-no-body
   #:finish-around-a-bare-skip
   #:finish-left-by-a-non-local-exit
   #:finish-around-a-contained-skip-left-by-a-non-local-exit
   #:finish-around-a-contained-skip
   #:finish-around-a-skip-with-a-body
   #:finish-that-completes
   #:control-suite
   #:control-suite-child
   #:suite-with-a-bare-skip
   #:suite-child-a
   #:suite-child-b
   #:suite-with-a-bare-skip-and-a-failing-child
   #:failing-suite-child))

(in-package #:zebra.test.subject)

(defvar *ran* ()
  "Markers pushed by the subject tests, newest first.")

;; Positive control. Every marker fires, so a missing marker elsewhere is a real
;; observation rather than a broken instrument.
(define-test plain-body
  (push :before *ran*)
  (push :after *ran*))

;; Control for the other direction. An unhandled error is already known to stop the
;; rest of a test body, so this proves a missing marker is detectable at all.
(define-test unhandled-error
  (push :before *ran*)
  (error "An expected error.")
  (push :after *ran*))

(define-test bare-skip
  (push :before *ran*)
  (skip "Skipping the rest of the test.")
  (push :after *ran*))

(define-test skip-with-body
  (push :before *ran*)
  (skip "Skipping the body."
    (push :body *ran*)
    (true NIL))
  (push :after *ran*))

(define-test skip-on-matching
  (push :before *ran*)
  (skip-on (:common-lisp) "Skipping the body."
    (push :body *ran*)
    (true NIL))
  (push :after *ran*))

(define-test skip-on-not-matching
  (push :before *ran*)
  (skip-on (:zebra-feature-that-does-not-exist) "Not skipping anything."
    (push :body *ran*))
  (push :after *ran*))

(define-test skip-on-with-no-body
  (push :before *ran*)
  (skip-on (:common-lisp) "Nothing to skip.")
  (push :after *ran*))

(define-test finish-around-a-bare-skip
  (push :before *ran*)
  (finish (skip "Skipping the rest of the test."))
  (push :after *ran*))

;; Control for the other direction. A FINISH left by a non-local exit that is not a
;; skip is a genuine failure to finish, so the guard is shown to still fire.
(define-test finish-left-by-a-non-local-exit
  (block escape
    (finish (return-from escape :gone))))

;; The finish is left by the RETURN-FROM, not by the skip: the skip unwinds to a
;; restart the form itself establishes and never crosses the finish at all.
(define-test finish-around-a-contained-skip-left-by-a-non-local-exit
  (block escape
    (finish (progn (with-forced-status (:skipped "A skip caught inside the form.")
                     (skip-body))
                   (return-from escape :gone)))))

(define-test finish-around-a-contained-skip
  (finish (progn (with-forced-status (:skipped "A skip caught inside the form.")
                   (skip-body))
                 :done)))

(define-test finish-around-a-skip-with-a-body
  (finish (skip "Skipping the body."
            (true NIL))))

(define-test finish-that-completes
  (finish :done))

;; Control. A suite whose body does not skip, so the framework is shown to run
;; children at all before absence is read as meaningful.
(define-test control-suite
  (push :control-suite *ran*))

(define-test control-suite-child
  :parent control-suite
  (push :control-suite-child *ran*))

(define-test suite-with-a-bare-skip
  (push :suite-before *ran*)
  (skip "Skipping the rest of the suite body.")
  (push :suite-after *ran*))

(define-test suite-child-a
  :parent suite-with-a-bare-skip
  (push :child-a *ran*)
  (true T))

(define-test suite-child-b
  :parent suite-with-a-bare-skip
  (push :child-b *ran*)
  (true T))

(define-test suite-with-a-bare-skip-and-a-failing-child
  (push :failing-suite-before *ran*)
  (skip "Skipping the rest of the suite body."))

(define-test failing-suite-child
  :parent suite-with-a-bare-skip-and-a-failing-child
  (push :failing-child *ran*)
  (true NIL))

(defpackage #:zebra.test
  (:use #:cl #:zebra #:zebra.test.subject))

(in-package #:zebra.test)

(defun run-subject (name)
  "Run the named subject test on its own and return its markers in evaluation
order, plus the report it produced. *PARENT* and *CONTEXT* are bound to NIL rather
than to the surrounding run's values, so the subject's results do not leak into the
suite's own."
  (let ((*ran* ())
        (*parent* NIL)
        (*context* NIL))
    (let ((report (test name :report 'quiet)))
      (values (reverse *ran*) report))))

(defun markers (name)
  (values (run-subject name)))

(defun subject-result (report)
  "The result object the named subject itself produced when it was run on its own."
  (find-if (lambda (result) (typep result 'test-result)) (results report)))

(defun finishing-status (name)
  "The status of the FINISHING-RESULT the named subject produced when run on its own."
  (multiple-value-bind (markers report) (run-subject name)
    (declare (ignore markers))
    (status (find-if (lambda (result) (typep result 'finishing-result))
                     (results (subject-result report))))))

(defun child-results (name)
  "The result objects the named subject's children produced when it was run on its own."
  (multiple-value-bind (markers report) (run-subject name)
    (declare (ignore markers))
    (remove-if-not (lambda (result) (typep result 'test-result))
                   (results (subject-result report)))))

(defun assertion-results (result)
  "Every value-result anywhere below RESULT, each listed once.

A result registers itself with more than one parent, so the tree has to be walked
with duplicates removed rather than read off any single level of it."
  (remove-duplicates
   (if (typep result 'parent-result)
       (loop for child across (results result)
             append (assertion-results child))
       (when (typep result 'value-result)
         (list result)))))

(defun plain-summary (name)
  "The text a PLAIN report prints for the named subject run on its own.

The bindings are as in RUN-SUBJECT, and the report is pointed at a string so the
subject's output does not land in the surrounding run's."
  (let ((*ran* ())
        (*parent* NIL)
        (*context* NIL)
        (stream (make-string-output-stream)))
    (test name :report 'plain :stream stream)
    (get-output-stream-string stream)))

(defun summary-count (label summary)
  "The number the summary text prints on the line LABEL introduces."
  (let ((start (search label summary)))
    (when start
      (parse-integer summary :start (+ start (length label)) :junk-allowed T))))

(define-test control-a-plain-body-runs-to-the-end
  (is equal '(:before :after) (markers 'plain-body)))

(define-test control-an-unhandled-error-stops-the-rest-of-the-body
  (is equal '(:before) (markers 'unhandled-error)))

(define-test a-bare-skip-aborts-the-rest-of-the-test-body
  (is equal '(:before) (markers 'bare-skip)))

(define-test a-skipped-test-is-not-a-failure
  (multiple-value-bind (markers report) (run-subject 'bare-skip)
    (declare (ignore markers))
    (false (results-with-status :failed report))))

(define-test a-skip-with-a-body-runs-the-body-and-the-rest-of-the-test
  (is equal '(:before :body :after) (markers 'skip-with-body)))

(define-test a-skip-with-a-body-stands-down-only-the-test-forms-in-it
  (multiple-value-bind (markers report) (run-subject 'skip-with-body)
    (declare (ignore markers))
    (let ((assertions (assertion-results report)))
      (false (results-with-status :failed report))
      (is = 1 (length assertions))
      (is eql :skipped (status (first assertions))))))

(define-test skip-on-stands-down-the-test-forms-when-a-feature-matches
  (multiple-value-bind (markers report) (run-subject 'skip-on-matching)
    (is equal '(:before :body :after) markers)
    (false (results-with-status :failed report))))

(define-test skip-on-runs-the-body-when-no-feature-matches
  (is equal '(:before :body :after) (markers 'skip-on-not-matching)))

(define-test skip-on-with-no-body-does-not-abort-the-test
  (is equal '(:before :after) (markers 'skip-on-with-no-body)))

(define-test control-a-suite-runs-its-children
  (is equal '(:control-suite :control-suite-child) (markers 'control-suite)))

(define-test a-bare-skip-leaves-the-children-to-run
  (is equal '(:suite-before :child-a :child-b) (markers 'suite-with-a-bare-skip)))

(define-test a-bare-skip-leaves-results-for-the-children
  (is = 2 (length (child-results 'suite-with-a-bare-skip))))

;; A single stood-down test puts two entries in the skipped set: its own result, and
;; the result of the form that stood it down. Only one test stood down.
(define-test plain-summary-counts-each-skip-once
  (is = 1 (summary-count "Skipped:" (plain-summary 'bare-skip)))
  ;; Control, so the one above is a count of the skip rather than whatever the line
  ;; always says.
  (is = 0 (summary-count "Skipped:" (plain-summary 'plain-body))))

(define-test a-failing-child-marks-a-skipped-test-failed
  (multiple-value-bind (markers report) (run-subject 'suite-with-a-bare-skip-and-a-failing-child)
    (declare (ignore markers))
    (is eql :failed (status (subject-result report)))))

(define-test a-bare-skip-inside-finish-is-not-a-failure
  (multiple-value-bind (markers report) (run-subject 'finish-around-a-bare-skip)
    (is equal '(:before) markers)
    (false (results-with-status :failed report))
    (is eql :skipped (status (subject-result report))))
  (is eql :skipped (finishing-status 'finish-around-a-bare-skip)))

(define-test a-bare-skip-inside-finish-is-not-recorded-as-its-value
  (multiple-value-bind (markers report) (run-subject 'finish-around-a-bare-skip)
    (declare (ignore markers))
    (let ((finish (find-if (lambda (result) (typep result 'finishing-result))
                           (results (subject-result report)))))
      (false (slot-boundp finish 'value)))))

(define-test control-a-finish-left-by-a-non-local-exit-still-fails
  (multiple-value-bind (markers report) (run-subject 'finish-left-by-a-non-local-exit)
    (declare (ignore markers))
    (is eql :failed (status (subject-result report))))
  (is eql :failed (finishing-status 'finish-left-by-a-non-local-exit)))

;; The control above cannot see this case: its subject has no skip before the exit,
;; so the attribution of the exit is never put to the question.

(define-test a-skip-caught-inside-a-finish-does-not-excuse-a-later-exit
  (is eql :failed (finishing-status 'finish-around-a-contained-skip-left-by-a-non-local-exit))
  (multiple-value-bind (markers report) (run-subject 'finish-around-a-contained-skip-left-by-a-non-local-exit)
    (declare (ignore markers))
    (is eql :failed (status (subject-result report)))))

(define-test a-skip-caught-inside-a-finish-leaves-it-finishing
  (is eql :passed (finishing-status 'finish-around-a-contained-skip)))

(define-test a-finish-around-a-skip-with-a-body-passes
  (is eql :passed (finishing-status 'finish-around-a-skip-with-a-body)))

(define-test control-a-finish-that-completes-passes
  (is eql :passed (finishing-status 'finish-that-completes)))
