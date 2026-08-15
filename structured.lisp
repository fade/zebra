(in-package #:zebra)

;;;; A report for a program rather than for a person.
;;;;
;;;; The result objects a run produces already carry everything that happened.
;;;; Rendering them to prose is a one-way trip: whatever the prose does not say
;;;; is gone, and a caller that needs the outcome is left counting glyphs and
;;;; parsing summary lines back into numbers that were exact to begin with. This
;;;; report writes the results themselves instead, once, as a single form that
;;;; READ accepts, so the answer to "what happened" is taken rather than
;;;; reconstructed.
;;;;
;;;; The tree under :RESULTS is the authority. The tallies beside it are a
;;;; convenience and are unfiltered: every result is counted under the status it
;;;; actually carries, with no status folded into another.

(defvar *structured-report-version* 1)

(defclass structured (report)
  ((output :initarg :output :initarg :stream :accessor output)
   (failure-records :initform (make-hash-table :test 'eq) :reader failure-records))
  (:default-initargs
   :stream *standard-output*))

;;; Capture.
;;;
;;; What kind of failure a result suffered is known only while it is being
;;; evaluated. Afterwards an error and a violated assertion look alike: both are
;;; a :FAILED status with children that say nothing about which happened. So the
;;; kind is written down at the moment the status is settled, and the condition
;;; is kept as its type and its message separately rather than as a sentence
;;; composed for a reader.

(defun condition-type-name (condition)
  (let ((name (ignore-errors (class-name (class-of condition)))))
    (if name
        (symbol-name name)
        (princ-to-string (type-of condition)))))

(defun condition-message (condition)
  (or (ignore-errors
       (let ((*print-pretty* NIL)
             (*print-circle* T)
             (*print-right-margin* 1000))
         (princ-to-string condition)))
      "The condition could not be printed."))

(defun failure-record (report result)
  (gethash result (failure-records report)))

(defun record-failure (report result kind &optional condition)
  ;; The first thing to settle a result's fate is the thing that decides it. A
  ;; later guess must not overwrite what was seen at the point of failure.
  (unless (failure-record report result)
    (setf (gethash result (failure-records report))
          (list :failure kind
                :condition (when condition (condition-type-name condition))
                :message (when condition (condition-message condition)))))
  kind)

(defmethod eval-in-context :around ((report structured) (result result))
  (when (eql :unknown (status result))
    (maybe-do-not-silence-errors
      (handler-case
          (call-next-method)
        (error (err)
          (record-failure report result :error err)
          (setf (status result) :failed))))))

(defmethod eval-in-context ((report structured) (result value-result))
  (maybe-do-not-silence-errors
    (handler-case
        (call-next-method)
      (error (err)
        (record-failure report result :error err)
        (setf (value result) (if (typep result 'multiple-value-result) (list err) err))
        (setf (status result) :failed)))))

(defmethod eval-in-context ((report structured) (result finishing-result))
  (handler-case
      (call-next-method)
    (error (err)
      (record-failure report result :error err))))

(defmethod eval-in-context :around ((report structured) (result finishing-result))
  ;; A form that is left by a non-local exit never returns here, so the kind has
  ;; to be written down on the way out.
  (unwind-protect
       (call-next-method)
    (when (eql :failed (status result))
      (record-failure report result :assertion))))

(defmethod eval-in-context :after ((report structured) (result comparison-result))
  (when (eql :failed (status result))
    (record-failure report result :assertion)))

(defmethod eval-in-context :around ((report structured) (result test-result))
  ;; The time limit is applied after the inner methods have run, so the verdict
  ;; is only readable once they have returned.
  (multiple-value-prog1
      (call-next-method)
    (when (and (eql :failed (status result))
               (ignore-errors (exceeded-time-limit-p result)))
      (record-failure report result :time-limit))))

;;; Assembly.
;;;
;;; Kept apart from the printing below so that another emitter can be written
;;; against the same data.

(defgeneric structured-result-kind (result))

(defmethod structured-result-kind ((result result)) :result)
(defmethod structured-result-kind ((result value-result)) :value)
(defmethod structured-result-kind ((result multiple-value-result)) :multiple-value)
(defmethod structured-result-kind ((result comparison-result)) :comparison)
(defmethod structured-result-kind ((result multiple-value-comparison-result)) :multiple-value-comparison)
(defmethod structured-result-kind ((result finishing-result)) :finish)
(defmethod structured-result-kind ((result parent-result)) :parent)
(defmethod structured-result-kind ((result group-result)) :group)
(defmethod structured-result-kind ((result test-result)) :test)
(defmethod structured-result-kind ((result controlling-result)) :forced-status)

(defgeneric structured-result-package (result))

(defmethod structured-result-package ((result result))
  NIL)

(defmethod structured-result-package ((result test-result))
  (ignore-errors (package-name (home (expression result)))))

(defmethod structured-result-package ((result group-result))
  (let ((expression (expression result)))
    (when (and (symbolp expression) (symbol-package expression))
      (package-name (symbol-package expression)))))

(defun structured-expression-name (designator)
  (typecase designator
    (string designator)
    (package (package-name designator))
    (null "")
    (symbol (if (symbol-package designator)
                (format NIL "~a::~a"
                        (package-name (symbol-package designator))
                        (symbol-name designator))
                (symbol-name designator)))
    (cons (format NIL "~{~a~^ ~}" (mapcar #'structured-expression-name designator)))
    (T (princ-to-string designator))))

(defun structured-result-name (result)
  (or (ignore-errors
       (let ((*print-circle* T))
         (format-result result :oneline)))
      (format NIL "<~a>" (type-of result))))

(defun structured-children (result)
  ;; A result registers itself with its parent and with the context, which for a
  ;; forced-status form are the same object, so a child can sit in the vector
  ;; twice. It is one result either way.
  (when (typep result 'parent-result)
    (remove-duplicates (coerce (results result) 'list) :from-end T)))

(defun toplevel-results (report)
  ;; Every result in the run registers itself with the report, so the report's
  ;; own vector is flat. What was asked for at the top is whatever nothing else
  ;; claims as a child.
  (let ((claimed (make-hash-table :test 'eq)))
    (loop for result across (results report)
          do (dolist (child (structured-children result))
               (setf (gethash child claimed) T)))
    (remove-duplicates
     (loop for result across (results report)
           unless (gethash result claimed)
           collect result)
     :from-end T)))

(defun map-result-tree (function results)
  (let ((seen (make-hash-table :test 'eq)))
    (labels ((walk (result)
               (unless (gethash result seen)
                 (setf (gethash result seen) T)
                 (funcall function result)
                 (mapc #'walk (structured-children result)))))
      (mapc #'walk results))))

(defun tally-status (tally status)
  (let ((count (getf tally status)))
    (if count
        (setf (getf tally status) (1+ count))
        (setf tally (append tally (list status 1))))
    tally))

(defun structured-counts (results)
  (let ((tally (list :passed 0 :failed 0 :skipped 0 :tentative 0 :unknown 0)))
    (map-result-tree (lambda (result)
                       (setf tally (tally-status tally (status result))))
                     results)
    tally))

(defun structured-packages (results)
  (let ((order ())
        (groups (make-hash-table :test 'equal)))
    (dolist (result results)
      (let ((name (structured-result-package result)))
        (unless (nth-value 1 (gethash name groups))
          (push name order)
          (setf (gethash name groups) ()))
        (push result (gethash name groups))))
    (loop for name in (nreverse order)
          collect (list name :counts (structured-counts (reverse (gethash name groups)))))))

(defun structured-result-data (report result &optional (seen (make-hash-table :test 'eq)))
  (setf (gethash result seen) T)
  (let ((record (failure-record report result)))
    (list :kind (structured-result-kind result)
          :name (structured-result-name result)
          :status (status result)
          :failure (getf record :failure)
          :condition (getf record :condition)
          :message (getf record :message)
          :duration (let ((duration (duration result)))
                      (when (realp duration) (float duration 1.0)))
          :children (loop for child in (structured-children result)
                          unless (gethash child seen)
                          collect (structured-result-data report child seen)))))

(defgeneric structured-report-data (report))

(defmethod structured-report-data ((report structured))
  (let ((toplevel (toplevel-results report)))
    (list :zebra-report *structured-report-version*
          :expression (structured-expression-name (expression report))
          :status (status report)
          ;; Nothing ran and everything passed are the same summary in prose.
          ;; Here they are a zero and a non-zero.
          :resolved (length toplevel)
          :counts (structured-counts toplevel)
          :packages (structured-packages toplevel)
          :results (mapcar (lambda (result) (structured-result-data report result))
                           toplevel))))

;;; Emission.

(defmethod summarize ((report structured))
  (let ((*print-pretty* NIL)
        (*print-circle* T)
        (*print-length* NIL)
        (*print-level* NIL)
        (*print-lines* NIL)
        (*print-case* :upcase)
        (*print-base* 10)
        (*print-radix* NIL)
        (*read-default-float-format* 'single-float)
        (*package* (find-package '#:cl-user)))
    (write (structured-report-data report) :stream (output report))
    (terpri (output report))
    (force-output (output report)))
  report)

(docs:define-docs
  (variable *structured-report-version*
    "The version of the form a STRUCTURED report writes.

It is the second element of that form, so a consumer can tell whether
the shape it was written against is the shape it is being handed.

See STRUCTURED")

  (type structured
    "A report that writes one READ-able form describing the whole run.

Where the other reports render results as prose for a person, this one
writes the results themselves, so a caller does not have to recover them
by parsing what was printed. Statuses appear exactly as the results carry
them: nothing is filtered, and no two statuses are folded together.

The form is a plist beginning with :ZEBRA-REPORT and the format version.
:RESOLVED counts what was asked for at the top level, so a run that found
no tests is distinguishable from one where everything passed. :COUNTS and
:PACKAGES tally the tree; the tree under :RESULTS is the authority.

Each entry under :RESULTS carries the kind of result it is, its name as a
string, its status, the kind of failure it suffered if it suffered one,
the type and message of the condition that caused it if there was one,
its duration in seconds, and its children.

See REPORT
See OUTPUT
See SUMMARIZE
See STRUCTURED-REPORT-DATA
See *STRUCTURED-REPORT-VERSION*")

  (function structured-report-data
    "Returns the report's contents as a fresh list, without printing anything.

This is what SUMMARIZE writes. It is separate from the writing so that
the same contents can be emitted in another form.

See STRUCTURED
See SUMMARIZE"))
