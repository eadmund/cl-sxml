;;;; cl-sxml.asd
;;;; Copyright (C) 2016 by Robert A. Uhl

(asdf:defsystem #:cl-sxml
  :description "SXML parsing for Common Lisp"
  :author "Bob Uhl <bob.denver.co@gmail.com>"
  :license "GNU General Public License"
  :serial t
  :depends-on (#:cxml)
  :in-order-to ((test-op (test-op #:cl-sxml-test)))
  :components ((:file "package")
               (:file "cl-sxml")))

(defsystem #:cl-sxml-test
  :depends-on (#:cl-sxml #:fiveam #:asdf #:uiop #:flexi-streams
                         #+lispworks #:closure-common)
  :serial t
  :components ((:file "package")
               (:file "test"))
  :perform (test-op (o s)
                    (let ((results
                           (uiop:symbol-call '#:fiveam
                                             '#:run
                                             (uiop:find-symbol* '#:cl-sxml
                                                                '#:cl-sxml-test))))
                      (unless (uiop:symbol-call '#:fiveam '#:results-status
                                                results)
                        (uiop:symbol-call :fiveam '#:explain! results)
                        (warn "tests failed")))))
