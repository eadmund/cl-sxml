;;;; test.lisp
;;;; Copyright (C) 2016 by Robert A. Uhl

(in-package #:cl-user)

(defpackage #:cl-sxml-test
  (:use #:cl #:fiveam #:cl-sxml))

(in-package #:cl-sxml-test)

(def-suite cl-sxml)
(in-suite cl-sxml)

(defun null-resolver (public-id system-id)
  (declare (ignorable public-id system-id))
  (flexi-streams:make-in-memory-input-stream nil))

(defun example-stream ()
  (let ((text "<?xml version='1.0' encoding='UTF-8'?>
<!DOCTYPE Document PUBLIC '-//foo.example//An Example//EN' 'http://foo.example/'>
<?top-level here's a top-level processing instruction?>
<doc xmlns:h='http://www.w3.org/1999/xhtml'>
<h:html>
<h:body>
<h:p class='bar'>Here is some text.</h:p>
</h:body></h:html></doc>"))
    #+(or sbcl allegro ccl) text
    #+(or lispworks clisp)(runes:utf8-string-to-rod text)))

(test doc-example
  (let ((result (cxml:parse (example-stream)
                           (make-instance 'cl-sxml:sxml-handler)
                           :entity-resolver #'null-resolver)))
    (is (eq (first result) '*TOP*))
    (is (equal result '(*TOP*
 (@
  (*DOCTYPE* "Document" "-//foo.example//An Example//EN"
   "http://foo.example/"))
 (*pi* |top-level| "here's a top-level processing instruction")
 (|doc|
  (@
   (@
    (*namespaces*
     (|http://www.w3.org/1999/xhtml| "http://www.w3.org/1999/xhtml" |h|))))
  "
"
  (|http://www.w3.org/1999/xhtml:html| "
"
   (|http://www.w3.org/1999/xhtml:body| "
"
    (|http://www.w3.org/1999/xhtml:p| (@ (|class| "bar"))
     "Here is some text.")
    "
"))))))))

(test package
  (let* ((package (make-package (gensym "TEST-PACKAGE") :use nil))
         (result (cxml:parse (example-stream)
                             (make-instance 'cl-sxml:sxml-handler :package package)
                             :entity-resolver #'null-resolver)))
    (check-type result cons)
    (format t "~&~s" result)
    (labels ((check-items (list) (loop for item in list
                        do (typecase item
                             (symbol (is (equal (symbol-package item) package)))
                             (cons (check-items item))))))
      (check-items result))
    (delete-package package)))
