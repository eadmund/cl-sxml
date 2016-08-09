;;;; cl-sxml.lisp
;;;; Copyright (C) 2016 by Robert A. Uhl

(in-package #:cl-sxml)

(defclass sxml-handler (sax:default-handler)
  (elements
   (package :initform *package*
            :initarg :package)))

(defmethod sax:start-document ((handler sxml-handler))
  (with-slots (doctype comments processing-instructions root package elements)
      handler
    (setf elements `((,(intern "*TOP*" package))))))

(defmethod sax:end-document ((handler sxml-handler))
  (reverse (first (slot-value handler 'elements))))

(defmethod sax:start-dtd ((handler sxml-handler) name public-id system-id)
  (with-slots (package elements)
      handler
    (push `(,(intern "@" package)
             (,(intern "*DOCTYPE*" package)
               ,name
               ,public-id
               ,system-id))
          (first elements))))

(defmethod sax:comment ((handler sxml-handler) data)
  (push (list (intern "*COMMENT*") data) (first (slot-value handler 'elements))))

(defmethod sax:processing-instruction ((handler sxml-handler) target data)
  (with-slots (package elements)
      handler
    (push `(,(intern "*PI*" package)
             ,(intern target package)
             ,data)
          (first elements))))

(defun namespace-p (attribute)
  (string= (sax:attribute-namespace-uri attribute)
           "http://www.w3.org/2000/xmlns/"))

(defun intern-attribute (attribute package)
  (intern
   (if (sax:attribute-namespace-uri attribute)
       (concatenate 'string
                    (sax:attribute-namespace-uri attribute)
                    ":"
                    (sax:attribute-local-name attribute))
       (sax:attribute-local-name attribute))
   package))

(defun make-namespace (attribute package)
  (let ((value (sax:attribute-value attribute))
        (local-name (sax:attribute-local-name attribute)))
    `(,(intern value package)
       ,value
       ,@(when local-name
           (intern local-name package)))))

(defmethod sax:start-element ((handler sxml-handler)
                              namespace-uri
                              local-name
                              qname
                              attributes)
  (with-slots (elements package)
      handler
    (let ((name
           (intern
            (if namespace-uri
                (concatenate 'string namespace-uri ":" local-name)
                local-name)
            package))
          (namespaces (remove-if-not #'namespace-p attributes))
          (attributes (mapcar
                       (lambda (x)
                         `(,(intern-attribute x package)
                            ,(sax:attribute-value x)))
                       (remove-if #'namespace-p attributes))))
      (when namespaces
        (push `(,(intern "@" package)
                 (,(intern "*NAMESPACES*" package)
                   ,@(mapcar (lambda (x) (make-namespace x package)) namespaces)))
              attributes))
      (push `(,@(when attributes `((,(intern "@" package) ,@attributes))) ,name)
            elements))))

(defmethod sax:end-element ((handler sxml-handler)
                            namespace-uri
                            local-name
                            qname)
  (with-slots (elements)
      handler
    (let ((element (pop elements)))
      (push (reverse element) (first elements)))))

(defmethod sax:characters ((handler sxml-handler) data)
  (push data (first (slot-value handler 'elements))))
