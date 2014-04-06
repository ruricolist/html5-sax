;;;; package.lisp

(defpackage #:html5-sax
  (:use #:cl #:alexandria #:serapeum)
  (:export #:serialize-dom #:make-html5-sink #:xml-character?
           #:close-sink))
