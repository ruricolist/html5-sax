;;;; html5-sax.asd

(asdf:defsystem #:html5-sax
  :serial t
  :description "Describe html5-sax here"
  :author "Your Name <your.name@example.com>"
  :license "Specify license here"
  :depends-on (#:alexandria
               #:serapeum
               #:fset
               #:cxml
               #:cxml-stp
               #:cl-html5-parser
               #:puri)
  :components ((:file "package")
               (:file "html5-sax")
               (:file "sink")))
