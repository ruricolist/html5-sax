;;;; html5-sax.asd

(asdf:defsystem #:html5-sax
  :serial t
  :description "Bridge between HTML5 and SAX."
  :author "Paul M. Rodriguez <pmr@ruricolist.com>"
  :license "MIT"
  :depends-on (#:alexandria
               #:serapeum
               #:fset
               #:cxml
               #:cxml-stp
               #:cl-html5-parser
               #:puri)
  :components ((:file "package")
               (:file "html5-sax")
               (:file "sink")
               (:file "transform")))
