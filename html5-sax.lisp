;;;; html5-sax.lisp

(in-package #:html5-sax)

;;; "html5-sax" goes here. Hacks and glory await!

;;; HTML5 uses six namespaces (see
;;; http://www.w3.org/TR/2011/WD-html5-20110113/namespaces.html). XML
;;; and XMLNS do not need to be declared. XHTML must be the default
;;; namespace for the root element. For each of <svg> and <math> we
;;; declare the appropriate default namespace if and when we see it.
;;; That just leaves Xlink. It can only actually be used in MathML and
;;; SVG, so we declare it when we find one.

(defconst xhtml-ns "http://www.w3.org/1999/xhtml")

(defconst svg-ns "http://www.w3.org/2000/svg")

(defconst mathml-ns "http://www.w3.org/1998/Math/MathML")

(defconst xlink-ns "http://www.w3.org/1999/xlink")

(defconst xml-ns "http://www.w3.org/XML/1998/namespace")

(defconst xmlns-ns "http://www.w3.org/2000/xmlns/")

(defvar *base*)
(setf (documentation '*base* 'variable)
      "The current base URL.")

(defgeneric serialize-node (type node)
  (:documentation "Serialize NODE, of TYPE, to the current handler."))

;; Lifted from STP.
(defun xml-character? (c)
  "Is C a character allowed in XML text?"
  (declare (optimize speed))
  (let ((code (char-code c)))
    (if (< code 32)
        (case code ((9 10 13) t))
        (or (<= code #xd7ff)
            (<= #xe000 code #xfffd)
            (<= #x10000 code #x10ffff)))))

;;; `namep' and `nc-name-p' are stolen from CXML-STP.

(defun namep (str)
  (and (not (equal str ""))
       (cxml::name-start-rune-p (char str 0))
       (every #'cxml::name-rune-p str)))

(defun nc-name-p (str)
  (and (namep str) (cxml::nc-name-p str)))

(defun escape-text (string &optional attr)
  "Remove any invalid XML characters from STRING."
  (declare (string string))
  (fbind ((test (if attr
                    (lambda (c)
                      (and (xml-character? c)
                           (not (eql c #\Newline))))
                    #'xml-character?)))
    (escape string
            (lambda (c)
              (unless (test c)
                "")))))

(defun ns-prefix (ns)
  "Map NS, a namespace URI, to its prefix."
  (if (null ns)
      ""
      (string-case ns
        (#.xhtml-ns nil)
        (#.svg-ns "svg")
        (#.mathml-ns "mathml")
        ;; http://dev.w3.org/html5/html-polyglot/html-polyglot.html#attribute-level-namespaces
        (#.xlink-ns "xlink")
        (#.xml-ns "xml")
        (#.xmlns-ns "xmlns")
        (t ""))))

(defun find-attribute (node qname)
  (html5-parser:element-map-attributes
   (lambda (name ns value)
     (when (string= name qname)
       (return-from find-attribute
         (values name ns value))))
   node))

(defun serialize-dom (node handler)
  "Serialize NODE, an HTML5 document, to HANDLER as SAX events."
  ;; Since HTML5 has (limited) support for namespaces, we have to use
  ;; SAX instead of HAX.
  (let ((sax:*use-xmlns-namespace* nil)
        (*base* nil))
    (cxml:with-xml-output handler
      (cxml:with-namespace ("" xhtml-ns)
        (serialize node)))))

(defun serialize (node)
  (serialize-node (html5-parser:node-type node) node))

(defmethod serialize-node ((type (eql :doctype)) node)
  (declare (ignore node))
  ;; TODO
  (cxml:doctype "HTML" "" ""))

(defmethod serialize-node ((type (eql :text)) node)
  (cxml:text (escape-text (html5-parser:node-value node))))

(defmethod serialize-node ((type (eql :comment)) node)
  (let ((data (html5-parser:node-value node)))
    (unless (string*= "--" data) ;??? There is no way to escape this.
      (cxml:comment (escape-text (string-right-trim "-" data))))))

(defmethod serialize-node ((type (eql :document)) node)
  (let ((root (block nil
                (html5-parser:element-map-children
                 (lambda (node)
                   (and (eql (html5-parser:node-type node) :element)
                        (equal (html5-parser:node-name node) "html")
                        (return node)))
                 node))))
    (serialize-node :element root)))

(defmethod serialize-node ((type (eql :fragment)) node)
  (html5-parser:element-map-children #'serialize node))

(defmethod serialize-node ((type (eql :element)) node)
  (let* ((name (html5-parser:node-name node)))
    (if (not (nc-name-p name))
        (html5-parser:element-map-children #'serialize node)
        (flet ((serialize (&aux (base *base*))
                 (cxml:with-element name
                   (html5-parser:element-map-attributes
                    (lambda (name ns value)
                      (cond ((equal name "lang")
                             (cxml:attribute* "xml" "lang" value))
                            ((and base (equal name "href"))
                             (ignoring error ;Unfortunately PURI does
                                             ;not name all its errors.
                               (let ((value
                                       (~> value
                                           puri:parse-uri
                                           (puri:merge-uris base)
                                           (write-to-string :pretty nil
                                                            :escape nil)
                                           (escape-text t))))
                                 (cxml:attribute name value))))
                            ((string^= "xmlns" name))
                            ((nc-name-p name)
                             (if (no ns)
                                 (cxml:attribute name (escape-text value t))
                                 (string-case ns
                                   ((#.xhtml-ns #.mathml-ns #.svg-ns)
                                    (cxml:attribute name (escape-text value t)))
                                   (t (cxml:attribute* (ns-prefix ns) name value)))))))
                    node)
                   (html5-parser:element-map-children #'serialize node))))
          (declare (dynamic-extent #'serialize))
          (string-case name
            ("base"
             (when-let (href (nth-value 2 (find-attribute node "href")))
               (ignoring puri:uri-parse-error
                 (setf *base* (puri:parse-uri href)))))
            ("math" (cxml:with-namespace ("" mathml-ns)
                      (cxml:with-namespace ("xlink" xlink-ns)
                        (serialize))))
            ("svg" (cxml:with-namespace ("" svg-ns)
                     (cxml:with-namespace ("xlink" xlink-ns)
                       (serialize))))
            (t (serialize)))))))
