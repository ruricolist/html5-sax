(in-package #:html5-sax)

(defmethod html5-parser:transform-html5-dom ((to-type (eql :xhtml)) node &key)
  (serialize-dom node (cxml:make-string-sink)))

(defmethod html5-parser:transform-html5-dom ((to-type (eql :cxml-dom)) node &key)
  (serialize-dom node (cxml-dom:make-dom-builder)))

(defmethod html5-parser:transform-html5-dom ((to-type (eql :cxml-stp)) node &key)
  (serialize-dom node (cxml-stp:make-builder)))

(defmethod html5-parser:transform-html5-dom ((to-type (eql :cxml-xmls)) node &key)
  (serialize-dom node (cxml-xmls:make-xmls-builder)))
