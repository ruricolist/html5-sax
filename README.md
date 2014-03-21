HTML5-SAX is a trivial bridge between [CL-HTML5-PARSER][html5] and
[CXML][cxml]. It takes the DOM returned by the HTML5 parser and
serializes it to SAX events.

     (defun html->xml (html)
       (html5-sax:serialize-html5 (html5-parser:parse-html5 html)
                                  (cxml:make-string-sink)))

HTML5-SAX is intended as a replacement for [CHTML][chtml]: CHTML is
faster, but experience at TBRSS has shown that the HTML5 parser is
stabler and gets better results, even on older documents.

[html5]: https://github.com/copyleft/cl-html5-parser
[cxml]: http://common-lisp.net/project/cxml/
[chtml]: http://common-lisp.net/project/closure/closure-html/
