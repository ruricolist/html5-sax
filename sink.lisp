(in-package #:html5-sax)

(def meta-charset
  (list (sax:make-attribute :namespace-uri xhtml-ns
                            :local-name "charset"
                            :qname "charset"
                            :value "UTF-8"
                            :specified-p t)))

(defclass html5-sink (sax:default-handler)
  ((stream :initform (make-string-output-stream) :accessor stream)
   (comments :initarg :allow-comments :accessor allow-comments?)
   (meta-blocked :initform nil :accessor meta-blocked?)
   (preserve-whitespace :initarg :preserve-whitespace :accessor preserve-whitespace?))
  (:default-initargs :preserve-whitespace t))

(defun omit-whitespace? (sink)
  (not (preserve-whitespace? sink)))

(defun make-html5-sink (&rest args)
  (apply #'make 'html5-sink args))

(def text-escapes
  (lambda (c)
    (case c
      (#\& "&amp;")
      (#\No-break_space "&nbsp;")
      (#\< "&lt;")
      (#\> "&gt")))
  "Characters to escape in text.")

(def attribute-escapes
  (lambda (c)
    (case c
      (#\& "&amp;")
      (#\No-break_space "&nbsp;")
      (#\" "&quot;")))
  "Characters to escape in attributes.")

(defmethod sax:end-document ((self html5-sink))
  (prog1 (get-output-stream-string (stream self))
    (close (stream self))))

(defmethod sax:start-dtd ((self html5-sink) name public-id system-id)
  (declare (ignore name public-id system-id))
  (write-string "<!doctype html>" (stream self)))

(defmethod sax:characters ((self html5-sink) text)
  (when (and (omit-whitespace? self) (blankp text))
    (return-from sax:characters))
  (escape-to-stream text (stream self) text-escapes))

(defun charset-declaration? (attr)
  "Is ATTR a charset declaration from the original document?"
  (and (not (sax:attribute-specified-p attr))
       (let ((lname (sax:attribute-local-name attr)))
         (or (string= lname "charset")
             (and (string= lname "http-equiv")
                  (string-equal (sax:attribute-value attr)
                                "content-type"))))))

(defmethod sax:start-element ((self html5-sink) ns lname qname attrs)
  (declare (ignore qname))
  ;; Suppress charset declarations from the original document.
  (when (string= lname "meta")
    (when (some #'charset-declaration? attrs)
      (setf (meta-blocked? self) t)
      (return-from sax:start-element)))
  (let ((*print-pretty*))
    (cond ((no attrs)
           (format (stream self) "<~a>" lname))
          (t (format (stream self) "<~a" lname)
             (dolist (attr attrs)
               (let ((name (sax:attribute-qname attr))
                     (value (sax:attribute-value attr)))
                 (unless (string^= "xmlns" name)
                   (format (stream self) " ~a=\"" name)
                   (escape-to-stream value (stream self) attribute-escapes)
                   (write-char #\" (stream self)))))
             (write-char #\> (stream self)))))
  ;; Force a UTF-8 charset declaration where it belongs, at the top of
  ;; HEAD.
  (when (string= lname "head")
    (sax:start-element self ns "meta" "meta" meta-charset)
    (sax:end-element self ns "meta" "meta")))

(defmethod sax:end-element ((self html5-sink) ns lname qname)
  (declare (ignore ns qname))
  ;; Finish suppressing the meta.
  (when (and (string= lname "meta")
             (meta-blocked? self))
    (setf (meta-blocked? self) nil)
    (return-from sax:end-element))
  (let (*print-pretty*)
    (format (stream self) "</~a>" lname)))
