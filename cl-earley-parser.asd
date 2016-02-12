;;;; Hey, Emacs! This is a -*- Lisp -*- file!
(defsystem cl-earley-parser
    :name "CL-EARLEY-PARSER"
    :version 1.0
    :components 
    ((:file "defpackage")
     (:file "object-representations"
	    :depends-on
	    ("defpackage"))
     (:file "grammar-reader" 
	    :depends-on
	    ("defpackage" "object-representations"))
     (:file "earley-parser" 
	    :depends-on 
	    ("defpackage" "grammar-reader" "object-representations")))
    :depends-on (:split-sequence))

