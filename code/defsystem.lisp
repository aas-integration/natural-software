;;; -*- Syntax: Ansi-common-lisp; Package: cl-USER; Base: 10; Mode: LISP -*- 

(in-package :cl-user)

(eval-when (:execute :load-toplevel)
  (let* ((loading-file *load-truename*)
	 (host (pathname-host loading-file))
	 (device (pathname-device loading-file))
	 (code-dir (pathname-directory loading-file))
	 (home-dir (butlast code-dir))
	 (wild-dir (append home-dir (list :wild-inferiors))))
    (let ((home-directory (make-pathname :directory home-dir
					 :host host
					 :device device))
	  (code-directory (make-pathname :directory code-dir
					 :host host
					 :device device))
	  (wild-directory (make-pathname :directory wild-dir
					 :host host 
					 :device device
					 :type :wild
					 :name :wild
					 :version :unspecific)))
      (setf (logical-pathname-translations "natsoft")
	`(("code;*.*" ,code-directory)
	  ("home;*.*" ,home-directory)
	  ("**;*.*"   ,wild-directory)
	  )))
    ))

(defsystem start-interface
    (:default-pathname "natsoft:code;"
	:default-module-class separate-destination-module)
  (:serial
   ("package-definition" (:module-class separate-destination-module)) 
   ("http-interface" (:module-class separate-destination-module)) 
   ("parse-interface" (:module-class separate-destination-module))))

(defsystem natsoft
    (:default-pathname "natsoft:code;"
	:default-module-class separate-destination-module)
  (:serial
   start-interface
   ("command-or-form" (:module-class separate-destination-module))
   ;; this holds all the predicate definitions that are used generally
   ("predicate-definitions" (:module-class joshua:separate-destination-joshua-module))
   ("basic-types" (:module-class joshua:separate-destination-joshua-module))
   ("design-editor" (:module-class joshua:separate-destination-joshua-module))
   ("display-code" (:module-class joshua:separate-destination-joshua-module))
   ("task-descriptions" (:module-class joshua:separate-destination-joshua-module))
   ("refinement" (:module-class joshua:separate-destination-joshua-module))
   ("reductions-and-cliches" (:module-class joshua:separate-destination-joshua-module))
   ("canonical-arguments" (:module-class joshua:separate-destination-joshua-module))
   ("dump-buffer" (:module-class joshua:separate-destination-joshua-module))
   ("core-facts" (:module-class joshua:separate-destination-joshua-module))
   ("learned-stuff" (:module-class data-module))
   ))









