;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Package: natsoft; readtable: Joshua -*-

(in-package :natsoft)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Dumping a buffer
;;;
;;;
;;; The Plan Language
;;;
;;;
;;; Language Definition:
;;;   Initial-input:  (:name <name> :type <type> (optional :port <port-name> defaults to name) (Optional :branch <branch-name>))
;;;   Final-output:   (:name <name> :type <type> (optional :port <port-name> defaults to name) (Optional :branch <branch-name>))
;;;   Entry-point:    (:name <name> :ports ((:name <port-name> :type <port-type>) ...))
;;;   Exit-point:     (:name <name> :ports ((:name <port-name> :type <port-type>) ...))
;;;   State-element   (:direction <source/sink> :name <name> :port-name <port-name> :port-type <port-type>)
;;;   Component:      (:name <name> :type <type> ,@[<propterty-keyword <property-value> ... ])
;;;   Dataflow:       ((:component <component-name> :port <port-name> (optional :branch <branch-name>)
;;;                    (:component <component-name> :port <port-name> (optional :branch <branch-name>)))
;;;   Control-flow:   ((:component <component-name> (optional :branch <branch-name>))
;;;                    (:component <component-name> (optional :branch <join-name>)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-design-editor-command (com-write-code-for-buffer :name t :menu t)
    (&key (only-selected-components 'clim:boolean)
	  (name 'string :default (string (name (design-in-progress clim:*application-frame*)))))
  (let ((dip (design-in-progress clim:*application-frame*)))
    (switch-to-buffer :buffer-name "*code-buffer*")
    (set-common-lisp-mode)
    (lep:with-output-to-temp-buffer (stream "*code-buffer*")
	(dump-a-drawing-as-code dip :stream stream :selected-only only-selected-components :name name))))
    
(defun dump-a-drawing-as-code (design &key (stream *standard-output*) args selected-only name)
  (format stream ";;; -*- Mode: Common-lisp; Package: Natsoft -*-")
  (format stream "~%;;; Plan format code for ~a" name)
  (format stream "~3%(defcliche ~a (~{~a~^ ~})" name args)
  (let ((components nil)
	(initial-inputs nil)
	(final-outputs nil)
	(entry-points nil)
	(exit-points nil)
	(state-sources nil)
	(state-sinks nil)
	(dataflows nil)
	(controlflows nil))
    ;; first output all the components
    (loop for c in (children design)
	unless (and selected-only (not (member c (selected-objects *design-editor*))))
	do (typecase c
	     (initial-input (push c initial-inputs))
	     (final-output (push c final-outputs))
	     (entry-point (push c entry-points))
	     (exit-point (push c exit-points))
	     (state-source (push c state-sources))
	     (state-sink (push c state-sinks))
	     (otherwise (push c components)))
	   (typecase c
	     (output-side-mixin
	      (loop for port in (outputs c)
		  do (loop for dataflow in (outgoing-flows port)
			 do (pushnew dataflow dataflows))))
	     (has-branches-mixin
	      (loop for branch in (branches c)
		  do (loop for port in (outputs branch)
			 do (loop for dataflow in (outgoing-flows port)
				do (pushnew dataflow dataflows))))))
	   (typecase c
	     (output-side-mixin
	      (loop for control-flow in (outgoing-control-flows c)
		  do (pushnew control-flow controlflows)))
	     (has-branches-mixin
	      (loop for branch in (branches c)
		  do (loop for control-flow in (outgoing-control-flows branch)
			 do (pushnew control-flow controlflows)))))
	   )
    (when initial-inputs 
      (let* ((prefix ":initial-inputs")
	     (prefix-indent (+ 4 (length prefix))))
	(format stream "~%~2t~a (" prefix)
	(loop for initial-input in initial-inputs do (format-initial-input stream initial-input prefix-indent))
	(format stream "~%~vt)" prefix-indent)))
    (when final-outputs
      (let* ((prefix ":final-outputs")
	     (prefix-indent (+ 4 (length prefix))))
	(when final-outputs 
	  (format stream "~%~2t~a (" prefix)
	  (loop for final-output in final-outputs do (format-final-output stream final-output prefix-indent))
	  (format stream "~%~vt)" prefix-indent))))
    (when entry-points
      (let* ((prefix ":entry-points")
	     (prefix-indent (+ 4 (length prefix))))
	(format stream "~%~2t~a (" prefix)
	(loop for entry-point in entry-points do (format-entry-point stream entry-point prefix-indent))
	(format stream "~%~vt)" prefix-indent)))
    (when exit-points 
      (let* ((prefix ":exit-points")
	     (prefix-indent (+ 4 (length prefix))))
	(format stream "~%~2t~a (" prefix)
	(loop for exit-point in exit-points do (format-exit-point stream exit-point prefix-indent))
	(format stream "~%~vt)" prefix-indent)))
    (when (or state-sources state-sinks)
      (let* ((prefix ":state-elements")
	     (prefix-indent (+ 4 (length prefix))))
	(format stream "~%~2t~a (" prefix)
	(loop for state-source in state-sources do (format-state-source stream state-source prefix-indent))
	(loop for state-sink in state-sinks do (format-state-sink stream state-sink prefix-indent))
	(format stream "~%~vt)" prefix-indent)))
    (when components
      (let* ((prefix ":components")
	     (prefix-indent (+ 4 (length prefix))))
	(format stream "~%~2t~a (" prefix)
	(loop for component in components do (format-component stream component prefix-indent))
	(format stream "~%~vt)" prefix-indent)))
    (when dataflows
      (let* ((prefix ":dataflows")
	     (prefix-indent (+ 4 (length prefix))))
	(format stream "~%~2t~a (" prefix)
	(loop for dataflow in dataflows do (format-dataflow stream dataflow prefix-indent))
	(format stream "~%~vt)" prefix-indent)))
    (when controlflows
      (let* ((prefix ":control-flows")
	     (prefix-indent (+ 4 (length prefix))))
	(format stream "~%~2t~a (" prefix)
	(loop for controlflow in controlflows do (format-controlflow stream controlflow prefix-indent))
	(format stream "~%~vt)" prefix-indent)))
    (format stream "~%~2t)~%")
    ))

	      
(defun format-initial-input (stream initial-input indent)
  (let ((name (name initial-input))
	(port-name (port-name initial-input))
	(type (port-type-constraint (first (outputs initial-input))))
	(branch (branch-name initial-input))
	)
    (when (eql port-name name) (Setq port-name nil))
    (format stream "~%~vt(:name ~a :type ~a~@[ :port ~a~]~@[ :branch ~a~])"
	    indent name type port-name branch)))

(defun format-final-output (stream final-output indent)
  (let ((name (name final-output))
	(port-name (port-name final-output))
	(type (port-type-constraint (first (inputs final-output))))
	(branch (branch-name final-output))
	)
    (when (eql port-name name) (Setq port-name nil))
    (format stream "~%~vt(:name ~a :type ~a~@[ :port ~a~]~@[ :branch ~a~])"
	   indent name type port-name branch)))

(defun format-entry-point (stream entry-point indent)
  (let ((name (name entry-point))
	(ports (outputs entry-point)))
    (format stream "~%~vt(:name ~a" indent name)
    (when ports
      (loop for port in ports
	  for name = (name port)
	  for type-constraint = (port-type-constraint port)
	  for first = t then nil
	  if first 
	  do (format stream " :ports ((:name ~a :type ~a)" name type-constraint)
	  else do (format stream " (:name ~a :type ~a)" name type-constraint)
	  finally (format stream ")")))
    (format stream ")")))

(defun format-exit-point (stream exit-point indent)
  (let ((name (name exit-point))
	(ports (inputs exit-point)))
    (format stream "~%~vt(:name ~a" indent name)
    (when ports
      (loop for port in ports
	  for name = (name port)
	  for type-constraint = (port-type-constraint port)
	  for first = t then nil
	  if first 
	  do (format stream " :ports ((:name ~a :type ~a)" name type-constraint)
	  else do (format stream " (:name ~a :type ~a)" name type-constraint)
	  finally (format stream ")")))
    (format stream ")")))

(defun format-state-source (stream state-source indent)
  (let* ((name (name state-source))
	 (port (first (outputs state-source)))
	 (port-name (name port))
	 (type (port-type-constraint port)))
    (format stream "~%~vt(:name ~a :direction source :port-name ~a :port-type ~a)"
	    indent name port-name type)
    ))

(defun format-state-sink (stream state-sink indent)
  (let* ((name (name state-sink))
	 (port (first (inputs state-sink)))
	 (port-name (name port))
	 (type (port-type-constraint port)))
    (format stream "~%~vt(:name ~a :direction sink :port-name ~a :port-type ~a)"
	    indent name port-name type)
    ))

(defun format-component (stream component indent)
  (let* ((name (name component))
	 (type (task-type component))
	 (properties (properties component)))
    (format stream "~%~vt(:name ~a :type ~a~{ ~s ~a~^~})"
	    indent name type properties)))

(defun format-dataflow (stream dataflow indent)
  (let* ((input-port (input dataflow))
	 (input-port-name (name input-port))
	 (input-component (task input-port))
	 (input-component-name (if (typep input-component 'branch) (name (superior input-component)) (name input-component)))
	 (output-port (output dataflow))
	 (output-port-name (name output-port))
	 (output-component (task output-port))
	 (output-component-name (if (typep output-component 'join) (name (superior output-component)) (name output-component)))
	 (input-branch-name (when (typep input-component 'branch) (name input-component)))
	 (output-join-name (when (typep output-component 'join) (name output-component))))
    (format stream "~%~vt((:component ~a :port ~a~@[ :branch ~a~]) (:component ~a :port ~a~@[ :branch ~a~]))"
	    indent input-component-name input-port-name input-branch-name
	    output-component-name output-port-name output-join-name)))

(defun format-controlflow (stream controlflow indent)
  (let* ((input-component (predecessor controlflow))
	 (input-component-name (if (typep input-component 'branch) (name (superior input-component)) (name input-component)))
	 (input-branch-name (when (typep input-component 'branch) (name input-component)))
	 (output-component (successor controlflow))
	 (output-component-name (if (typep output-component 'join) (name (superior output-component)) (name output-component)))
	 (output-branch-name (when (typep output-component 'join) (name output-component))))
    (format stream "~%~vt((:component ~a~@[ :branch ~a~]) (:component ~a~@[ :branch ~a~]))"
	    indent input-component-name input-branch-name output-component-name output-branch-name)))

		    


;;;	for name = (name c)
;;;	do (typecase c
;;;	     (initial-input
;;;	      (let ((the-port (first (outputs c))))
;;;		(format stream "~%(initial-input ~a ~a ~a)"
;;;			name (name the-port) (port-type-constraint the-port))))
;;;	     (final-output
;;;	      (let ((the-port (first (inputs c))))
;;;		(format stream "~%(final-output ~a ~a ~a ~a)"
;;;			name (name the-port) (port-type-constraint the-port)
;;;			(branch-name c))))
;;;	     (state-source
;;;	      (let ((the-port (first (outputs c))))
;;;		(format stream "~%(state source ~a ~a ~a)"
;;;			name (name the-port) (port-type-constraint the-port))))
;;;	     (state-sink
;;;	      (let ((the-port (first (inputs c))))
;;;		(format stream "~%(state sink ~a ~a ~a)"
;;;			name (name the-port) (port-type-constraint the-port))))
;;;	     (core-task-mixin
;;;	      (let ((type (task-type c))
;;;		    (property-list (properties c)))
;;;		(format stream "~%(component ~a ~a ~{~a~^ ~})"
;;;			type name property-list)))
;;;	     
;;;	     ))
;;;    ;; next loop over the components finding dataflows
;;;    (loop for source-task in components
;;;	for source-task-name = (name source-task)
;;;	do (loop for source-port in (outputs source-task)
;;;	       for source-port-name = (name source-port)
;;;	       do (loop for flow in (outgoing-flows source-port)
;;;		      for destination-port = (output flow)
;;;		      for destination-port-name = (name destination-port)
;;;		      for destination-task = (task destination-port)
;;;		      for destination-task-name =  (name destination-task)
;;;		      do (format stream "~%(dataflow (~a ~a) (~a ~a))"
;;;				 source-task-name source-port-name
;;;				 destination-task-name destination-port-name))))
;;;    ;; next loop over vanilla, non-branching components and dump any control
;;;    ;; flows from them
;;;    (loop for source-task in components
;;;	for source-task-name = (name source-task)
;;;	do (typecase source-task 
;;;	     (branching-task
;;;	      (loop for branch in (branches source-task)
;;;		  for branch-name = (name branch)
;;;		  do (loop for control-flow in (outgoing-control-flows branch)
;;;			 for destination-task = (successor control-flow)
;;;			 for destination-task-name = (name destination-task)
;;;			 do (format stream "~%(control-flow (~a ~a) ~a)"
;;;				    source-task-name branch-name destination-task-name))))
;;;	     (basic-task
;;;	      (loop for control-flow in (outgoing-control-flows source-task)
;;;		  for destination-task = (successor control-flow)
;;;		  for destination-task-name = (name destination-task)
;;;		  do (format stream "~%(control-flow ~a ~a)"
;;;			     source-task-name destination-task-name)))))
;;;    (format stream "~%)")
;;;    ))