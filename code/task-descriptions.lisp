;;; -*- Mode: LISP; Syntax: Ansi-common-lisp; Package: natsoft; readtable: joshua -*-

(in-package :natsoft)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Most of the predicate definitions are in the file predicate-definitions
;;;  here are the rules that act on them
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Is of type is the transitive closure of sub-type
;;; this gets hairy with the interactions with 
;;; definitions, parameters and viewpoints
;;; It might be better to do this as an ask-data method
;;; since the rules are doing a bunch of lispy stuff


(defrule is-of-type-direct-sub (:backward)
  :then [is-of-type ?sub-type ?super-type]
  :if [subtype ?sub-type ?super-type])

;;; Keep this from going on a hunt when you don't know what the
;;; subtype is.
;;; When the subtype is known, move one step to the intermediate
(defrule is-of-type-intermediate-sub (:backward)
  :then [is-of-type ?sub-type ?super-type]
  :if [and (not (unbound-logic-variable-p ?sub-type))
	   [subtype ?sub-type ?intermediate]
	   ;; data-structure is a sub-type of itself apparently
	   (not (eql ?sub-type ?intermediate))
	   [is-of-type ?intermediate ?super-type]]
  )

;;; This is the dual, only move one step from
;;; super-type
(defrule is-of-type-intermediate-sub-2 (:backward)
  :then [is-of-type ?sub-type ?super-type]
  :if [and (not (unbound-logic-variable-p ?super-type))
	   [subtype ?intermediate ?super-type]
	   ;; data-structure is a sub-type of itself apparently
	   (not (eql ?intermediate ?super-type))
	   [is-of-type ?sub-type ?intermediate]]
  )

;;; Make sure that the query was in fact a list
;;; This would only be true if super-type is without qualifiers
;;; this version works down from the supertype
(defrule is-of-type-strip-subtype (:backward)
  :then [is-of-type ?qualified-sub-type ?super-type]
  :if [and (not (unbound-logic-variable-p ?super-type))
	   (not (listp ?super-type))
	   (and (not (unbound-logic-variable-p ?qualified-sub-type))
		(listp ?qualified-sub-type))
	   (unify ?sub-type (first ?qualified-sub-type))
	   [is-of-type ?sub-type ?super-type]]
  )

;;; This works up from the sub-type
(defrule is-of-type-strip-subtype-2 (:backward)
  :then [is-of-type ?qualified-sub-type ?super-type]
  :if [and (not (unbound-logic-variable-p ?qualified-sub-type))
	   (listp ?qualified-sub-type)
	   (not (unbound-logic-variable-p ?super-type))
	   (not (listp ?super-type))
	   (unify ?sub-type (first ?qualified-sub-type))
	   [is-of-type ?sub-type ?super-type]
	   ]
  )

(defrule is-of-type-definition (:backward)
  :then [is-of-type ?sub-type ?super-type]
  :if [and (not (unbound-logic-variable-p ?sub-type))
	   [definition ?sub-type ?definition]
	   [is-of-type ?definition ?super-type]])

(defrule is-of-type-super-definition (:backward)
  :then [is-of-type ?sub-type ?super-type]
  :if [and (not (unbound-logic-variable-p ?super-type))
	   [definition ?super-type ?definition]
	   [is-of-type ?sub-type ?definition]])
	   
(defrule is-of-type-identity (:backward)
  :then [is-of-type ?type ?type]
  :if t
  )

(defrule is-of-type-union (:backward)
  :then [is-of-type ?putative-subtype ?supertype]
  :if [union-member ?putative-subtype ?supertype]
  )

(defun union-members (union-type)
  (let ((answers nil))
    (ask* `[union-member ?sub-type ,union-type]
	  (push ?sub-type answers))
  answers))

;;; Fix these to always work from a grounded position
;;; This works from the super-type
;;; This answers the foo viewpoint of what can be regarded as a ?super-type
(defrule apply-viewpoint-equivalence-for-subtype (:backward)
  :then [is-of-type (?viewpoint ?base-type) ?supertype]
  :if [and (not (unbound-logic-variable-p ?supertype))
	   (not (unbound-logic-variable-p ?viewpoint))
	   (symbolp ?viewpoint)
	   [can-be-viewed-as ?base-type ?intermediate-type ?viewpoint]
	   [is-of-type ?intermediate-type ?supertype]])

;;; same with the base type qualified
(defrule apply-viewpoint-equivalence-for-subtype-unpack (:backward)
  :then [is-of-type (?viewpoint (?base-type . ?qualifiers)) ?super-type]
  :if [and (not (unbound-logic-variable-p ?super-type))
	   (not (unbound-logic-variable-p ?viewpoint))
	   (symbolp ?viewpoint)
	   [can-be-viewed-as ?base-type ?intermediate-type ?viewpoint]
	   [is-of-type (?intermediate-type . ?qualifiers) ?super-type]])

;;; This is answers what can the foo viewpoint of ?base-type be regarded as a sub-type of
(defrule apply-viewpoint-equivalence-for-subtype-2 (:backward)
  :then [is-of-type (?viewpoint ?base-type) ?supertype]
  :if [and (not (unbound-logic-variable-p ?base-type))
	   (not (unbound-logic-variable-p ?viewpoint))
	   (symbolp ?viewpoint)
	   [can-be-viewed-as ?base-type ?intermediate-type ?viewpoint]
	   (not (unbound-logic-variable-p ?intermediate-type))
	   [is-of-type ?intermediate-type ?supertype]])

(defrule apply-viewpoint-equivalence-for-subtype-2-unpack (:backward)
  :then [is-of-type (?viewpoint (?base-type . ?qualifiers)) ?supertype]
  :if [and (not (unbound-logic-variable-p ?base-type))
	   (not (unbound-logic-variable-p ?viewpoint))
	   (symbolp ?viewpoint)
	   [can-be-viewed-as ?base-type ?intermediate-type ?viewpoint]
	   (not (unbound-logic-variable-p ?intermediate-type))
	   [is-of-type (?intermediate-type . ?qualifiers) ?supertype]])



;;; Probably want to make sure that 
;;; that both things are actually lists
(defrule compound-is-of-type (:backward)
  :then [is-of-type (?base-1 . ?qualifiers-1) (?base-2 . ?qualifiers-2)]
  :if [and [is-of-type ?base-1 ?base-2]
	   [is-of-type ?qualifiers-1 ?qualifiers-2]])

;;; this just chases the inheritance paths
;;; anything connected to this class specifically
;;; will be retrieved by the ASK when it fetches predications
(defrule ports-of-type-in-direction (:backward)
  :then [has-port ?procedure ?direction ?port-name]
  :if [and [subtype ?procedure ?super-type]
	   [has-port ?direction ?super-type ?port-name]])

(defrule get-port-and-type-constraints-inheritance (:backward)
  :then [has-data-type ?procedure-type ?direction ?name ?type-constraint]
  :if [and [subtype ?procedure-type ?super-type]
	   [has-data-type ?super-type ?direction ?name ?type-constraint]
	   ])

(defun get-port-type-constraints (type)
  (let ((entries nil))
    (ask* `[has-data-type ,type ?direction ?name ?type-constraint]
	  (let ((entry (find ?name entries :key #'first)))
	    (unless entry
	      (setq entry (list ?name ?direction nil))
	      (push entry entries))
	    (pushnew ?type-constraint (third entry))))
    (loop for (name direction type-constraints) in entries
	if (eql direction 'input)
	collect (list name type-constraints) into inputs
	else collect (list name type-constraints) into outputs
	finally (return (values inputs outputs)))))

(defrule assert-type (:forward)
  :if [component-of ?parent ?child]
  :then `[type-of ?child ,(task-type ?child)]
  )

(defrule qualified-type-inheritance (:forward)
  :if [and [type-of ?thing (?base-type ?element-type)]
	   (not (eql ?base-type 'stream))
	   ]
  :then [type-of ?thing ?base-type]
  )

(defrule forward-type-inheritance (:forward)
  :if [and [type-of ?child ?procedure-type]
	   [subtype ?procedure-type ?super-type]]
  :then [type-of ?child ?super-type])

(defrule property-inheritance (:backward)
  :then [has-property ?type ?property]
  :if [and [subtype ?type ?parent-type]
	   [has-property ?parent-type ?property]])

(defrule property-inheritance-through-definition (:backward)
  :then [has-property ?type ?property]
  :if [and [or [definition ?type ?supertype]
	       [definition ?type (?supertype ?element-type)]]
	   [has-property ?supertype ?property]
	   ])

(defun properties-of-type (type-name)
  (let ((properties nil))
    (ask* `[has-property ,type-name ?property]
	  (pushnew ?property properties))
    properties))


(define-predicate port-name-and-direction (port direction name)
  (tell-error-model false-query-error-model default-ask-model)
  )

(define-predicate-method (ask port-name-and-direction) (truth-value continuation do-backward-rules do-queries)
  (declare (ignore truth-value do-backward-rules do-queries))
  (with-statement-destructured (port direction name) self
    (when (unbound-logic-variable-p port)
      (error
	'ji:model-cant-handle-query
	:query self
	:model ' port-name-and-direction))
    (with-unification
	(when (and (unify direction (direction port))
		   (unify name (name port)))
	  (funcall continuation nil)))))

;;; you could write an ask method for this

(define-predicate-method (expand-forward-rule-trigger port-name-and-direction) (support-variable-name truth-value context bound-variables)
  (declare (ignore context bound-variables)) 
  (unless (eql truth-value *true*)
    (error "The rule pattern ~s does not have a truth-value of true" self))
  (with-predication-maker-destructured (port direction name) self
    `(:procedure
      (and (unify ,direction (direction ,port))
	   (unify ,name (name ,port)))
      ,support-variable-name
      ,(list name direction))))
  
;;; Do we actually need two different assertion types
;;; one for dependent data type constraints
;;; and one for absolute?

(define-predicate parameter-pattern-value (instance type pattern instantiated-pattern)
  (tell-error-model false-query-error-model default-ask-model)
  )

(define-predicate-method (fetch parameter-pattern-value) (continuation)
  (declare (ignore continuation))
  (values))

(define-predicate-method (expand-forward-rule-trigger parameter-pattern-value) (support-variable-name truth-value context bound-variables)
  (declare (ignore context bound-variables)) 
  (unless (eql truth-value *true*)
    (error "The rule pattern ~s does not have a truth-value of true" self))
  (with-predication-maker-destructured (instance type pattern instantiated-pattern) self
    `(:procedure
      (multiple-value-bind (answer support) (instantiate-parameter-pattern ,pattern ,instance ,type)
	(when answer
	  (unify answer ,instantiated-pattern)
	  (succeed support)))
      ,support-variable-name
      ,(list instantiated-pattern))))

(defun property-value-of (object property-name)
  (ask `[property-value-of ,object ,property-name ?property-value]
       #'(lambda (just)
	   (return-from property-value-of
	     (values ?property-value
		     (ask-database-predication just))))))

(defun port-property-value-of (port property-name)
  (ask `[port-property-value-of ,port ,property-name ?property-value]
       #'(lambda (just)
	   (return-from port-property-value-of
	     (values ?property-value
		     (ask-database-predication just))))))

(defun instantiate-parameter-pattern (pattern instance type)
  (let ((predications nil))
    (labels ((do-it (sub-pattern)
	       (if (atom sub-pattern)
		   (get-instance-property-value sub-pattern)
		 (loop for term in sub-pattern
		     collect (do-it term))))
	     (get-instance-property-value (property-name)
	       ;; if the type doesn't have this symbol as a property
	       ;; then just return it, otherwise look up it's value
	       (ask `[has-property ,type ,property-name]
		    #'(lambda (just)
			(push (ask-database-predication just) predications)
			(ask `[property-value-of ,instance ,property-name ?property-value]
			     #'(lambda (just)
				 (push (ask-database-predication just) predications)
				 (return-from get-instance-property-value ?property-value)))
			(return-from get-instance-property-value nil)))
	       property-name))
      (let ((answer (do-it pattern)))
	(values answer predications)))))

;;;(defrule assert-type-constraint (:forward)
;;;  :if [and [component-of ?parent ?child]
;;;	   [type-of ?child ?procedure-type]
;;;	   [port-of ?child ?port]
;;;	   [port-name-and-direction ?port ?port-direction ?port-name]
;;;	   [has-data-type ?procedure-type ?port-direction ?port-name ?type-pattern]
;;;	   [parameter-pattern-value ?child ?procedure-type ?type-pattern ?type-constraint]
;;;	   ]
;;;  :then [port-type-constraint ?port ?type-constraint])


;;;(defrule assert-branch-type-constraint (:forward)
;;;  :if [and [component-of ?parent ?child]
;;;	   [type-of ?child ?procedure-type]
;;;	   [branch-of ?child ?branch]
;;;	   (unify ?branch-name (name ?branch))
;;;	   [port-of ?branch ?port]
;;;	   [port-name-and-direction ?port ?port-direction ?port-name]
;;;	   [has-data-type ?procedure-type ?branch-name ?port-name ?type-pattern]
;;;	   [parameter-pattern-value ?child ?procedure-type ?type-pattern ?type-constraint]
;;;	   ]
;;;  :then [port-type-constraint ?port ?type-constraint]
;;;  )


;;;(defrule assert-join-type-constraint (:forward)
;;;  :if [and [component-of ?parent ?child]
;;;	   [type-of ?child ?procedure-type]
;;;	   [join-of ?child ?join]
;;;	   (unify ?join-name (name ?join))
;;;	   [port-of ?join ?port]
;;;	   [port-name-and-direction ?port ?port-direction ?port-name]
;;;	   [has-data-type ?procedure-type ?join-name ?port-name ?type-pattern]
;;;	   [parameter-pattern-value ?child ?procedure-type ?type-pattern ?type-constraint]
;;;	   ]
;;;  :then [port-type-constraint ?port ?type-constraint]
;;;  )


(defrule create-typical-value (:forward)
  :if [and [port-of ?x ?port]
	   (not (is-known? [typical-value-of ?port ?existing-value]))
	   ]
  :then `[typical-value-of ?port ,(intern (gensym "VALUE"))])

;;; for example: the typical disk drive has max write rate of x
(defrule assume-typical-property-value (:forward)
  :if [and [type-of ?thing ?type]
	   [typical-property-value-of ?type ?property ?value]
	   [has-property ?type ?property]
	   ]
  :then (tell `[property-value-of ?thing ?property ?value]
	      :justification `(:assumption)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Reload a file of facts
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun restore-core-facts (&key (clean? nil) (pathnames (list "natsoft:code;core-facts.lisp" "natsoft:code;reductions-and-cliches.lisp")))
  (when clean? 
    (clear t nil)
    (clrhash *task-type-hash-table*))
  (ji:with-joshua-readtable 
      (let ((*package* (find-package :natsoft)))
	(cond
	 ((listp pathnames)
	  (loop for pathname in pathnames
	      do (load pathname)))
	 ((pathnamep pathnames) 
	  (load pathnames)))	
	)))

(defun is-a-type? (type-name)
  (let ((found-it nil))
    (ask* `[is-type ,type-name]
	  (setq found-it t))
    found-it))

(defun is-of-type? (putative-sub super)
  (block found-it
	(ask* `[is-of-type ,putative-sub ,super]
	      (return-from found-it t))
	nil))

(defrule check-dataflow-port-compatibilty (:forward)
  :if [dataflow-from ?flow ?source-port ?destination-port]
  :then (check-port-compatability ?flow ?source-port ?destination-port))


(defun check-port-compatability (flow source-port destination-port)
  (declare (optimize (debug 3) (speed 1)))
  (let ((source-type (port-type-constraint source-port))
	(destination-type (port-type-constraint destination-port)))
    (unless (is-of-type? source-type destination-type)
      (complain-about-port-incompatibility flow source-port destination-port
					   source-type destination-type))))

(define-predicate is-problematic (port-or-dataflow) (no-variables-in-data-mixin ltms:ltms-predicate-model))

(define-predicate incompatible-port-types (source-type source-port destination-type destination-port) 
  (no-variables-in-data-mixin ltms:ltms-predicate-model)
  )

(defrule incompatibe-types-means-problematic-dataflow (:forward)
  :if [and [dataflow-from ?flow ?source-port ?destination-port]
	   [incompatible-port-types ?source-type ?source-port ?destination-type ?destination-port]
	   ]
  :then [is-problematic ?flow]
  )

(define-predicate-method (act-on-truth-value-change is-problematic) (old-truth-value &optional old-state)
  (declare (ignore old-state))
  (with-statement-destructured (thing) self
    (let ((current-truth-value (predication-truth-value self)))
      (cond
       ((and (eql old-truth-value *unknown*)
	     (eql current-truth-value *true*))
	(setf (is-problematic? thing) t))
       ((eql current-truth-value *unknown*)
	(setf (is-problematic? thing) nil))))))

(defrule problematic-flow-means-problematic-ports (:forward)
  :if [and [is-problematic ?flow]
	   (typep ?flow 'dataflow)
	   ]
  :then `[and [is-problematic ,(input ?flow)]
	      [is-problematic ,(output ?flow)]]
  )


(defparameter *learned-facts-style* (clim:make-text-style :fix :bold :large))

(defparameter *complain-out-loud* t)
(defun complain-about-port-incompatibility (flow source-port destination-port source-type destination-type)
  (let* ((generator (make-instance 'question-generator))
	 (output (intern-entity generator "output" :definite? t :singular? t :modifier (string (name source-port))))
	 (component-1 (intern-entity generator "component" :definite? t :singular? t :modifier (string (name (task source-port)))))
	 (input (intern-entity generator "input" :definite? t :singular? t :modifier (string (name destination-port))))
	 (component-2 (intern-entity generator "component" :definite? t :singular? t :modifier (string (name (task destination-port)))))
	 (verb (intern-new-instance generator "is"))
	 (with (intern-new-instance generator "with"))
	 (compatible (intern-constant generator "compatible")))
    (create-main-clause generator output verb compatible :is-question? nil :is-imperative? nil :subject-is-wh? t)
    (make-verb-negative generator verb)
    (create-possesive generator output component-1 :is-pp? nil)
    (create-possesive generator input component-2 :is-pp? nil)
    (add-relative-clause generator verb with input)
    (let ((first (intern-entity generator "first" :definite? t :singular? t))
	  (produce (intern-new-instance generator "produce"))
	  (source-type-entity (intern-entity generator (string (if (listp source-type) (first source-type) source-type)) :definite? nil :singular? t)))
      (create-main-clause generator first produce source-type-entity :is-question? nil)
      (when (listp source-type)
	(recursively-qualify generator source-type-entity (rest source-type)))
      )
    (let ((second (intern-entity generator "second" :definite? t :singular? t))
	  (consumes (intern-new-instance generator "consume"))
	  (destination-type-entity (intern-entity generator (string (if (listp destination-type) (first destination-type) destination-type)) 
						  :definite? nil :singular? t)))
      (create-main-clause generator second consumes destination-type-entity :is-question? nil)
      (when (listp destination-type)
	(recursively-qualify generator destination-type-entity (rest destination-type))))
    (setf (current-parse generator) (nreverse (current-parse generator)))
    (setf (is-problematic? source-port) t
	  (is-problematic? destination-port) t)
    (vocalize-generator generator :vocalize? *complain-out-loud* :wait? t)
    (let ((source-type-constraint (tell `[port-type-constraint ,source-port ,source-type] :justification :none))
	  (destination-type-constraint (tell `[port-type-constraint ,destination-port ,destination-type] :justification :none))
	  (flow-assertion (tell `[dataflow-from ,flow ,source-port ,destination-port] :justification :none)))
      (tell `[incompatible-port-types ,source-type ,source-port ,destination-type ,destination-port]
	    :justification `(incompatible-types (,source-type-constraint ,destination-type-constraint ,flow-assertion))))
    (let ((stream (clim:get-frame-pane clim:*application-frame* 'learned-facts)))
      (clim:with-text-style (stream *learned-facts-style*)
	(terpri stream)
	(princ (substitute #\newline #\; (generate-text generator)) stream)))
    ))

(defparameter *created-task-counter* 0)

(defmethod insert-transducer ((flow dataflow) (source-port port) (destination-port port) source-type destination-type)
  (remove-dataflow flow)
  (let ((transducer (create-task (intern (make-symbol (format nil "TRANS-~d" (incf *created-task-counter*)))) 
				 'transducer 
				 (superior (task source-port))
				 :input-type source-type
				 :output-type destination-type)
				 ))
    (tell `[property-value-of ,transducer input-type ,source-type] :justification :premise)
    (tell `[property-value-of ,transducer output-type ,destination-type] :justification :premise)
    (let ((raw-data-port (port-named 'input 'raw-data transducer))
	  (reformatted-data-port (port-named 'output 'new-data transducer)))
      (add-dataflow source-port raw-data-port)
      (add-dataflow reformatted-data-port destination-port))
    (select-object transducer clim:*application-frame*)
    transducer
  ))

;;; This is what you do when somebody gives you a description 
;;; of a new task type

;;; Notice that this buffer is different than the one used
;;; by the top-level.  A poor man's input context

;;; Notice that we open and close the learned-facts file 3 times here.
;;; This could be optimized by holding it open longer, but I doubt it matters much.

(defun handle-new-task-type-description (new-type super-type)
  ;; for example "an imager is an input-device"
  (unless (is-a-type? super-type)
    (error "There is no type named ~a" super-type))
  (cond
   ((is-of-type? super-type 'procedure)
    (handle-new-procedure-type-description new-type super-type))
   ((is-of-type? super-type 'data-structure)
    (handle-new-data-type-description new-type super-type)
    )))

(defun handle-new-data-type-description (new-type super-type)
  (declare (ignore new-type super-type))
  )

(defun handle-new-procedure-type-description (new-type super-type)
  (multiple-value-bind (inputs outputs) (get-port-type-constraints super-type)
    (flet ((check-for-informative-type-constraint (direction name type-constraints)
	     (unless (find 'data-structure type-constraints :test-not 'eql)
	       (query-about-io-type (string new-type) (string name) :direction direction :vocalize? t)
	       (parse-text (get-response))
	       )))
      (let ((learned-facts-pane (clim:get-frame-pane *design-editor* 'learned-facts)))
	(clim:with-text-style (learned-facts-pane *learned-facts-style*)
	  (cond
	   ((and (is-a-type? new-type)
		 (is-of-type? new-type super-type))
	    (let ((string 
		   (format nil "~%I already knew that ~a is a subtype of ~a"
			   new-type super-type)))
	      (write-string string learned-facts-pane)
	      (vocalize-text string :vocalize? t :wait? t)))
	   (t (format learned-facts-pane "~%~a is a subtype of ~a" new-type super-type)
	    (tell `[is-type ,new-type])
	    (tell `[subtype ,new-type ,super-type])
	    (with-open-file (stream "natsoft:code;learned-stuff.lisp" :direction :output 
			     :if-does-not-exist :create
			     :if-exists :append)
	      ;; (print `[is-type ,new-type] stream)
	      ;; (print `[subtype ,new-type ,super-type] stream)
	    (let ((new-inputs-with-constraints nil)
		  (new-outputs-with-constraints nil))
	      (loop for (name type-constraint) in inputs
		  for response = (check-for-informative-type-constraint 'input name type-constraint)
		  when response 
		  do (push (make-type-assertion-about-io-typing response 'input name new-type) new-inputs-with-constraints))
	      (loop for (name type-constraint) in outputs
		  for response = (check-for-informative-type-constraint 'output name type-constraint)
		  when response 
		  do (push (make-type-assertion-about-io-typing response 'output name new-type) new-outputs-with-constraints))
	      (let ((new-task-description `(deftask ,new-type
					       :super-types (,super-type)
					       ;; Deftask no longer annotates the io names
					       ;; we leave that for Joshua assertions
					       :interface ((:inputs ,@new-inputs-with-constraints)
							   (:outputs ,@new-outputs-with-constraints))
					       )))
		(with-open-file (stream "natsoft:code;learned-stuff.lisp" :direction :output 
				 :if-does-not-exist :create
				 :if-exists :append)
		  (print new-task-description stream))
		;; we have to do this since deftask is a macro
		(eval new-task-description)))))))))))


(defmethod make-type-assertion-about-io-typing ((parse core-parser-mixin) direction sub-atom task-type)
  (declare (ignore direction))
  (let* ((main (get-main parse))
	 (subject (subject main))
	 (subject-name (name subject))
	 (object (object main))
	 (object-name (name object))
	 (relation (relation main))
	 (relation-name (name relation))
	 (learned-facts-pane (clim:get-frame-pane *design-editor* 'learned-facts)))
    (flet ((canonicalize-type (type)
	     (if (eql (find-singular-of-noun (convert-start-string-to-lisp-atom (name type))) 'string)
		 (let ((real-type (find-related-to type)))
		   (values real-type (name real-type)))
	       (values type (name type))))
	   (add-assertion (type)
	     (let ((type (find-singular-of-noun (convert-start-string-to-lisp-atom type)))
		   (task-is-io (is-of-type? task-type 'io-device)))
	       (when task-is-io
		 (setq type (list 'stream type)))
	       (clim:with-text-style (learned-facts-pane *learned-facts-style*)
		 (format learned-facts-pane
			 "~%The ~a port of ~a devices contains objects of type ~a"
			 sub-atom task-type type))
;;;	       (with-open-file (stream "natsoft:code;learned-stuff.lisp" :direction :output 
;;;				:if-does-not-exist :create
;;;				:if-exists :append)
;;;		 (print `[has-data-type ,task-type ,direction ,sub-atom ,type] stream))
;;;	       (tell `[has-data-type ,task-type ,direction ,sub-atom ,type])
	       (list sub-atom type))))
      (cond
       ((or (eql relation-name '|produce|) (eql relation-name '|consume|))
	(multiple-value-bind (real-object real-object-name) (canonicalize-type object)
	  (declare (ignore real-object))
	  (add-assertion real-object-name)))
       ;; If you respond with just a noun for the answer
       ;; START turns that into a question of the form "can you tell me about a <noun>"
       ((and (eql relation-name '|tell|)
	     (eql subject-name '|you|)
	     (eql object-name 'I))
	(let ((the-type (find-related-to main :connective '|about|)))
	  (multiple-value-bind (real-type real-type-name) (canonicalize-type the-type)
	    (declare (ignore real-type))
	    (add-assertion real-type-name))))
       ((eql relation-name '|is-a|)
	(multiple-value-bind (real-object real-object-name) (canonicalize-type object)
	  (declare (ignore real-object))
	  (add-assertion real-object-name)))
       ))))

;;; for handling is-a type statements
;;; There are 2 cases: <X> is a <Y> 
;;;                    <X> is a <kind-of|type-of> <Y>
;;; in the second case you need to find the related-to texp to find the type

;;; There are two types of utterances that might come through here
;;; The first describes an element of the design, e.g. "this component is an imager"
;;; The second describes a type sub-type relationship, e.g. "imagers are input-devices"
;;; To distinguish, check for singular, definite and subject is a member of {component, task, guy, thing}

(defmethod handle-is-a ((parser core-parser-mixin) &optional (texp (get-main parser)))
  (let* ((subject (subject texp))
	 (object (object texp))
	 (subject-determiner (find-related-to subject :connective '|has_det|))
	 (subject-number (find-related-to subject :connective '|has_number|))
	 (object-determiner (find-related-to object :connective '|has_det|))
	 (object-number (find-related-to object :connective '|has_number|)))
    (declare (optimize debug))
    (cond
     ((and 
       ;; i.e this that the
       (not (eql (name subject-determiner) '|indefinite|))
       (eql (name subject-number) '|singular|)
       ;; i.e. a an
       (eql (name object-determiner) '|indefinite|)
       (eql (name object-number) '|singular|)
       (member (name subject) '(|component| |task| |object| |guy|)))
      (handle-is-a-about-specific-object (first (selected-objects clim:*application-frame*)) (name object)))
     (t (handle-type-sub-type-assertion subject object))
    )))

(defmethod handle-is-a-about-specific-object ((object core-task-mixin) (type symbol))
  (let ((type-atom (find-singular-of-noun (convert-start-string-to-lisp-atom type))))
    (unless (is-a-type? type-atom)
      (confess-ignorance-about-a-type type)
      (query-about-unknown-type type)
      (let ((response (get-response)))
	;; This will take you back to the same place as if he had just said
	;; something like "imagers are input devices"
	(handle-is-a (parse-text response))))
    ;; so if what he said now makes this a type, then apply it to the selected-object
    (when (is-a-type? type-atom)
      (let ((properties (properties object)))
	(apply #'rebuild-task-for-new-type type-atom (first (selected-objects clim:*application-frame*)) properties)))))

(defmethod handle-type-sub-type-assertion ((sub-type-parse-object occurs-in-mixin) (super-type-parser-object occurs-in-mixin))
  (let ((sub-type-atom (find-singular-of-noun (convert-start-string-to-lisp-string (name sub-type-parse-object))))
	(super-type-atom (find-singular-of-noun (convert-start-string-to-lisp-string (name super-type-parser-object)))))
    (cond
     ((member super-type-atom '(type kind))
      ;; we're in case 2 
      (let ((super-type-object (find-related-to super-type-parser-object)))
	(handle-new-task-type-description sub-type-atom (convert-start-string-to-lisp-atom (name super-type-object )))))
     (t (handle-new-task-type-description sub-type-atom super-type-atom)))))

;;; the user said "replace the flow with a transducer"
(defmethod handle-replace-request ((parse core-parser-mixin) (main texp))
  (let ((new-thing (find-related-to main :connective "with")))
    (when (eql (name new-thing) '|transducer|)
      (insert-transducer-for-problematic-thing))))

;;; The user said "insert a transducer"
;;; for the moment we're begging the issue about context and just
;;; assuming he means the current flow
(defmethod handle-insert-request ((parse core-parser-mixin) (main texp))
  (let ((new-thing (object main)))
    (when (eql (name new-thing) '|transducer|)
      (insert-transducer-for-problematic-thing ))))

(defun insert-transducer-for-problematic-thing ()
  (ask* [and [is-problematic ?flow]
	     [dataflow-from ?flow ?source-port ?destination-port]
	     [incompatible-port-types ?source-type ?source-port ?destination-type ?destination-port]
	     ]
	(insert-transducer ?flow ?source-port ?destination-port ?source-type ?destination-type)))

;;; for handling statements like the A port of Module B is connected to the C port of Module D
(defmethod handle-connect ((parser core-parser-mixin) &optional (texp (get-main parser)))
  (let* ((source (object texp))
	 (source-qualifier (find-related-to source))
	 (agent (subject texp)))
    (let* ((destination (loop for possible-to-texp in (as-subject texp)
			   when (eql (name (relation possible-to-texp)) '|to|)
			    return (object possible-to-texp)))
	   (destination-qualifier (when destination (find-related-to destination))))
      (values source source-qualifier destination destination-qualifier agent))))

(defmethod handle-make-request ((parser core-parser-mixin) &optional (texp (get-main parser)))
  (let* ((thing-to-make (object texp))
	 (type-name (name thing-to-make))
	 (type (convert-start-string-to-lisp-atom type-name))
	 (name (intern (string-upcase (get-first-initials (string type-name))))))
    (add-component clim:*application-frame* 
		name
		(intern type)
		nil)
    ))

(defun get-first-initials (string)
  (loop for pos = 0 then (1+ next-pos)
      for next-pos = (or (position #\- string :start pos :test #'char-equal)
			 (position #\_ string :start pos :test #'char-equal))
      collect (subseq string pos (1+ pos)) into chars
      until (null next-pos)
      finally (return (apply #'concatenate 'string chars))))

(defmethod handle-implement-request ((parser core-parser-mixin) &optional (texp (get-main parser)))
  (declare (ignore texp))
  (loop for task in (selected-objects *design-editor*)
      do (produce-implementations task nil)))

(defmethod handle-generate-request ((parser core-parser-mixin) &optional (texp (get-main parser)))
  (declare (ignore texp))
  (let* ((design (design-in-progress *design-editor*))
	 (task-interface (abstract-task design)))
    (produce-implementations task-interface t)))

(defun port-type-constraints (port)
  (let ((answers nil))
    (ask* `[port-type-constraint ,port ?type]
	  (push ?type answers))
    answers))

(defun find-most-specific-types-of (object)
  (let ((types nil))
    (ask* `[type-of ,object ?type]
	  (unless (find ?type types :test #'(lambda (a b) (is-of-type? b a)))
	    (push ?type types))
	  (loop for thing in types
	      when (and (not (eql ?type thing)) (is-of-type? ?type thing))
	      do (setq types (delete thing types))))
    types))

;;;  Note this is dead code
;;;; (defun get-port-properties-of-object (object direction port-name)
;;;;   (let ((properties nil))
;;;;     (ask `[and [type-of ,object ?type]
;;;; 	       [has-port-property ?type ,direction ,port-name ?property-name]]
;;;; 	 #'(lambda (just)
;;;; 	     (declare (ignore just))
;;;; 	     (pushnew ?property-name properties)))
;;;;     properties))

(defun is-known? (predication)
  (ask* predication
	(return-from is-known? t))
  nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  Speed computations
;;;
;;;   Every port has a typical value
;;;   Every container has a typical element
;;;   Every stream has a typical element
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod handle-keep-up-request ((parser core-parser-mixin) main original-thing1 original-thing2)
  (declare (ignore main))
  ;; first get into the format used in the system
  (let* ((thing1 (convert-start-string-to-lisp-atom original-thing1))
	 (thing2 (convert-start-string-to-lisp-atom original-thing2))
	 (dip (design-in-progress *design-editor*))
	 (components (children dip))
	 (first-guy  (find thing1 components :key #'task-type))
	 (second-guy (find thing2 components :key #'task-type))
	 (first-guys-port nil)
	 (first-guys-port-type-constraint nil)
	 ;; (second-guys-port-type-constraint nil)
	 (second-guys-port nil))
    (when (and first-guy second-guy)
      (labels ((trace-it (next-guy his-successor initial-port)
		 (loop for first-port in (outputs next-guy)
		     when (null initial-port)
		     do (setq initial-port first-port)
		     do (loop for his-flow in (outgoing-flows first-port)
			    for second-port = (output his-flow)
			    for next-task = (task second-port) 
			    if (eql next-task his-successor)
			    do (setq first-guys-port initial-port second-guys-port second-port)
			       (return (values))
			    else do (trace-it next-task his-successor initial-port)))))
	(trace-it first-guy second-guy nil)))
    (when (and first-guys-port second-guys-port)
      (setq first-guys-port-type-constraint (port-type-constraint first-guys-port)
	    ;; second-guys-port-type-constraint (port-type-constraint second-guys-port)
	    )
      (vocalize-generator (explain-purpose "calculate" "speed" original-thing1 t) :vocalize? t :wait? t)
      (find-size-in-bytes-from-user first-guys-port (if (symbolp first-guys-port-type-constraint)
							first-guys-port-type-constraint
						      (first first-guys-port-type-constraint))
				    (if (symbolp first-guys-port-type-constraint)
					nil
				      (rest first-guys-port-type-constraint))
				    *standard-input*
				    t)
      (let* ((thing1-rate (round (port-property-value-of first-guys-port 'data-rate) 1e6))
	     (thing2-rate (property-value-of second-guy 'max-write-speed))
	     (learned-facts-pane (clim:get-frame-pane *design-editor* 'learned-facts)))
	(clim:with-text-style (learned-facts-pane *learned-facts-style*)
	  (speak-and-print learned-facts-pane (state-a-speed-estimate original-thing1 thing1-rate '|Mega-bytes| :verb "calculate" :vocalize? nil))
	  (speak-and-print learned-facts-pane (state-a-speed-estimate original-thing2 thing2-rate '|Mega-bytes| :vocalize? nil))
	  (speak-and-print learned-facts-pane (build-a-comparison thing1 thing2 (if (> thing1-rate thing2-rate) '|faster| '|slower|) :vocalize? nil))
	  (when (> thing1-rate thing2-rate)
	    (speak-and-print learned-facts-pane (make-a-suggestion "compress" (string original-thing1) "output"))
	    (let ((victim (find-matching-compressor dip)))
	      (when victim
		(rebuild-task-for-new-type 'image-stream-compressor victim)
		(select-object victim *design-editor*)
		(switch-focus *design-editor* (superior victim))))))
	))))

(defun state-a-speed-estimate (thing quantity units &key (verb "estimate") (vocalize? t))
  (let* ((generator (make-instance 'question-generator))
	 (I (intern-constant generator '|I|))
	 (estimate (intern-verb generator verb
			       :tense '|past|
			       ))
	 (speed (intern-entity generator '|speed|
					 :definite? t
					 :modifier thing
					 ))
	 (quantity (intern-constant generator quantity))
	 (is-a (intern-verb generator "is-a" :tense '|inf|))
	 (interned-units (intern-constant generator units))
	 (speed-is-clause (intern-and-add-texp generator (list speed is-a interned-units)))
	 )
    (create-main-clause generator I estimate speed-is-clause)
    (intern-and-add-texp generator (list speed-is-clause (intern-new-instance generator '|per|) (intern-new-instance generator '|second|)))
    (intern-and-add-texp generator (list interned-units (intern-new-instance generator '|has_quantity|) quantity))
    (when vocalize?
      (vocalize-generator generator :vocalize? vocalize?))
    generator
    ))

(defun build-a-comparison (thing1 thing2 comparative &key (connective '|than|) (vocalize? t))
  (let* ((generator (make-instance 'question-generator))
	 (first-guy (intern-entity generator thing1))
	 (second-guy (intern-entity generator thing2))
	 (verb (intern-verb generator "is"))
	 (adjective (intern-constant generator comparative))
	 (main-clause (create-main-clause generator first-guy verb adjective 
					  :is-imperative? nil
					  :is-question? nil :subject-is-wh? nil))
	 (has-property (intern-new-instance generator '|has_property|))
	 (than (intern-new-instance generator connective)))
    (intern-and-add-texp generator (list first-guy has-property adjective))
    (intern-and-add-texp generator (list main-clause than second-guy))
    (when vocalize?
      (vocalize-generator generator :vocalize? vocalize?))
    generator))

;;; Once you know the typical value of a port and the port type constraint
;;; apply that constraint to the typical value
;;; Fix: type-of should apply only to procedural things
;;; but here this is applying it to a data element.  Should be is-of-type?
(defrule type-typical-value (:forward)
  :if [and [typical-value-of ?port ?value]
	   [port-type-constraint ?port ?constraint]
	   ]
  :then [type-of ?value ?constraint])

;;; Create a typical element of every container
(defrule create-typical-element-of (:forward)
  :if [and [type-of ?value container]
	   (not (is-known? [typical-element-of ?value ?anything]))
	   ]
  :then `[typical-element-of ?value ,(intern (gensym "VALUE"))]
  )

;;; Create a typical element of every stream
(defrule create-stream-typical-element (:forward)
  :if [and [type-of ?value (stream ?subtype)]
	   (not (is-known? [typical-element-of ?value ?anything]))
	   ]
  :then `[typical-element-of ?value ,(intern (gensym "VALUE"))]
  )

;;; type the typical element of any stream or container
(defrule type-typical-element (:forward)
  :if [and [typical-element-of ?container ?value]
	   [type-of ?container (?container-type ?element-type)]]
  :then [type-of ?value ?element-type]
  )

;;; Type the typical value of a port that contains streams
(defrule type-typical-element-of (:forward)
  :if [and [typical-value-of ?port ?value]
	   [typical-element-of ?value ?element]
	   [type-of ?value (stream ?type)]]
  :then [type-of ?element ?type]
  )

(defrule compute-data-rate (:forward)
  :if [and [port-type-constraint ?port (stream ?stuff)]
	   [typical-value-of ?port ?port-stream]
	   [typical-element-of ?port-stream ?port-element]
	   [property-value-of ?port-stream stream-rate ?stream-rate]
	   ;; This is clearly wrong, just a hack for now
	   ;; really need to have typical object in ports
	   ;; and ask about that
	   [property-value-of ?port-element size-in-bytes ?byte-size]
	   ]
  :then `[port-property-value-of ?port data-rate ,(* ?stream-rate ?byte-size)]
  )

(defrule propagate-definition-type (:forward)
  :if [and [type-of ?thing ?type]
	   [definition ?type ?definition]]
  :then [type-of ?thing ?definition]
  )

(defrule compute-size-in-bytes-array (:forward)
  :if [and [type-of ?array array]
	   [property-value-of ?array x-dimension ?x]
	   [property-value-of ?array y-dimension ?y]
	   [typical-element-of ?array ?element]
	   [property-value-of ?element bytes-per-element ?n-bytes]
	   ]
    :then `[property-value-of ?array size-in-bytes ,(* ?x ?y ?n-bytes)]
    )

(defparameter *image-formats* '(vga xvga hd720 hd1080))

(defparameter *image-format-alist*
    '((XVGA 1024 768 1 20)
      (HD720 1280 720 4 30)
      (HD1080 1920 1080 4 30)
      ))

(defmethod find-size-in-bytes-from-user (x (type (eql 'image)) qualifiers &optional (stream *standard-input*) (top-level? nil) (ht (make-hash-table :test #'equal)))
  (flet ((do-the-work (stream)
	   (let* ((format (gethash 'format ht))
		  (x-dim (gethash (list x 'x-dimension) ht))
		  (y-dim (gethash (list x 'y-dimension) ht))
		  (typical-element (typical-element-of x))
		  (the-parent-stream (is-typical-element-of x))
		  (size-in-bytes (gethash (list typical-element 'bytes-per-element) ht)))
	     (when format
	       (unless (equal (assoc format *image-format-alist*) (list format x-dim y-dim size-in-bytes))
		 (setq format nil)))
	     (terpri stream)
	     (setq format (clim:accept `(clim:member-sequence ,*image-formats*) :stream stream :default format))
	     (setf (gethash 'format ht) format)
	     (when format
	       (destructuring-bind (width height size-in-bytes frame-rate) (rest (assoc format *image-format-alist*))
		 (setf (gethash (list x 'x-dimension) ht) width
		       (gethash (list x 'y-dimension) ht) height
		       (gethash (list typical-element 'bytes-per-element) ht) size-in-bytes
		       (gethash (list the-parent-stream 'stream-rate) ht) frame-rate
		       ))))
	   (find-size-in-bytes-from-user x 'array (cons 'pixel qualifiers) stream nil ht)))
    (if (not top-level?)
	(do-the-work stream)
      (clim:accepting-values (stream :own-window t)
	(clim:with-text-face (stream :bold)
	  (write-string "Please fill in this information" stream))
	(do-the-work stream)
	)) 
    (remhash 'format ht)
    (when top-level?
      (maphash #'(lambda (key value)
		   (destructuring-bind (object property) key
		       (tell `[property-value-of ,object ,property ,value])))
	       ht))))

(defmethod find-size-in-bytes-from-user (x (type (eql 'array)) qualifiers 
					 &optional (stream *standard-input*) (top-level? nil) (ht (make-hash-table :test #'equal)))
  (let* ((typical-element (typical-element-of x))
	 (defined-as (definition-of `(array ,@qualifiers)))
	 )
    (labels ((get-value (object property default-value stream prompt)
	       (let ((entry (gethash (list object property) ht)))
		 (unless entry 
		   (setf (gethash (list object property) ht) default-value))
		 (terpri stream)
		 (setf (gethash (list object property) ht)
		   (clim:accept 'integer
				:view clim:+textual-view+
				:prompt prompt
				:default entry
				:stream stream))))
	     (do-the-questions (stream)
	       (clim:with-text-size (stream :large)
		 (get-value x 'x-dimension 0 stream (format nil "Width of the ~a" (or defined-as 'array)))
		 (get-value x 'y-dimension 0 stream (format nil "Height of the ~a" (or defined-as 'array)))
		 (get-value typical-element 'bytes-per-element 0 stream (format nil "Number of bytes per ~a" 
										(or (first qualifiers) "array element"))))))
      (if (not top-level?)
	  (do-the-questions stream)
	(clim:accepting-values (stream :own-window t)
	  (clim:with-text-face (stream :bold)
	    (write-string "Please fill in this information" stream))
	  (do-the-questions stream)
	  )))
    (when top-level?
      (maphash #'(lambda (key value)
		   (destructuring-bind (object property) key
		     (tell `[property-value-of ,object ,property ,value])))
	       ht))))

(defun is-definition-for (type)
  (ask* `[definition ,type ?other-type]
	(return-from is-definition-for ?other-type))
  nil)

(defun is-definition-of (type)
  (ask* `[definition ?other-type ,type]
	   (return-from is-definition-of ?other-type))
  nil)

;;; Notice this only holds if nothing else does, so if it's not a definition
;;; it should probably be an error
(defmethod find-size-in-bytes-from-user (x type qualifiers &optional (stream *standard-input*)
								     (top-level? nil)
								     (ht (make-hash-table :test #'equal)))
  (let ((defined-as (is-definition-for (if qualifiers (list* type qualifiers) type))))
    (cond
     ((null defined-as) (Error "Don't know how to find size in bytes of ~a of type (~a ~{~A~^ ~}" x type qualifiers))
     ((symbolp defined-as) (find-size-in-bytes-from-user x defined-as nil stream top-level? ht))
     (t (find-size-in-bytes-from-user x (first defined-as) (rest defined-as) stream top-level? ht)))))

(defun typical-value-of (thing)
  (ask* `[typical-value-of ,thing ?value]
	(return-from typical-value-of ?value))
  nil)

(defun is-typical-value-of (thing)
  (ask* `[typical-value-of ?value ,thing]
	(return-from is-typical-value-of ?value))
  nil)

(defun typical-element-of (thing)
  (ask* `[typical-element-of ,thing ?value]
	(return-from typical-element-of ?value))
  nil)


(defun is-typical-element-of (thing)
  (ask* `[typical-element-of ?value ,thing]
	(return-from is-typical-element-of ?value))
  nil)

(defun definition-of (complex-name)
  (ask* `[definition ?defined-name ,complex-name]
	(return-from definition-of ?defined-name))
  nil)

;;; Here x is a port
(defmethod find-size-in-bytes-from-user (x (type (eql 'stream)) qualifiers 
					 &optional (stream *standard-input*) (top-level? t) (ht (make-hash-table :test #'equal)))
  
  ;;; idea is to set up the accepting-values and to ask for the stream rate 
  ;;; then call the appropriate routine for the qualifier
  (let* ((stream-of-what (first qualifiers))
	 (element-type (if (symbolp stream-of-what) stream-of-what (first stream-of-what)))
	 (element-qualifiers (if (symbolp stream-of-what) nil (rest stream-of-what)))
	 (task-type (task-type (task x)))
	 (the-stream (typical-value-of x))
	 (defined-name (definition-of `(stream-rate (stream ,@qualifiers)))))
    (flet ((body (stream)
	     (clim:with-text-size (stream :large)
	       (clim:with-text-face (stream :bold)
		 (let ((entry (gethash (list the-stream 'stream-rate) ht)))
		   (unless entry (setf (gethash (list the-stream 'stream-rate) ht) 0))
		   (setf (gethash (list the-stream 'stream-rate) ht)
		     (clim:accept 'integer 
				  :view clim:+textual-view+
				  :stream stream 
				  :default entry
				  :prompt (if defined-name
					      (format nil "~%What is the ~a of the ~a" defined-name task-type)
					    (format nil "~%At what rate does the ~a produce ~a" task-type stream-of-what))))
		   (find-size-in-bytes-from-user (typical-element-of the-stream) element-type element-qualifiers stream nil ht)
		   )))))
      (cond
       (top-level?
	(ask-to-fill-in-information t)
	(clim:accepting-values (stream :own-window t :resynchronize-every-pass t)
	  (body stream)))
       (t
	(body stream)))
      (when top-level?
	(maphash #'(lambda (key value)
		     (destructuring-bind (object property) key
		       (tell `[property-value-of ,object ,property ,value])))
		 ht)))))

(defun ask-to-fill-in-information (&optional (vocalize? t))
  (let* ((generator (make-instance 'question-generator))
	 (you (intern-constant generator '|you|))
	 (verb (intern-verb generator "fill_in"))
	 (information (intern-entity generator '|information| :definite? '|this|))
	 (main (create-main-clause generator you verb information
				   :subject-is-wh? nil
				   :is-question? nil
				   :is-imperative? t
				   )))
    (intern-and-add-texp generator
			 (list main (intern-constant generator '|is_imperative|) (intern-constant generator '|Yes|)))
    (vocalize-generator generator :vocalize? vocalize?)))


;;; The issue with the above is that each method makes its own assertions, so if you run subordinate you make assertions
;;; on every iteration of the accepting-values loop, which isn't what you want to do.  Currently fixed this by doing two
;;; accepting-values.  Better is that if you're the top level you need to bind an alist (or hashtable) that the values
;;; are kept in and only the top level does the assertions at the end.

;;; (explain-purpose '|determine| '|speed| '|disk_drive| t) -> "I am trying to determine the speed of the disk drive"

;;; Note 100MB/sec is a reasonable number for disk speed these days.
;;; 1280 x 800 is the WXGA standard
;;; 15 frames per second?
;;; Gives 61,440,000 Byte/sec
									      
(defun find-rate-reducer (&optional (top-level-type 'transducer) (property 'data-rate))
  (let ((answers nil))
    (ask* `[has-postcondition ?component-type
			      [less-than (,property (output (?output-port ?component-type))) (,property (input (?input-port ?component-type)))]
			      ]
	  (when (is-of-type? ?component-type top-level-type)
	    (push (list ?component-type ?input-port ?output-port)
		  answers)))
    answers))

(defun make-a-suggestion (verb object property-of-object)
  (let* ((generator (make-instance 'question-generator))
	 (suggest (intern-verb generator "suggest"))
	 (me (intern-constant generator "I"))
	 (you (intern-constant generator "you"))
	 (verb (intern-verb generator verb))
	 (interned-object (intern-entity generator object :definite? t))
	 (interned-property (intern-entity generator property-of-object :definite? t))
	 (sub (intern-and-add-texp generator (list you verb interned-property)))
	 (has-modal (intern-constant generator "has_modal"))
	 (will (intern-constant generator "will"))
	 (has-tense (intern-constant generator "has_tense"))
	 (past (intern-constant generator "past"))
	 (main (create-main-clause generator me suggest sub))
	 )
    (create-possesive generator interned-property interned-object)
    (intern-and-add-texp generator (list main has-tense past))	 
    (intern-and-add-texp generator (list main has-modal will))
    generator
  ))


(defgeneric matches-signature (object1 object2)
  (:method-combination and)
  )

;;; This will apply to all objects, since everything is core-task-mixin
;;; Things only match if they are the same type of thing.
(defmethod matches-signature and ((object1 core-task-mixin) (object2 core-task-mixin))
  (eql (common-lisp:type-of object1) (common-lisp:type-of object2)))
  
(defmethod matches-signature and ((object1 branching-task) (object2 branching-task))
  (let ((branches1 (branches object1))
	(branches2 (branches object2)))
    (and (= (length branches1) (length branches2))
	 (loop for branch1 in branches1
	     for name1 = (name branch1)
	     for branch2 = (branch-named name1 object2)
	     always (and branch2
			 (matches-signature branch1 branch2))
		    )))  
  )

(defmethod matches-signature and ((object1 joining-task) (object2 joining-task))
  (let ((joins1 (joins object1))
	(joins2 (joins object2)))
    (and (= (length joins1) (length joins2))
	 (loop for join1 in joins1
	     for name1 = (name join1)
	     for join2 = (join-named name1 object2) 
	     always (and join2
			 (matches-signature join1 join2))
		    )))
  )

(defmethod matches-signature and ((object1 input-side-mixin) (object2 input-side-mixin))
  (let ((inputs1 (inputs object1))
	(inputs2 (inputs object2)))
    (and (= (length inputs1) (length inputs2))
	 (loop for input1 in inputs1
	     for name1 = (name input1)
	     for type-constraint-1 = (port-type-constraint input1)
	     for input2 = (port-named 'input name1 object2)
	     always (and input2
			 (equal (port-type-constraint input2) type-constraint-1)))))
  )

(defmethod matches-signature and ((object1 output-side-mixin) (object2 output-side-mixin))
  (let ((outputs1 (outputs object1))
	(outputs2 (outputs object2)))
    (and (= (length outputs1) (length outputs2))
	 (loop for output1 in outputs1
	     for name1 = (name output1)
	     for type-constraint-1 = (port-type-constraint output1)
	     for output2 = (port-named 'output name1 object2)
	     always (and output2
			 (equal (port-type-constraint output2) type-constraint-1)))))
  )

(defun find-matching-component (target-component top-level)
  (map-over-selected-implementation top-level
				    #'(lambda (sub-task history implementation)
					(declare (ignore history implementation))
					(when (matches-signature sub-task target-component)
					  (return-from find-matching-component sub-task))))
  nil
  )

(defun find-matching-compressor (top-level)
  (let* ((new-object (create-task 'foo 'image-stream-compressor nil '(output-type (sequence byte))))
	 (the-match (loop for component in (children top-level)
			for target = (find-matching-component new-object component)
			when target return target)))
    the-match
    ))


(defmethod handle-locate-component-request ((parser core-parser-mixin) main verb object qualifier)
  (declare (ignore main))
  ;; Total wind up toy after here
  (when (and (eql verb '|compress|)
	     (eql object '|streams|))
    (when (eql qualifier '|video|)
      (let* ((learned-facts-pane (clim:get-frame-pane *design-editor* 'learned-facts)))
	(clim:with-text-style (learned-facts-pane *learned-facts-style*)
	  (speak-and-print learned-facts-pane (build-can-be-done-using-statement "Java_Media_Framework" "build" "component" "video_compession" t))
	  (speak-and-print learned-facts-pane (build-try-sentence "find" "component" "video_compression"))
  )))))

(defun build-can-be-done-using-statement (agent verb object &optional object-modifier agent-definite)
  (let* ((generator (make-instance 'question-generator))
	 (somebody (intern-constant generator "somebody"))
	 (interned-agent (intern-entity generator agent :definite? agent-definite))
	 (verb (intern-verb generator verb :voice '|passive| :modality '|can|))
	 (interned-object (intern-entity generator object :definite? nil))
	 (main (create-main-clause generator somebody verb interned-object)))
    (when object-modifier (create-modification generator interned-object object-modifier))
    (create-using-clause generator main interned-agent)
    generator))

(defun build-try-sentence (verb object &optional object-modifier)
  (let* ((generator (make-instance 'question-generator))
	 (me (intern-constant generator "I"))
	 (interned-verb (intern-verb generator verb :tense '|to|))
	 (interned-object (intern-entity generator object :definite? nil))
	 (try (intern-verb generator "try" :modality '|will|))
	 (clause (intern-and-add-texp generator (list me interned-verb interned-object))))
    (create-main-clause generator me try clause)    
    (when object-modifier (create-modification generator interned-object object-modifier))
    generator))
