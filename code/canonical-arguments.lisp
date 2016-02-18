;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Package: natsoft; readtable: Joshua -*-

(in-package :natsoft)


;;; Rationale:
;;; The reason that this passes on both the original expression of the input and output types
;;; not just the canonical ones:
;;; The original ones might refer to something that needs to be viewed as something else (e.g. an array as a vector of rows)
;;; and somewhere in the process we want to record that we reduced from one to the other.
;;;
;;; As a consequence Reductions (i.e. the planning operators) take both canonical and as expressed arguments
;;; but the definitions of a plan-structure need not.

(defmethod canonical-arguments-for ((type (eql 'transducer)) the-transducer)
  (flet ((get-one-property (property-name)
	   (ask `[property-value-of ,the-transducer ,property-name ?answer]
		#'(lambda (just) 
		    (declare (ignore just)) 
		    (return-from get-one-property (values (canonical-form-for ?answer) ?answer))))))
    (multiple-value-bind (canonical-input-type input-type) (get-one-property 'input-type)
      (multiple-value-bind (canonical-output-type output-type) (get-one-property 'output-type)
	(List canonical-input-type canonical-output-type input-type output-type
	      (name (first (inputs the-transducer)))
	      (name (first (outputs the-transducer)))
	      )))))

(defmethod canonical-arguments-for ((type (eql 'branching-transducer)) the-transducer)
  (flet ((get-one-property (property-name)
	   (ask `[property-value-of ,the-transducer ,property-name ?answer]
		#'(lambda (just) 
		    (declare (ignore just)) 
		    (return-from get-one-property (values (canonical-form-for ?answer) ?answer))))))
    (multiple-value-bind (canonical-input-type input-type) (get-one-property 'input-type)
      (multiple-value-bind (canonical-output-type output-type) (get-one-property 'output-type)
	(List canonical-input-type canonical-output-type input-type output-type
	      (name (first (inputs the-transducer)))
	      (name (first (outputs (branch-named 'more the-transducer))))
	      )))))

;;; because of final branches and initial joins these need to be a bit different
(defmethod canonical-arguments-for ((type (eql 'generator)) the-generator)
  (flet ((get-one-property (property-name)
	   (ask `[property-value-of ,the-generator ,property-name ?answer]
		#'(lambda (just) 
		    (declare (ignore just)) 
		    (return-from get-one-property (values (canonical-form-for ?answer) ?answer))))))
    (multiple-value-bind (canonical-input-type input-type) (get-one-property 'input-type)
      (multiple-value-bind (canonical-output-type output-type) (get-one-property 'output-type)
	(List canonical-input-type 
	      canonical-output-type
	      input-type		;before canonicalization
	      output-type		;ditto
	      (name (first (inputs the-generator))) ;input-name
	      (name (first (outputs (branch-named 'more the-generator)))) ;output-name
	      )))))

(defmethod canonical-arguments-for ((type (eql 'collector)) the-collector)
  (flet ((get-one-property (property-name)
	   (ask `[property-value-of ,the-collector ,property-name ?answer]
		#'(lambda (just) 
		    (declare (ignore just)) 
		    (return-from get-one-property (values (canonical-form-for ?answer) ?answer))))))
    (multiple-value-bind (canonical-input-type input-type) (get-one-property 'input-type)
      (multiple-value-bind (canonical-output-type output-type) (get-one-property 'output-type)
	  (List canonical-input-type canonical-output-type input-type output-type
	      (name (first (inputs (join-named 'more the-collector))))
	      (name (second (inputs (join-named 'more the-collector))))
	      )))))

(defmethod canonical-arguments-for ((type (eql 'detect-duplicates)) the-detector)
  (let (canonical-input-type canonical-output-type canonical-key-extractor-type)
    (flet ((get-one-property (property-name)
	     (ask `[property-value-of ,the-detector ,property-name ?answer]
		  #'(lambda (just) 
		      (declare (ignore just)) 
		      (return-from get-one-property (values (canonical-form-for ?answer) ?answer))))))
      (multiple-value-setq (canonical-output-type) (get-one-property 'output-type))
      (multiple-value-setq (canonical-key-extractor-type) (get-one-property 'key-extractor-type))
      (multiple-value-setq (canonical-input-type) (get-one-property 'input-type))
      (list canonical-input-type canonical-output-type canonical-key-extractor-type))))
    
(defmethod canonical-arguments-for ((type (eql 'tree-traverse)) the-traverser)
  (let (canonical-set-type canonical-recursive-call-name canonical-key-type)
    (flet ((get-one-property (property-name)
	     (ask `[property-value-of ,the-traverser ,property-name ?answer]
		  #'(lambda (just) 
		      (declare (ignore just)) 
		      (return-from get-one-property (values (canonical-form-for ?answer) ?answer))))))
      (multiple-value-setq (canonical-set-type) (get-one-property 'set-type))
      (multiple-value-setq (canonical-recursive-call-name) (get-one-property 'recursive-call-name))
      ;; (multiple-value-setq (canonical-element-type) (get-one-property 'element-type))
      (multiple-value-setq (canonical-key-type) (get-one-property 'key-type))
      (list canonical-set-type canonical-recursive-call-name canonical-key-type))))

;;; Both of these might be capable of simplification
(defmethod canonical-arguments-for ((type (eql 'enumerator)) the-enumerator)
  (flet ((get-one-property (property-name)
	   (ask `[property-value-of ,the-enumerator ,property-name ?answer]
		#'(lambda (just) 
		    (declare (ignore just)) 
		    (return-from get-one-property (values (canonical-form-for ?answer) ?answer))))))
    (multiple-value-bind (canonical-input-type input-type) (get-one-property 'set-type)
      (multiple-value-bind (canonical-output-type output-type) (get-one-property 'element-type)
	(multiple-value-bind (canonical-key-type key-type) (get-one-property 'key-type)
	  (declare (ignore key-type))
	  (List canonical-input-type 
		canonical-output-type
		input-type		;before canonicalization
		output-type		;ditto
		(name (first (inputs the-enumerator))) ;input-name
		(name (first (outputs (branch-named 'more the-enumerator)))) ;output-name
		canonical-key-type
		))))))

(defmethod canonical-arguments-for ((type (eql 'enumerator-with-keys)) the-enumerator)
  (flet ((get-one-property (property-name)
	   (ask `[property-value-of ,the-enumerator ,property-name ?answer]
		#'(lambda (just) 
		    (declare (ignore just)) 
		    (return-from get-one-property (values (canonical-form-for ?answer) ?answer))))))
    (multiple-value-bind (canonical-input-type input-type) (get-one-property 'set-type)
      (multiple-value-bind (canonical-output-type output-type) (get-one-property 'element-type)
	(multiple-value-bind (canonical-key-type key-type) (get-one-property 'key-type)
	  (declare (ignore key-type))
	  (List canonical-input-type 
		canonical-output-type
		input-type		;before canonicalization
		output-type		;ditto
		(name (first (inputs the-enumerator))) ;input-name
		(name (first (outputs (branch-named 'more the-enumerator)))) ;output-name
		canonical-key-type
		))))))

;;; Now a hack for implementing extract, this will be a cheat for a while wired to the needs of the json tree example.

(defmethod canonical-arguments-for ((type (eql 'extractor)) the-enumerator)
  (flet ((get-one-property (property-name)
	   (ask `[property-value-of ,the-enumerator ,property-name ?answer]
		#'(lambda (just) 
		    (declare (ignore just)) 
		    (return-from get-one-property (values (canonical-form-for ?answer) ?answer))))))
    (multiple-value-bind (input-type original-input-type) (get-one-property 'input-type)
      (declare (ignore original-input-type))
      (multiple-value-bind (output-type original-output-type) (get-one-property 'output-type)
	(declare (ignore original-output-type))
	(list input-type output-type)
	))))

(defmethod canonical-arguments-for ((type (eql 'enumerate-filter-accumulate)) the-e-f-a)
  (let (input-type output-type enumerator-type filter-type accumulator-type)
    (flet ((get-one-property (property-name)
	     (ask* `[property-value-of ,the-e-f-a ,property-name ?answer]
		   (return-from get-one-property (values (canonical-form-for ?answer) ?answer)))))
      (multiple-value-setq (input-type) (get-one-property 'input-type))
      (multiple-value-setq (output-type) (get-one-property 'output-type))
      (multiple-value-setq (enumerator-type) (get-one-property 'enumerator-type))
      (multiple-value-setq (filter-type) (get-one-property 'filter-type))
      (multiple-value-setq (accumulator-type) (get-one-property 'accumulator-type))
      (list input-type enumerator-type filter-type accumulator-type output-type)
      )))

;;; This is the demo

(defmethod canonical-arguments-for ((type (eql 'jdd)) the-dup-detector)
  (declare (ignore the-dup-detector))
  nil)