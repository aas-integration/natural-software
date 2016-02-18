;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Package: natsoft; readtable: Joshua -*-

(in-package :natsoft)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; The actual planning and cliche knowledge
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;; input-type is (container-type input-element-type)
;;; There is an overall pattern that we should follow:
;;;  GENERATORS take objects as inputs, and produce sequences as outputs
;;;   So if two generators occur in a row, they have to be separated by a TAKE
;;;  ACCUMULATORS take sequences as inputs and produce streams as outputs

;;; this says find a plan for the viewpoint
;;; This uses the full capability of the defreduction macro, calling a subplan and accumulating choices made
;;; The original input and output types are what the query had before it was canonicalized (definitions replacing terms as in image -> (array pixel))

(defreduction view-as-sequence (generator (?input-type 
					   (temporal-sequence ?output-type)
					   ?original-input-type 
					   ?original-output-type 
					   ?input-name 
					   ?output-name))
  :the-instance ?the-generator
  :the-plan ?the-plan
  :prerequisites ((not (is-of-type? ?input-type 'temporal-sequence))
		  (is-of-type? ?output-type 'primitives)
		  [can-be-viewed-as ?input-type ?view-type ?viewpoint]
		  (is-of-type? ?view-type 'sequence))
  :subplans ((subplan :type generator
		      :instance ?the-generator
		      :parameters ((?viewpoint ?input-type)
				   (temporal-sequence ?output-type)
				   ?original-input-type ?original-output-type 
				   ?input-name ?output-name)
		      :previous-choices ((viewpoint ?viewpoint) . ?choices-so-far)
		      :new-choices ?choices
		      :sub-plan ?the-sub-plan
		      ))
  :reduce-to ?the-sub-plan
  :previous-choices ?choices-so-far
  :new-choices ?choices
  :actions ((tell [viewpoint-applied ?the-generator ?input-name ?viewpoint generate ?original-output-type ?original-input-type]))
  )

;;; Here Orginal-<input/output>-type is what is expressed in the design before cononicalization
;;; while <input/output>-container-type is what it might be canonicalized to
;;; for example (sequence image) vs. (sequence (array pixel))
;;; where array is the container-type and pixel is the element-type
;;; This is taking 2 jumps at once.
;;; First is generate accumulate: e.g. generator of stream of element-type plus accumlate element-type
;;; Second is implementation of the generator

;;; NOTE: Idea is to split this into two steps
;;;  First is to a transducer and an collector
;;;  Second is To reduce that transducr to a take and generate.
;;;  Caveat: make sure that we don't come back here for the 2nd refinement.
(defreduction reduce-to-element (transducer ((stream (?input-container-type ?input-element-type))
					     (stream (?output-container-type ?output-element-type))
					     ?original-input-type ?original-output-type
					     ?input-name ?output-name))
  :reduce-to (generate-and-collect  ?original-input-type ?input-name
				       ?original-output-type ?output-container-type ?output-element-type ?output-name
				       )
  :prerequisites ((is-of-type? ?input-container-type 'container)
		  (is-of-type? ?output-container-type 'container)
		  ;; we'll get viewpoints other ways
		  (not (is-viewpoint? (list ?input-container-type ?input-elemet-type))))
  )


(defreduction take-and-generate (branching-transducer ((stream (?input-container-type ?input-element-type))
						       (temporal-sequence ?output-element-type)
						       ?original-input-type ?original-output-type
						       ?input-name ?output-name))
  :reduce-to (take-and-generate-elements 'stream ?original-input-type ?input-container-type ?input-element-type ?input-name
					 ?output-element-type ?output-name
					 )
  :prerequisites (;; we'll get viewpoints other ways
		  ;; (not (is-viewpoint? (list ?input-container-type ?input-element-type)))
		  )
  )
 
;;; Notice that this takes a vector
;;; and generates its elements
(defreduction vector-to-elements (generator (?input-type (temporal-sequence ?output-type) ?original-input-type ?original-output-type ?input-name ?output-name))
  :reduce-to (enumerate-vector-elements ?input-type ?output-type ?input-name ?output-name 'indices)
  ;; I'm doing this so that I can use subtypes of vectors (e.g. columns)
  ;; rather than pattern matching on the :then pattern for (vector ?output-type)
  ;; which would only work if it was explictly a vector
  ;; This makes viewing an array as a vector of vectors work!
  :prerequisites ((is-of-type? ?input-type 'vector)
		  [property-value-of ?input-type element-type ?sub-type]
		  (is-of-type? ?sub-type ?output-type)
		  )
  )

(defreduction vector-to-elements-2 (generator (?input-type (temporal-sequence ?output-type)
							   ?original-input-type ?original-output-type
							   ?input-name ?output-name))
  :reduce-to (enumerate-elements-then-bridge ?input-type ?output-type ?intermediate-type ?input-name ?output-name)
  :prerequisites ((is-of-type? ?input-type 'vector)
		  [property-value-of ?input-type element-type ?intermediate-type]
		  [not [unify ?intermediate-type ?output-type]]
		  )
  )


(defreduction implement-collector (collector (?input-type (stream ?output-type)
							      ?original-input-type 
							      (stream ?original-output-type)
							      ?input-name ?output-name))
  :reduce-to (buffer-and-ship ?input-type ?original-output-type ?input-name ?output-name )
  :the-instance ?the-collector
  :prerequisites ((is-of-type? ?output-type 'vector)
		  [property-value-of ?output-type element-type ?element-type]
		  (is-of-type? ?input-type ?element-type)
		  )
  )

;;; Original-input-type: type of the initial input
;;; Original-output-type: external type of the output
;;; Output-container-type: internal container-type of the output
;;; Output-element-type: internal element-type of the output
;;; E.g. (stream image), foo, (stream disk-buffer), vector, byte
(defcliche generate-and-collect (?original-input-type ?input-name
				     ?original-output-type ?output-container-type ?output-element-type ?output-name)
  :initial-inputs (;; this is the container of input stuff
		   (:name ?input-name :type ?original-input-type)
		   ;; this is the output queue
		   (:name ?output-name :type (stream (?output-container-type ?output-element-type)))
		   )
  :final-outputs ((:name chunks :port ?output-name :type ?original-output-type))
  :components ((:name tr :type branching-transducer :input-type ?original-input-type :output-type (temporal-sequence ?output-element-type))
	       (:name acc-1 :type collector :input-type ?output-element-type :input-container-type temporal-sequence :output-type ?original-output-type))
  :dataflows (((:component ?input-name :port ?input-name ) (:component tr :port raw-data))
	      ((:component tr :branch more :port new-data) (:component acc-1 :branch more :port new-data))
	      ((:component ?output-name :port ?output-name) (:component acc-1 :branch more :port queue))
	      ((:component ?output-name :port ?output-name) (:component acc-1 :branch empty :port queue))
	      ((:component acc-1 :port chunks) (:component chunks :port ?output-name))
	      )
  :control-flows (((:component tr :branch empty) (:component acc-1 :branch empty)))
  )

;;; Sequence-type: The input-container-type of the take
;;; Original-input-type: Full type description of the input
;;; Input-container-type: Container-type of the elements of the input to the take
;;; Input-element-type:   Element-type   of the elements of input to the take
;;; Output-element-type: Type of whatever comes out
;;; E.g. stream, (stream image), array, pixel, foo pixel, bar
;;; original-input-type is subtype of (sequence-type (input-container-type input-element-type))
(defcliche take-and-generate-elements (?sequence-type ?original-input-type
					 ?input-container-type ?input-element-type ?input-name
					 ?output-element-type ?output-name
					 )
  :initial-inputs ((:name ?input-name :type ?original-input-type))
  :components ((:name t-1 :type take :element-type (?input-container-type ?input-element-type) :input-container-type ?sequence-type)
	       (:name gen :type generator :input-type (?input-container-type ?input-element-type) :output-type (temporal-sequence ?output-element-type)))
  :exit-points ((:name more :ports ((:name ?output-name :type (temporal-sequence ?output-element-type))))
		(:name empty))
  :dataflows (((:component ?input-name :port ?input-name) (:component t-1 :port sequence-data))
	      ((:component t-1 :port data) (:component gen :port container))
	      ((:component gen :branch more :port new-data) (:component more :port ?output-name)))
  :control-flows (((:component gen :branch empty) (:component empty)))
  )

;;; Input-type: (vector of something)
;;; output-type: the something and the indices
;;; E.g. (vector integer), number, foo,  bar
(defcliche enumerate-vector-elements (?input-type ?output-type ?input-name ?output-name ?indices-name)
  :initial-inputs ((:name ?input-name :type ?input-type))
  :state-elements ((:direction source :name s-1 :port-name old-stream :port-type (temporal-sequence ?output-type) :state old-stream)
		   (:direction source :name s-2 :port-name old-indices :port-type (temporal-sequence integer) :state old-indices-stream)
		   )
  :exit-points ((:name more :ports ((:name ?output-name :type (temporal-sequence ?output-type))
				    (:name ?indices-name :type (temporal-sequence integer))
				    ))
		(:name empty))
  :components ((:name length :type vector-length :vector-type ?input-type)
	       (:name enum :type index-enumerator  :index-type integer)
	       (:name t-1 :type Take :element-type integer :input-container-type temporal-sequence)
	       (:name va :type vector-accessor :element-type ?output-type)
	       (:name p-1 :type put :element-type ?output-type :output-container-type temporal-sequence)
	       (:name p-2 :type put :element-type integer :output-container-type temporal-sequence)
	       )
  :dataflows (((:component ?input-name :port ?input-name) (:component length :port vector))
	      ((:component length :port length) (:component enum :port bound))
	      ((:component enum :branch more :port indices) (:component t-1 :port sequence-data))
	      ((:component ?input-name :port ?input-name) (:component va :port vector))
	      ((:component t-1 :port data) (:component va :port index))
	      ((:component t-1 :port data) (:component p-2 :port data))
	      ((:component va :port the-element) (:component p-1 :port data))
	      ((:component s-1 :port old-stream) (:component p-1 :port sequence))
	      ((:component s-2 :port old-indices) (:component p-2 :port sequence))
	      ((:component p-1 :port sequence-data) (:component more :port ?output-name))
	      ((:component p-2 :port sequence-data) (:component more :port ?indices-name))
	      )
  :control-flows (((:component enum :branch empty) (:component empty)))
  )

(defcliche enumerate-elements-then-bridge (?input-type ?output-type ?intermediate-type ?input-name ?output-name)
  :initial-inputs ((:name ?input-name :type ?input-type))
  :components ((:name gen-1 :type generator :input-type ?input-type :output-type (temporal-sequence ?intermediate-type))
	       (:name t-1 :type take :element-type ?intermediate-type :input-container-type temporal-sequence)
	       (:name gen-2 :type generator :input-type ?intermediate-type :output-type (temporal-sequence ?output-type)))
  :exit-points ((:name more :ports ((:name ?output-name :type (temporal-sequence ?output-type))))
		(:name empty))
  :dataflows (((:component ?input-name :port ?input-name) (:component gen-1 :port container))
	      ((:component gen-1 :branch more :port new-data) (:component t-1 :port sequence-data))
	      ((:component t-1 :port data) (:component gen-2 :port container))
	      ((:component gen-2 :branch more :port new-data) (:component more :port ?output-name)))
  :control-flows (((:component gen-1 :branch empty) (:component empty)))
  )

;;; Input-type: The element-type of the stream of stuff coming in
;;; Output-type: The container-type into which the input elements are accumulated
;;; E.g. Byte, Disk-buffer, foo, bar
(defcliche buffer-and-ship (?input-type ?output-type ?input-name ?output-name)
  ;; First the initialization of the local state
  :state-elements ((:direction source :name s-1 :port-name buffer :port-type ?output-type :state buffer)
		   (:direction sink :name s-2 :port-name buffer :port-type ?output-type :state buffer))
  ;; Next we have two ways to enter this routine:
  ;; 1) Normal path with a data input and a queue: More
  ;; 2) When we've run out of inputs and a queue:  Empty
  ;; Here's the normal pathway
  :entry-points ((:name more :ports ((:name ?input-name :type (temporal-sequence ?input-type)) (:name ?output-name :type (stream ?output-type))))
		 (:name empty :ports ((:name ?output-name :type (stream ?output-type)))))
  :components (;; The stuff left pathway
	       (:name initial-buffer :type allocate :object-type ?output-type)
	       (:name t-1 :type take :element-type ?input-type :input-container-type temporal-sequence)
	       (:name vpush :type vector-push  :vector-type ?output-type :element-type ?input-type)
	       (:name full? :type vector-full-test :vector-type (vector ?input-type))
	       (:name p-1 :type put :element-type ?output-type :output-container-type stream)
	       (:name new-buffer :type allocate :object-type ?output-type)
	       ;; Now for the empty pathway
	       (:name empty? :type vector-empty-test :vector-type (vector ?input-type))
	       (:name p-2 :type put :element-type ?output-type :output-container-type stream)
	       (:name sj :type simple-join :object-type (stream disk-buffer) :first-join-name normal :second-join-name empty))
  :final-outputs ((:name ?output-name :port chunks :type (stream ?output-type)))
  :dataflows (((:component initial-buffer :port new-object) (:component s-1 :port initial-buffer))
	      ((:component more :port ?input-name) (:component t-1 :port sequence-data))
	      ((:component t-1 :port data) (:component vpush :port new-data))
	      ((:component s-1 :port buffer) (:component vpush :port vector))
	      ((:component vpush :port updated-vector) (:component full? :port the-vector))
	      ((:component new-buffer :port new-object) (:component s-2 :port buffer))
	      ((:component vpush :port updated-vector) (:component p-1 :port data))
	      ((:component more :port ?output-name) (:component p-1 :port sequence))
	      ((:component s-1 :port buffer) (:component empty? :port the-vector))
	      ((:component s-1 :port buffer) (:component p-2 :port data))
	      ((:component empty :port ?output-name) (:component p-2 :port sequence))
	      ((:component p-1 :port sequence-data) (:component sj :branch normal :port the-object))
	      ((:component p-2 :port sequence-data) (:component sj :branch empty :port the-object))
	      ((:component sj :port the-object) (:component ?output-name :port chunks))
	      )
  :control-flows (((:component full? :branch full) (:component p-1))
		  ((:component p-1) (:component new-buffer))
		  ((:component empty) (:component empty?))
		  ((:component empty? :branch has-stuff) (:component p-2))
		  )
  )

(defreduction dd-as-list (detect-duplicates (?input-type ?output-type ?key-extractor-type))
  :reduce-to (detect-duplicates ?input-type ?output-type 'list ?key-extractor-type)
  :the-instance ?the-detector
  :prerequisites ()
  )

(defcliche detect-duplicates (?input-type ?output-type ?seen-set-type ?key-extractor-type)
  :bindings ((?real-input-type (or (definition-for ?input-type) ?input-type))
	     (?element-type (second ?real-input-type))
	     (?real-output-type (or (definition-for ?output-type) ?output-type))
	     (?output-set-type (first ?real-output-type))
	     (?key-type (second ?real-output-type)))
  :initial-inputs (
                   (:name THE-SEQUENCE :type ?input-type)
                   )
  :final-outputs (
                  (:name ACCUMULAND :type (?output-set-type ?key-type))
                  )
  :state-elements (
                   (:name EXISTING-DUPS :direction source :port-name EXISTING-DUPS :port-type (?output-set-type ?key-TYPE) :state dups :locality local)
                   (:name EXISTING-SEEN :direction source :port-name EXISTING-SEEN :port-type (?seen-set-type ?key-TYPE) :state seen :locality local)
		   (:name UPDATED-DUPS :direction sink :port-name UPDATED-DUPS :port-type (?output-set-type ?key-TYPE) :state dups)
		   (:name UPDATED-SEEN :direction sink :port-name UPDATED-SEEN :port-type (?seen-set-type ?key-TYPE) :state seen)
                   )
  :components ((:name enum-elements :type list-enumerator :element-type ?element-type) 
	       (:name TAKE-ONE :type TAKE :INPUT-CONTAINER-TYPE temporal-sequence :ELEMENT-TYPE ?element-type)
	       (:name key-extractor :type ?key-extractor-type :input-type ?element-type)
	       (:name initial-seen :type allocate :object-type (?seen-set-type ?key-type))
	       (:name initial-dups :type allocate :object-type (?output-set-type ?key-type))
               (:name ADD-TO-SEEN :type ADD-TO-SET :SET-TYPE ?seen-set-type :ELEMENT-TYPE ?key-TYPE :unique? nil)
               (:name SEEN? :type MEMBER-TEST :SET-TYPE ?seen-set-type :ELEMENT-TYPE ?key-TYPE)
               (:name ADD-TO-DUPS :type ADD-TO-SET :SET-TYPE ?output-set-type :ELEMENT-TYPE ?key-TYPE :unique? t)
               )
  :dataflows (
	      ((:component initial-seen :port new-object) (:component existing-seen :port initial-existing-seen))
	      ((:component initial-dups :port new-object) (:component existing-dups :port initial-existing-dups))
              ((:component EXISTING-DUPS :port EXISTING-DUPS) (:component ADD-TO-DUPS :port THE-SET))
	      ((:component TAKE-ONE :port data) (:component key-extractor :port the-container))
	      ((:component key-extractor :port the-part) (:component ADD-TO-DUPS :port THE-ELEMENT))
              ((:component EXISTING-SEEN :port EXISTING-SEEN) (:component SEEN? :port THE-SET))
              ((:component EXISTING-SEEN :port EXISTING-SEEN) (:component ADD-TO-SEEN :port THE-SET))
	      ((:component key-extractor :port the-part) (:component ADD-TO-SEEN :port THE-ELEMENT))
	      ((:component ADD-TO-DUPS :port THE-SET) (:component updated-DUPS :port updated-dups))
	      ((:component existing-DUPS :port existing-dups) (:component ACCUMULAND :port ACCUMULAND))
	      ((:component ADD-TO-SEEN :port THE-SET) (:component updated-SEEN :port updated-seen))
              ((:component key-extractor :port the-part) (:component SEEN? :port THE-ELEMENT))
              ((:component THE-SEQUENCE :port THE-SEQUENCE) (:component enum-elements :port the-list))
	      ((:component enum-elements :port list-elements :branch more) (:component take-one :port sequence-data))
              )
  :control-flows (((:component enum-elements :branch empty) (:component accumuland))
                  ((:component SEEN? :branch NO) (:component ADD-TO-SEEN))
                  ((:component SEEN? :branch YES) (:component ADD-TO-DUPS))
                  )
  )

(defreduction tree-traverse-reduce (tree-traverse (?node-type ?recursive-call-name ?key-type))
  :reduce-to (tree-traverse ?node-type (non-terminal-of-tree-type ?node-type) (terminal-of-tree-type ?node-type) ?recursive-call-name ?key-type)
  :the-instance ?the-tree
)

;;; This should maybe take parameters for the input and output names
(defcliche tree-traverse (?Node-type ?non-terminal-node-type ?terminal-node-type ?recursive-call-name ?key-type)
  :initial-inputs ((:name the-set :type ?node-type))
  :state-elements ((:direction source :name node-sequence :port-name node-sequence :port-type (temporal-sequence ?node-type) :state the-sequence))
  :exit-points ((:name more :ports ((:name the-elements :type (temporal-sequence ?node-type))))
		(:Name empty))
  ;; :final-outputs ((:name the-sequence :type (temporal-sequence ?node-type)))
  :components ((:name add-to-sequence :type put :element-type ?node-type :output-container-type temporal-sequence)
               (:name non-terminal? :type type-split :input-type ?node-type :output-type ?non-terminal-node-type :non-matching-output-type ?terminal-node-type)
               (:name enumerate-nodes :type enumerator-with-keys :set-type ?non-terminal-node-type :element-type ?node-type :key-type ?key-type)
               (:name get-node :type take :input-container-type temporal-sequence :element-type ?node-type)
               (:name ?recursive-call-name :type tree-traverse :set-type ?node-type :element-type ?node-type :key-type ?key-type
		      :dont-expand t :recursive-call-name ?recursive-call-name)
               ;; (:name join :type simple-join :object-type (temporal-sequence ?node-type) :first-join-name non-primitive :second-join-name terminal)
	       (:name exit-join :type control-JOIN :FIRST-JOIN-NAME enumerate-nodes :second-join-name traverse)
               )
  :dataflows (((:component node-sequence :port node-sequence) (:component add-to-sequence :port sequence))
              ((:component the-set :port the-set) (:component add-to-sequence :port data))
              ((:component the-set :port the-set) (:component non-terminal? :port test-data))
              ((:component non-terminal? :port matching-data :branch match) (:component enumerate-nodes :port the-set))
              ((:component enumerate-nodes :port the-elements :branch more) (:component get-node :port sequence-data))
              ((:component get-node :port data) (:component ?recursive-call-name :port the-set))
              ;; ((:component traverse :port the-elements :branch more) (:component join :port the-object :branch non-primitive))
	      ((:component add-to-sequence :port sequence-data) (:component more :port the-elements))
              ;; ((:component add-to-sequence :port sequence-data) (:component join :port the-object :branch terminal))
              ;; ((:component join :port the-object) (:component more :port the-elements)
	      )              
  :control-flows (((:component enumerate-nodes :branch empty) (:component exit-join :branch enumerate-nodes))
		  ;; this is to make acting on nodes happen before recursion
		  ((:component add-to-sequence) (:component non-terminal?))
		  ((:component ?recursive-call-name :branch empty) (:component exit-join :branch traverse))
		  ((:component exit-join) (:component empty)))
  )

(defreduction enumerate-subtypes (enumerator-with-keys (?input-node-type ?output-node-type ?original-input-type ?original-output-type ?input-name ?output-name 
							       ?key-type))
  :reduce-to (split-enumerate ?input-node-type ?first-sub-type ?second-sub-type ?output-node-type ?input-name ?output-name ?key-type)
  :the-instance ?the-tree
  :prerequisites ((let ((sub-types (union (union-members ?input-node-type) (sub-types ?input-node-type))))
		   (and (= (length sub-types) 2)
			(unify (first sub-types) ?first-sub-type)
			(unify (second sub-types) ?second-sub-type))))
		  )

(defcliche split-enumerate (?node-type ?first-sub-type ?second-sub-type ?output-node-type ?input-name ?output-name ?key-type)
  :initial-inputs ((:name ?input-name :type ?node-type))
  :exit-points ((:name more :ports ((:name ?output-name :type (temporal-sequence ?output-node-type))
				    (:name the-keys :type (temporal-sequence ?key-type))))
		(:name empty))
  ;; :final-outputs ((:name ?output-name :type (TEMPORAL-SEQUENCE ?output-node-type)))
  :components ((:name TYPE-SPLIT :type TYPE-SPLIT :input-TYPE ?node-type :output-TYPE ?first-sub-type :non-matching-output-type ?second-sub-type)
               (:name ENUM-1 :type ENUMERATOR-with-keys :SET-TYPE ?first-sub-type :ELEMENT-TYPE ?output-node-type :key-type ?key-type)
               (:name ENUM-2 :type ENUMERATOR-with-keys :SET-TYPE ?second-sub-type :ELEMENT-TYPE ?output-node-type :key-type ?key-type)
               (:name SJ :type SIMPLE-JOIN-2 :FIRST-JOIN-NAME ?first-sub-type :second-join-name ?second-sub-type 
		      :first-object-type (TEMPORAL-SEQUENCE ?output-node-type) :second-object-type (temporal-sequence ?key-type))
	       (:name exit-join :type control-JOIN :FIRST-JOIN-NAME ?first-sub-type :second-join-name ?second-sub-type)
               )
  :control-flows (((:component enum-1 :branch empty) (:component exit-join :branch ?first-sub-type))
		  ((:component enum-2 :branch empty) (:component exit-join :branch ?second-sub-type))
		  ((:component exit-join) (:component empty)))
  :dataflows (
              ((:component ?input-name :port ?input-name) (:component TYPE-SPLIT :port test-data))
	      ((:component TYPE-SPLIT :port matching-data :branch match) (:component ENUM-1 :port THE-SET))
              ((:component TYPE-SPLIT :port non-matching-data :branch no-match) (:component ENUM-2 :port THE-SET))
              ((:component ENUM-1 :port THE-ELEMENTS :branch MORE) (:component SJ :port first-OBJECT :branch ?first-sub-type))
	      ((:component ENUM-1 :port the-keys :branch more) (:component sj :port second-object :branch ?first-sub-type))
              ((:component ENUM-2 :port THE-ELEMENTS :branch MORE) (:component SJ :port first-OBJECT :branch ?second-sub-type))
	      ((:component ENUM-2 :port the-keys :branch more) (:component sj :port second-object :branch ?second-sub-type))
              ((:component SJ :port first-OBJECT) (:component more :port ?output-name))
	      ((:component SJ :port second-OBJECT) (:component more :port the-keys))))



;;; This is the same as for generator.  Probably should merge enumerator and generator?
(defreduction vector-to-elements-3 (enumerator-with-keys (?input-type ?output-type ?original-input-type ?original-output-type ?input-name ?output-name ?key-type))
  :reduce-to (enumerate-vector-elements ?input-type ?output-type ?input-name ?output-name 'the-keys)
  :the-instance ?the-enumerator
  :prerequisites ((is-of-type? ?input-type 'vector)
		  [property-value-of ?the-enumerator element-type ?sub-type]
		  (is-of-type? ?sub-type ?output-type)
		  )
  )

(defreduction list-of-struct-to-stuff (enumerator-with-keys (?input-type ?output-type
							       ?original-input-type ?original-output-type
							       ?input-name ?output-name
							       ?key-type))
  :reduce-to (enumerate-elements-then-extract ?input-type ?output-type ?intermediate-type ?input-name ?output-name ?local-key-type)
  :the-instance ?the-enumerator
  :prerequisites ((is-of-type? ?input-type 'list)
		  (unify (second ?input-type) ?intermediate-type)
		  [property-value-of ?the-enumerator element-type ?output-type]
		  [property-value-of ?the-enumerator key-type ?local-key-type]
		  [not [unify ?intermediate-type ?output-type]]
		  )
  )

(defcliche enumerate-elements-then-extract (?input-type ?output-type ?intermediate-type ?input-name ?output-name ?key-type)
  :initial-inputs ((:name ?input-name :type ?input-type))
  :state-elements ((:direction source :name node-sequence :port-name node-sequence :port-type (temporal-sequence ?output-type) :state the-sequence)
		   (:direction source :name key-sequence :port-name key-sequence :port-type (temporal-sequence ?key-type) :state the-key-sequence))
  :components ((:name gen-1 :type list-enumerator :element-type ?intermediate-type)
	       (:name t-1 :type take :element-type ?intermediate-type :input-container-type temporal-sequence)
	       (:name extract :type extractor :input-type ?intermediate-type :output-type ?output-type)
	       (:name key-extract :type extractor :input-type ?intermediate-type :output-type ?key-type)
	       (:name add-to-sequence :type put :element-type ?output-type :output-container-type temporal-sequence)
	       (:name add-to-key-sequence :type put :element-type ?key-type :output-container-type temporal-sequence)
	       )
  :exit-points ((:name more :ports ((:name ?output-name :type (temporal-sequence ?output-type))
				    (:name the-keys :type (temporal-sequence ?key-type))))
		(:name empty))
  :dataflows (((:component ?input-name :port ?input-name) (:component gen-1 :port the-list))
	      ((:component gen-1 :branch more :port list-elements) (:component t-1 :port sequence-data))
	      ((:component t-1 :port data) (:component extract :port the-container))
	      ((:component t-1 :port data) (:component key-extract :port the-container))
	      ((:component extract :port the-part) (:component add-to-sequence :port data))
	      ((:component key-extract :port the-part) (:component add-to-key-sequence :port data))
	      ((:component node-sequence :Port node-sequence) (:component add-to-sequence :port sequence))
	      ((:component key-sequence :port key-sequence) (:component add-to-key-sequence :port sequence))
	      ((:component add-to-sequence :port sequence-data) (:component more :port ?output-name))
	      ((:component add-to-key-sequence :port sequence-data) (:component more :port the-keys)))
  :control-flows (((:component gen-1 :branch empty) (:component empty)))
  )

(defreduction extractor-reducer-left (extractor (?input-type ?output-type))
  :reduce-to (left-extractor ?input-type ?output-type)
  :the-instance ?the-extractor
  :prerequisites ((is-of-type? ?input-type 'json-pair)
		  (is-of-type? ?output-type 'json-key))
  )

(defcliche left-extractor (?input-type ?output-type)

  :initial-inputs ((:name the-container :type ?input-type))
  :final-outputs ((:name the-part :type ?output-type))
  :components ((:name left :type left :output-type ?output-type))
  :dataflows (((:component the-container :port the-container) (:component left :port the-container))
	      ((:component left :port the-part) (:component the-part :port the-part))))

(defreduction extractor-reducer-right (extractor (?input-type ?output-type))
  :reduce-to (right-extractor ?input-type ?output-type)
  :the-instance ?the-extractor
  :prerequisites ((is-of-type? ?input-type 'json-pair)
		  (is-of-type? ?output-type 'json-node))
  )

(defcliche right-extractor (?input-type ?output-type)
  :initial-inputs ((:name the-container :type ?input-type))
  :final-outputs ((:name the-part :type ?output-type))
  :components ((:name right :type right :output-type ?output-type))
  :dataflows (((:component the-container :port the-container) (:component right :port the-container))
	      ((:component right :port the-part) (:component the-part :port the-part))))

;;; To make this a practical cliche' we need to standardize enough about each of the pieces so that
;;; For the enumerator: We assume it follows an emuerator protocol
;;; For the filter: Similarly we assume that it follows a "filter protocol"
;;; For the accumulator: Follows an accumulator protocol
;;; The passed in types are actually lists of the type together with a parameter list
;;; The relevant information is then extracted from these in the bindings.

(defreduction efa-reduce (enumerate-filter-accumulate (?input-type ?enumerator-type ?filter-type ?accumulator-type ?output-type))
  :reduce-to (enumerate-filter-accumulate ?input-type ?enumerator-type ?filter-type ?accumulator-type ?output-type)
					  )

(defcliche enumerate-filter-accumulate (?input-type ?enumerator-type ?filter-type ?accumulator-type ?output-type)
  :bindings ((?intermediate-type (getf (rest ?enumerator-type) :element-type)))
  :initial-inputs ((:name the-set :type ?input-type))
  :final-outputs ((:name the-accumuland :type ?output-type))
  :components ((:name enumerator :type ?enumerator-type :recursive-call-name do-it :introduce-labels do-it)
	       (:name do-one :type take :element-type ?intermediate-type :input-container-type temporal-sequence)
	       (:name filter :type ?filter-type)
	       (:name accumulate :type ?accumulator-type :dont-expand nil)
	       )
  :dataflows (
	      ((:component the-set :port the-set) (:component enumerator :port the-set))
	      ((:component enumerator :branch more :port the-elements) (:component do-one :port sequence-data))
	      ((:component do-one :port data) (:component filter :port test-data))
	      ((:component filter :branch match :port matching-data) (:component accumulate :port the-sequence))
	      ((:component accumulate :port accumuland) (:component the-accumuland :port the-accumuland)))
  )
  
(defreduction jdd-reduce (jdd ())
  :reduce-to (jdd-plan))
					    
(defcliche jdd-plan ()
  :initial-inputs ((:name the-node :type json-node))
  :final-outputs ()
  :components ((:name generate-dups :type enumerate-filter-accumulate
		      :input-type json-node :output-type (list symbol)
		      :enumerator-type (tree-traverse :set-type json-node :element-type json-node :key-type json-key)
		      :filter-type (type-split :input-type json-node :output-type json-alist-node)
		      :accumulator-type (detect-duplicates :input-type json-alist-node :output-type (list symbol) :key-extractor-type left))
	       (:name null-test :type empty-test :element-type symbol)
	       (:name print-dups :type print :input-type (list symbol)))
  :dataflows (((:component the-node :port the-node) (:component generate-dups :port the-set))
	      ((:component generate-dups :port the-accumuland) (:component null-test :port the-list))
	      ((:component generate-dups :port the-accumuland) (:component print-dups :port the-data)))
  :control-flows (((:component null-test :branch not-empty) (:component print-dups))))

(defcliche path-accumulator (?element-type ?path-type)
  :initial-inputs ((:name the-path :type ?path-type))
  :state-elements ((:name path-sequence :direction source :port-name path-sequence :port-type (temporal-sequence ?path-type)))
  :final-outputs ((:name the-paths :type (temporal-sequence ?path-type)))
  :components ((:name get-key :type take :element-type ?element-type :input-container-type temporal-sequence)
               (:name add-to-path :type add-to-set :set-type list :element-type ?element-type :unique? nil)
	       (:name add-to-path-sequence :type put :element-type ?path-type :output-container-type temporal-sequence)
	       )
  :dataflows (((:component the-path :port the-path) (:component add-to-path :port the-set))
	      ((:component the-path :port the-path) (:component add-to-path-sequence :port data))
              ((:component get-key :port data) (:component add-to-path :port the-element))
	      ((:component path-sequence :port path-sequence) (:component add-to-path-sequence :port sequence))
              ((:component add-to-path-sequence :port sequence-data) (:component the-paths :port the-paths)))
  :contact-points ((:component get-key :direction input :name sequence-data)
		   (:component add-to-path :direction output :name the-set))
  :initializer path-accumulator-initializer
  )

(defcliche path-accumulator-initializer (?init-component ?path-key-type ?path-type)
  :state-elements ((:name initial-path :direction source :port-name initial-path :port-type ?path-type))
  :components ((:name allocate :type allocate :object-type ?path-type)
	       (:name take-path :type take :input-container-type temporal-sequence :element-type ?path-type))
  :dataflows (((:component allocate :port new-object) (:component initial-path :port initial-initial-path))
	      ((:component initial-path :port initial-path) (:component ?init-component :port the-path))
	      ((:component ?init-component :branch more :port the-paths) (:component take-path :port sequence-data))
	      ))

(defmethod composition-args ((thing (eql 'path-accumulator)) partner)
  (let* ((properties (properties partner))
	 (key-type (getf properties :key-type))
	 (path-description  `(list ,key-type))
	 (path (or (simplest-version path-description) path-description)))
    (list key-type path)))
