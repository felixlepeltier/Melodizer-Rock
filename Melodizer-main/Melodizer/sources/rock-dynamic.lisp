(in-package :mldz)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                         ROCK CLASS                          ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(om::defclass! rock ()
    (
      (block-list :accessor block-list :initarg :block-list :initform nil :documentation "")
    (melody-source :accessor melody-source :initarg :melody-source :initform nil :documentation "")
    (position-list :accessor position-list :initarg :position-list :initform nil :documentation "")
    (bar-length :accessor bar-length :initform 0 :type integer)
    (beat-length :accessor beat-length :initform 0 :type integer)
    (voices :accessor voices :initform nil :type integer)
    (min-pushed-notes :accessor min-pushed-notes :initform nil :type integer)
    (max-pushed-notes :accessor max-pushed-notes :initform nil :type integer)
    (min-notes :accessor min-notes :initform nil :type integer)
    (max-notes :accessor max-notes :initform nil :type integer)
    (min-added-notes :accessor min-added-notes :initform nil :type integer)
    (max-added-notes :accessor max-added-notes :initform nil :type integer)
    (min-note-length-flag :accessor min-note-length-flag :initform nil :type integer)
    (min-note-length :accessor min-note-length :initform 0 :type integer)
    (max-note-length-flag :accessor max-note-length-flag :initform nil :type integer)
    (max-note-length :accessor max-note-length :initform 192 :type integer)
    (quantification :accessor quantification :initform nil :type string)
    (note-repartition-flag :accessor note-repartition-flag :initform nil :type integer)
    (note-repartition :accessor note-repartition :initform nil :type integer)
    (rhythm-repetition :accessor rhythm-repetition :initform nil :type string)
    (pause-quantity-flag :accessor pause-quantity-flag :initform nil :type integer)
    (pause-quantity :accessor pause-quantity :initform 0 :type integer)
    (pause-repartition-flag :accessor pause-repartition-flag :initform nil :type integer)
    (pause-repartition :accessor pause-repartition :initform 0 :type integer)
    (key-selection :accessor key-selection :initform nil :type string)
    (mode-selection :accessor mode-selection :initform nil :type string)
    (chord-key :accessor chord-key :initform nil :type string)
    (chord-quality :accessor chord-quality :initform nil :type string)
    (all-chord-notes :accessor all-chord-notes :initform nil :type integer)
    (min-pitch :accessor min-pitch :initform 1 :type integer)
    (min-pitch-flag :accessor min-pitch-flag :initform nil :type integer)
    (max-pitch :accessor max-pitch :initform 127 :type integer)
    (max-pitch-flag :accessor max-pitch-flag :initform nil :type integer)
    (pitch-direction :accessor pitch-direction :initform nil :type string)
    (golomb-ruler-size :accessor golomb-ruler-size :initform 0 :type integer)
    (note-repetition-flag :accessor note-repetition-flag :initform nil :type integer)
    (note-repetition-type :accessor note-repetition-type :initform "Random" :type string)
    (note-repetition :accessor note-repetition :initform 0 :type integer)
    (solution :accessor solution :initarg :solution :initform nil :documentation "The current solution of the CSP in the form of a voice object.")
    (result :accessor result
      :result :initform (list) :documentation
      "A temporary list holder to store the result of the call to the CSPs, shouldn't be touched.")
    (stop-search :accessor stop-search :stop-search :initform nil :documentation
      "A boolean variable to tell if the user wishes to stop the search or not.")
    (input-rhythm :accessor input-rhythm :input-rhythm :initform (make-instance 'voice) :documentation
      "The rhythm of the melody or a melody in the form of a voice object. ")
    (tempo :accessor tempo :initform 120 :type integer :documentation
      "The tempo (BPM) of the project")
    (branching :accessor branching :initform "Top down" :type string :documentation
      "The tempo (BPM) of the project")
    (percent-diff :accessor percent-diff :initform 0 :type integer)
    )
)


(defclass rock-editor (om::editorview) ())

(defmethod om::class-has-editor-p ((self rock)) t)
(defmethod om::get-editor-class ((self rock)) 'rock-editor)

(defmethod om::om-draw-contents ((view rock-editor))
  (let* ((object (om::object view)))
    (om::om-with-focused-view
      view
      ;;; DRAW SOMETHING ?
    )
  )
)

(defmethod initialize-instance ((self rock-editor) &rest args)
  ;;; do what needs to be done by default
  (call-next-method) ; start the search by default?
  (make-my-interface self)
)

(defmethod make-my-interface ((self rock-editor))

  ; create the main view of the object
  (make-main-view self)
  (let*
    (
      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      ;;; setting the different regions of the tool ;;;
      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

      (rock-panel (om::om-make-view 'om::om-view
        :size (om::om-make-point 200 300)
        :position (om::om-make-point 5 5)
        :bg-color om::*azulito*)
      )
      (constraints-panel (om::om-make-view 'om::om-view
        :size (om::om-make-point 810 500)
        :position (om::om-make-point 5 310)
        :bg-color om::*azulito*)
      )
      (structure-panel (om::om-make-view 'om::om-view
        :size (om::om-make-point 200 300)
        :position (om::om-make-point 210 5)
        :bg-color om::*azulito*)
      )
      (search-panel (om::om-make-view 'om::om-view
        :size (om::om-make-point 400 300)
        :position (om::om-make-point 415 5)
        :bg-color om::*azulito*)
      )
    )

    (setf elements-rock-panel (make-rock-panel self rock-panel))
    (setf elements-constraints-panel (make-constraints-panel self constraints-panel))
    (setf elements-structure-panel (make-structure-panel self structure-panel))
    (setf elements-search-panel (make-rock-search-panel self search-panel))

    ; add the subviews for the different parts into the main view
    (om::om-add-subviews
      self
      rock-panel
      constraints-panel
      structure-panel
      search-panel
    )
  )
  ; return the editor
  self
)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                          A CLASS                            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(om::defclass! A ()
    (
      (s-block :accessor s-block :initarg :s-block :initform (make-instance 's) :documentation "")
      (r-block :accessor r-block :initarg :r-block :initform (make-instance 'r) :documentation "")
      (d-block :accessor d-block :initarg :d-block :initform (make-instance 'd) :documentation "")
      (c-block :accessor c-block :initarg :c-block :initform (make-instance 'c) :documentation "")
      (parent :accessor parent :initarg :parent :initform nil :documentation "")
      (melody-source :accessor melody-source :initarg :melody-source :initform nil :documentation "")
      (position-list :accessor position-list :initarg :position-list :initform nil :documentation "")
      (bar-length :accessor bar-length :initform 0 :type integer)
      (beat-length :accessor beat-length :initform 0 :type integer)
      (voices :accessor voices :initform nil :type integer)
      (min-pushed-notes :accessor min-pushed-notes :initform nil :type integer)
      (max-pushed-notes :accessor max-pushed-notes :initform nil :type integer)
      (min-notes :accessor min-notes :initform nil :type integer)
      (max-notes :accessor max-notes :initform nil :type integer)
      (min-added-notes :accessor min-added-notes :initform nil :type integer)
      (max-added-notes :accessor max-added-notes :initform nil :type integer)
      (min-note-length-flag :accessor min-note-length-flag :initform nil :type integer)
      (min-note-length :accessor min-note-length :initform 0 :type integer)
      (max-note-length-flag :accessor max-note-length-flag :initform nil :type integer)
      (max-note-length :accessor max-note-length :initform 192 :type integer)
      (quantification :accessor quantification :initform nil :type string)
      (note-repartition-flag :accessor note-repartition-flag :initform nil :type integer)
      (note-repartition :accessor note-repartition :initform nil :type integer)
      (rhythm-repetition :accessor rhythm-repetition :initform nil :type string)
      (pause-quantity-flag :accessor pause-quantity-flag :initform nil :type integer)
      (pause-quantity :accessor pause-quantity :initform 0 :type integer)
      (pause-repartition-flag :accessor pause-repartition-flag :initform nil :type integer)
      (pause-repartition :accessor pause-repartition :initform 0 :type integer)
      (key-selection :accessor key-selection :initform nil :type string)
      (mode-selection :accessor mode-selection :initform nil :type string)
      (chord-key :accessor chord-key :initform nil :type string)
      (chord-quality :accessor chord-quality :initform nil :type string)
      (all-chord-notes :accessor all-chord-notes :initform nil :type integer)
      (min-pitch :accessor min-pitch :initform 1 :type integer)
      (min-pitch-flag :accessor min-pitch-flag :initform nil :type integer)
      (max-pitch :accessor max-pitch :initform 127 :type integer)
      (max-pitch-flag :accessor max-pitch-flag :initform nil :type integer)
      (pitch-direction :accessor pitch-direction :initform nil :type string)
      (golomb-ruler-size :accessor golomb-ruler-size :initform 0 :type integer)
      (note-repetition-flag :accessor note-repetition-flag :initform nil :type integer)
      (note-repetition-type :accessor note-repetition-type :initform "Random" :type string)
      (note-repetition :accessor note-repetition :initform 0 :type integer)
    )
)

(defclass A-editor (om::editorview) ())

(defmethod om::class-has-editor-p ((self A)) t)
(defmethod om::get-editor-class ((self A)) 'A-editor)

(defmethod om::om-draw-contents ((view A-editor))
  (let* ((object (om::object view)))
    (om::om-with-focused-view
      view
      ;;; DRAW SOMETHING ?
    )
  )
)

(defmethod initialize-instance ((self A-editor) &rest args)
  ;;; do what needs to be done by default
  (call-next-method) ; start the search by default?
  (make-my-interface self)
)



(defmethod make-my-interface ((self A-editor))

  ; create the main view of the object
  (make-main-view self)

  (let*
    (
      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      ;;; setting the different regions of the tool ;;;
      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

      (A-panel (om::om-make-view 'om::om-view
        :size (om::om-make-point 810 150)
        :position (om::om-make-point 5 5)
        :bg-color om::*azulito*)
      )
       (constraints-panel (om::om-make-view 'om::om-view
        :size (om::om-make-point 810 500)
        :position (om::om-make-point 5 160)
        :bg-color om::*azulito*)
      )
    )

    (setf elements-A-panel (make-A-panel self A-panel))
    (setf elements-constraints-panel (make-constraints-panel self constraints-panel))

    ; add the subviews for the different parts into the main view
    (om::om-add-subviews
      self
      A-panel
      constraints-panel
    )
  )
  ; return the editor
  self
)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                          B CLASS                            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(om::defclass! B ()
    (
      (s-block :accessor s-block :initarg :s-block :initform (make-instance 's) :documentation "")
      (r-block :accessor r-block :initarg :r-block :initform (make-instance 'r) :documentation "")
      (d-block :accessor d-block :initarg :d-block :initform (make-instance 'd) :documentation "")
      (c-block :accessor c-block :initarg :c-block :initform (make-instance 'c) :documentation "")
      (parent :accessor parent :initarg :parent :initform nil :documentation "")
      (melody-source :accessor melody-source :initarg :melody-source :initform nil :documentation "")
      (position-list :accessor position-list :initarg :position-list :initform nil :documentation "")
      (bar-length :accessor bar-length :initform 0 :type integer)
      (voices :accessor voices :initform nil :type integer)
      (min-pushed-notes :accessor min-pushed-notes :initform nil :type integer)
      (max-pushed-notes :accessor max-pushed-notes :initform nil :type integer)
      (min-notes :accessor min-notes :initform nil :type integer)
      (max-notes :accessor max-notes :initform nil :type integer)
      (min-added-notes :accessor min-added-notes :initform nil :type integer)
      (max-added-notes :accessor max-added-notes :initform nil :type integer)
      (min-note-length-flag :accessor min-note-length-flag :initform nil :type integer)
      (min-note-length :accessor min-note-length :initform 0 :type integer)
      (max-note-length-flag :accessor max-note-length-flag :initform nil :type integer)
      (max-note-length :accessor max-note-length :initform 192 :type integer)
      (quantification :accessor quantification :initform nil :type string)
      (note-repartition-flag :accessor note-repartition-flag :initform nil :type integer)
      (note-repartition :accessor note-repartition :initform nil :type integer)
      (rhythm-repetition :accessor rhythm-repetition :initform nil :type string)
      (pause-quantity-flag :accessor pause-quantity-flag :initform nil :type integer)
      (pause-quantity :accessor pause-quantity :initform 0 :type integer)
      (pause-repartition-flag :accessor pause-repartition-flag :initform nil :type integer)
      (pause-repartition :accessor pause-repartition :initform 0 :type integer)
      (key-selection :accessor key-selection :initform nil :type string)
      (mode-selection :accessor mode-selection :initform nil :type string)
      (chord-key :accessor chord-key :initform nil :type string)
      (chord-quality :accessor chord-quality :initform nil :type string)
      (all-chord-notes :accessor all-chord-notes :initform nil :type integer)
      (min-pitch :accessor min-pitch :initform 1 :type integer)
      (min-pitch-flag :accessor min-pitch-flag :initform nil :type integer)
      (max-pitch :accessor max-pitch :initform 127 :type integer)
      (max-pitch-flag :accessor max-pitch-flag :initform nil :type integer)
      (pitch-direction :accessor pitch-direction :initform nil :type string)
      (golomb-ruler-size :accessor golomb-ruler-size :initform 0 :type integer)
      (note-repetition-flag :accessor note-repetition-flag :initform nil :type integer)
      (note-repetition-type :accessor note-repetition-type :initform "Random" :type string)
      (note-repetition :accessor note-repetition :initform 0 :type integer)
    )
)

(defclass B-editor (om::editorview) ())

(defmethod om::class-has-editor-p ((self B)) t)
(defmethod om::get-editor-class ((self B)) 'B-editor)

(defmethod om::om-draw-contents ((view B-editor))
  (let* ((object (om::object view)))
    (om::om-with-focused-view
      view
      ;;; DRAW SOMETHING ?
    )
  )
)

(defmethod initialize-instance ((self B-editor) &rest args)
  ;;; do what needs to be done by default
  (call-next-method) ; start the search by default?
  (make-my-interface self)
)


(defmethod make-my-interface ((self B-editor))

  ; create the main view of the object
  (make-main-view self)

  (let*
    (
      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      ;;; setting the different regions of the tool ;;;
      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

      (B-panel (om::om-make-view 'om::om-view
        :size (om::om-make-point 810 150)
        :position (om::om-make-point 5 5)
        :bg-color om::*azulito*)
      )
       (constraints-panel (om::om-make-view 'om::om-view
        :size (om::om-make-point 810 500)
        :position (om::om-make-point 5 160)
        :bg-color om::*azulito*)
      )
      
    )

    (setf elements-B-panel (make-B-panel self B-panel))
    (setf elements-constraints-panel (make-constraints-panel self constraints-panel))

    ; add the subviews for the different parts into the main view
    (om::om-add-subviews
      self
      B-panel
      constraints-panel
    )
  )
  ; return the editor
  self
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                          s CLASS                            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(om::defclass! s ()
    (
      (parent :accessor parent :initarg :parent :initform nil :documentation "")
      (melody-source :accessor melody-source :initarg :melody-source :initform nil :documentation "")
      (position-list :accessor position-list :initarg :position-list :initform nil :documentation "")
      (bar-length :accessor bar-length :initform 0 :type integer)
      (beat-length :accessor beat-length :initform 0 :type integer)
      (voices :accessor voices :initform nil :type integer)
      (min-pushed-notes :accessor min-pushed-notes :initform nil :type integer)
      (max-pushed-notes :accessor max-pushed-notes :initform nil :type integer)
      (min-notes :accessor min-notes :initform nil :type integer)
      (max-notes :accessor max-notes :initform nil :type integer)
      (min-added-notes :accessor min-added-notes :initform nil :type integer)
      (max-added-notes :accessor max-added-notes :initform nil :type integer)
      (min-note-length-flag :accessor min-note-length-flag :initform nil :type integer)
      (min-note-length :accessor min-note-length :initform 0 :type integer)
      (max-note-length-flag :accessor max-note-length-flag :initform nil :type integer)
      (max-note-length :accessor max-note-length :initform 192 :type integer)
      (quantification :accessor quantification :initform nil :type string)
      (note-repartition-flag :accessor note-repartition-flag :initform nil :type integer)
      (note-repartition :accessor note-repartition :initform nil :type integer)
      (rhythm-repetition :accessor rhythm-repetition :initform nil :type string)
      (pause-quantity-flag :accessor pause-quantity-flag :initform nil :type integer)
      (pause-quantity :accessor pause-quantity :initform 0 :type integer)
      (pause-repartition-flag :accessor pause-repartition-flag :initform nil :type integer)
      (pause-repartition :accessor pause-repartition :initform 0 :type integer)
      (key-selection :accessor key-selection :initform nil :type string)
      (mode-selection :accessor mode-selection :initform nil :type string)
      (chord-key :accessor chord-key :initform nil :type string)
      (chord-quality :accessor chord-quality :initform nil :type string)
      (all-chord-notes :accessor all-chord-notes :initform nil :type integer)
      (min-pitch :accessor min-pitch :initform 1 :type integer)
      (min-pitch-flag :accessor min-pitch-flag :initform nil :type integer)
      (max-pitch :accessor max-pitch :initform 127 :type integer)
      (max-pitch-flag :accessor max-pitch-flag :initform nil :type integer)
      (pitch-direction :accessor pitch-direction :initform nil :type string)
      (golomb-ruler-size :accessor golomb-ruler-size :initform 0 :type integer)
      (note-repetition-flag :accessor note-repetition-flag :initform nil :type integer)
      (note-repetition-type :accessor note-repetition-type :initform "Random" :type string)
      (note-repetition :accessor note-repetition :initform 0 :type integer)
    )
)

(defclass s-editor (om::editorview) ())

(defmethod om::class-has-editor-p ((self s)) t)
(defmethod om::get-editor-class ((self s)) 's-editor)

(defmethod om::om-draw-contents ((view s-editor))
  (let* ((object (om::object view)))
    (om::om-with-focused-view
      view
      ;;; DRAW SOMETHING ?
    )
  )
)

(defmethod initialize-instance ((self s-editor) &rest args)
  ;;; do what needs to be done by default
  (call-next-method) ; start the search by default?
  (make-my-interface self)
)


(defmethod make-my-interface ((self s-editor))

  ; create the main view of the object
  (make-main-view self)

  (let*
    (
      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      ;;; setting the different regions of the tool ;;;
      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
       (constraints-panel (om::om-make-view 'om::om-view
        :size (om::om-make-point 810 500)
        :position (om::om-make-point 5 5)
        :bg-color om::*azulito*)
      )
    )

    (setf elements-constraints-panel (make-constraints-panel self constraints-panel))

    ; add the subviews for the different parts into the main view
    (om::om-add-subviews
      self
      constraints-panel
    )
  )
  ; return the editor
  self
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                          r CLASS                            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(om::defclass! r ()
    (
      (parent :accessor parent :initarg :parent :initform nil :documentation "")
      (melody-source :accessor melody-source :initarg :melody-source :initform nil :documentation "")
      (position-list :accessor position-list :initarg :position-list :initform nil :documentation "")
      (bar-length :accessor bar-length :initform 0 :type integer)
      (beat-length :accessor beat-length :initform 0 :type integer)
      (voices :accessor voices :initform nil :type integer)
      (min-pushed-notes :accessor min-pushed-notes :initform nil :type integer)
      (max-pushed-notes :accessor max-pushed-notes :initform nil :type integer)
      (min-notes :accessor min-notes :initform nil :type integer)
      (max-notes :accessor max-notes :initform nil :type integer)
      (min-added-notes :accessor min-added-notes :initform nil :type integer)
      (max-added-notes :accessor max-added-notes :initform nil :type integer)
      (min-note-length-flag :accessor min-note-length-flag :initform nil :type integer)
      (min-note-length :accessor min-note-length :initform 0 :type integer)
      (max-note-length-flag :accessor max-note-length-flag :initform nil :type integer)
      (max-note-length :accessor max-note-length :initform 192 :type integer)
      (quantification :accessor quantification :initform nil :type string)
      (note-repartition-flag :accessor note-repartition-flag :initform nil :type integer)
      (note-repartition :accessor note-repartition :initform nil :type integer)
      (rhythm-repetition :accessor rhythm-repetition :initform nil :type string)
      (pause-quantity-flag :accessor pause-quantity-flag :initform nil :type integer)
      (pause-quantity :accessor pause-quantity :initform 0 :type integer)
      (pause-repartition-flag :accessor pause-repartition-flag :initform nil :type integer)
      (pause-repartition :accessor pause-repartition :initform 0 :type integer)
      (key-selection :accessor key-selection :initform nil :type string)
      (mode-selection :accessor mode-selection :initform nil :type string)
      (chord-key :accessor chord-key :initform nil :type string)
      (chord-quality :accessor chord-quality :initform nil :type string)
      (all-chord-notes :accessor all-chord-notes :initform nil :type integer)
      (min-pitch :accessor min-pitch :initform 1 :type integer)
      (min-pitch-flag :accessor min-pitch-flag :initform nil :type integer)
      (max-pitch :accessor max-pitch :initform 127 :type integer)
      (max-pitch-flag :accessor max-pitch-flag :initform nil :type integer)
      (pitch-direction :accessor pitch-direction :initform nil :type string)
      (golomb-ruler-size :accessor golomb-ruler-size :initform 0 :type integer)
      (note-repetition-flag :accessor note-repetition-flag :initform nil :type integer)
      (note-repetition-type :accessor note-repetition-type :initform "Random" :type string)
      (note-repetition :accessor note-repetition :initform 0 :type integer)
    )
)

(defclass r-editor (om::editorview) ())

(defmethod om::class-has-editor-p ((self r)) t)
(defmethod om::get-editor-class ((self r)) 'r-editor)

(defmethod om::om-draw-contents ((view r-editor))
  (let* ((object (om::object view)))
    (om::om-with-focused-view
      view
      ;;; DRAW SOMETHING ?
    )
  )
)

(defmethod initialize-instance ((self r-editor) &rest args)
  ;;; do what needs to be done by default
  (call-next-method) ; start the search by default?
  (make-my-interface self)
)


(defmethod make-my-interface ((self r-editor))

  ; create the main view of the object
  (make-main-view self)

  (let*
    (
      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      ;;; setting the different regions of the tool ;;;
      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
       (constraints-panel (om::om-make-view 'om::om-view
        :size (om::om-make-point 810 500)
        :position (om::om-make-point 5 5)
        :bg-color om::*azulito*)
      )
    )

    (setf elements-constraints-panel (make-constraints-panel self constraints-panel))

    ; add the subviews for the different parts into the main view
    (om::om-add-subviews
      self
      constraints-panel
    )
  )
  ; return the editor
  self
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                          d CLASS                            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(om::defclass! d ()
    (
      (melody-source :accessor melody-source :initarg :melody-source :initform nil :documentation "")
      (position-list :accessor position-list :initarg :position-list :initform nil :documentation "")
      (bar-length :accessor bar-length :initform 0 :type integer)
      (beat-length :accessor beat-length :initform 0 :type integer)
      (voices :accessor voices :initform nil :type integer)
      (min-pushed-notes :accessor min-pushed-notes :initform nil :type integer)
      (max-pushed-notes :accessor max-pushed-notes :initform nil :type integer)
      (min-notes :accessor min-notes :initform nil :type integer)
      (max-notes :accessor max-notes :initform nil :type integer)
      (min-added-notes :accessor min-added-notes :initform nil :type integer)
      (max-added-notes :accessor max-added-notes :initform nil :type integer)
      (min-note-length-flag :accessor min-note-length-flag :initform nil :type integer)
      (min-note-length :accessor min-note-length :initform 0 :type integer)
      (max-note-length-flag :accessor max-note-length-flag :initform nil :type integer)
      (max-note-length :accessor max-note-length :initform 192 :type integer)
      (quantification :accessor quantification :initform nil :type string)
      (note-repartition-flag :accessor note-repartition-flag :initform nil :type integer)
      (note-repartition :accessor note-repartition :initform nil :type integer)
      (rhythm-repetition :accessor rhythm-repetition :initform nil :type string)
      (pause-quantity-flag :accessor pause-quantity-flag :initform nil :type integer)
      (pause-quantity :accessor pause-quantity :initform 0 :type integer)
      (pause-repartition-flag :accessor pause-repartition-flag :initform nil :type integer)
      (pause-repartition :accessor pause-repartition :initform 0 :type integer)
      (key-selection :accessor key-selection :initform nil :type string)
      (mode-selection :accessor mode-selection :initform nil :type string)
      (chord-key :accessor chord-key :initform nil :type string)
      (chord-quality :accessor chord-quality :initform nil :type string)
      (all-chord-notes :accessor all-chord-notes :initform nil :type integer)
      (min-pitch :accessor min-pitch :initform 1 :type integer)
      (min-pitch-flag :accessor min-pitch-flag :initform nil :type integer)
      (max-pitch :accessor max-pitch :initform 127 :type integer)
      (max-pitch-flag :accessor max-pitch-flag :initform nil :type integer)
      (pitch-direction :accessor pitch-direction :initform nil :type string)
      (golomb-ruler-size :accessor golomb-ruler-size :initform 0 :type integer)
      (note-repetition-flag :accessor note-repetition-flag :initform nil :type integer)
      (note-repetition-type :accessor note-repetition-type :initform "Random" :type string)
      (note-repetition :accessor note-repetition :initform 0 :type integer)
    )
)

(defclass d-editor (om::editorview) ())

(defmethod om::class-has-editor-p ((self d)) t)
(defmethod om::get-editor-class ((self d)) 'd-editor)

(defmethod om::om-draw-contents ((view d-editor))
  (let* ((object (om::object view)))
    (om::om-with-focused-view
      view
      ;;; DRAW SOMETHING ?
    )
  )
)

(defmethod initialize-instance ((self d-editor) &rest args)
  ;;; do what needs to be done by default
  (call-next-method) ; start the search by default?
  (make-my-interface self)
)

(defmethod make-my-interface ((self d-editor))

  ; create the main view of the object
  (make-main-view self)

  (let*
    (
      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      ;;; setting the different regions of the tool ;;;
      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
       (constraints-panel (om::om-make-view 'om::om-view
        :size (om::om-make-point 810 500)
        :position (om::om-make-point 5 5)
        :bg-color om::*azulito*)
      )
    )

    (setf elements-constraints-panel (make-constraints-panel self constraints-panel))

    ; add the subviews for the different parts into the main view
    (om::om-add-subviews
      self
      constraints-panel
    )
  )
  ; return the editor
  self
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                          c CLASS                            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(om::defclass! c ()
    (
      (melody-source :accessor melody-source :initarg :melody-source :initform nil :documentation "")
      (position-list :accessor position-list :initarg :position-list :initform nil :documentation "")
      (bar-length :accessor bar-length :initform 0 :type integer)
      (beat-length :accessor beat-length :initform 0 :type integer)
      (voices :accessor voices :initform nil :type integer)
      (min-pushed-notes :accessor min-pushed-notes :initform nil :type integer)
      (max-pushed-notes :accessor max-pushed-notes :initform nil :type integer)
      (min-notes :accessor min-notes :initform nil :type integer)
      (max-notes :accessor max-notes :initform nil :type integer)
      (min-added-notes :accessor min-added-notes :initform nil :type integer)
      (max-added-notes :accessor max-added-notes :initform nil :type integer)
      (min-note-length-flag :accessor min-note-length-flag :initform nil :type integer)
      (min-note-length :accessor min-note-length :initform 0 :type integer)
      (max-note-length-flag :accessor max-note-length-flag :initform nil :type integer)
      (max-note-length :accessor max-note-length :initform 192 :type integer)
      (quantification :accessor quantification :initform nil :type string)
      (note-repartition-flag :accessor note-repartition-flag :initform nil :type integer)
      (note-repartition :accessor note-repartition :initform nil :type integer)
      (rhythm-repetition :accessor rhythm-repetition :initform nil :type string)
      (pause-quantity-flag :accessor pause-quantity-flag :initform nil :type integer)
      (pause-quantity :accessor pause-quantity :initform 0 :type integer)
      (pause-repartition-flag :accessor pause-repartition-flag :initform nil :type integer)
      (pause-repartition :accessor pause-repartition :initform 0 :type integer)
      (key-selection :accessor key-selection :initform nil :type string)
      (mode-selection :accessor mode-selection :initform nil :type string)
      (chord-key :accessor chord-key :initform nil :type string)
      (chord-quality :accessor chord-quality :initform nil :type string)
      (all-chord-notes :accessor all-chord-notes :initform nil :type integer)
      (min-pitch :accessor min-pitch :initform 1 :type integer)
      (min-pitch-flag :accessor min-pitch-flag :initform nil :type integer)
      (max-pitch :accessor max-pitch :initform 127 :type integer)
      (max-pitch-flag :accessor max-pitch-flag :initform nil :type integer)
      (pitch-direction :accessor pitch-direction :initform nil :type string)
      (golomb-ruler-size :accessor golomb-ruler-size :initform 0 :type integer)
      (note-repetition-flag :accessor note-repetition-flag :initform nil :type integer)
      (note-repetition-type :accessor note-repetition-type :initform "Random" :type string)
      (note-repetition :accessor note-repetition :initform 0 :type integer)
    )
)

(defclass c-editor (om::editorview) ())

(defmethod om::class-has-editor-p ((self c)) t)
(defmethod om::get-editor-class ((self c)) 'c-editor)

(defmethod om::om-draw-contents ((view c-editor))
  (let* ((object (om::object view)))
    (om::om-with-focused-view
      view
      ;;; DRAW SOMETHING ?
    )
  )
)

(defmethod initialize-instance ((self c-editor) &rest args)
  ;;; do what needs to be done by default
  (call-next-method) ; start the search by default?
  (make-my-interface self)
)

(defmethod make-my-interface ((self c-editor))

  ; create the main view of the object
  (make-main-view self)

  (let*
    (
      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      ;;; setting the different regions of the tool ;;;
      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

       (constraints-panel (om::om-make-view 'om::om-view
        :size (om::om-make-point 810 500)
        :position (om::om-make-point 5 5)
        :bg-color om::*azulito*)
      )
    )

    (setf elements-constraints-panel (make-constraints-panel self constraints-panel))

    ; add the subviews for the different parts into the main view
    (om::om-add-subviews
      self
      constraints-panel
    )
  )
  ; return the editor
  self
)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                  INTERFACE CONSTRUCTION                     ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

    ;;;;;;;;;;;;;;;;;
    ;;; main view ;;;
    ;;;;;;;;;;;;;;;;;

; this function creates the elements for the main panel
(defun make-main-view (editor)
  ; background colour
  (om::om-set-bg-color editor om::*om-light-gray-color*) ;pour changer le bg color. om peut fabriquer sa propre couleur: (om-make-color r g b)
)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                      ROCK INTERFACE                         ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun make-rock-panel (editor rock-panel)
  (om::om-add-subviews
    rock-panel
    (om::om-make-dialog-item
        'om::om-button
        (om::om-make-point 5 5) ; position (horizontal, vertical)
        (om::om-make-point 150 40) ; size (horizontal, vertical)
        "Add A to structure"
        :di-action #'(lambda (b)
        
          (print "Added A to structure")
          (setf (block-list (om::object editor)) (append (block-list (om::object editor)) (list (make-instance 'A :parent (om::object editor) (om::object editor)))))
          (change-sublocks-bar-length (om::object editor) (bar-length (om::object editor)))
          (change-sublocks-chord-key (om::object editor) (chord-key (om::object editor)))
          (change-sublocks-min-pitch (om::object editor) (min-pitch-flag (om::object editor)) (min-pitch (om::object editor)))
          (change-sublocks-max-pitch (om::object editor) (max-pitch-flag (om::object editor)) (max-pitch (om::object editor)))
          ;; (print (block-list (om::object editor)))
        )
    )

    

    (om::om-make-dialog-item
        'om::om-button
        (om::om-make-point 5 50) ; position (horizontal, vertical)
        (om::om-make-point 150 40) ; size (horizontal, vertical)
        "Add B to structure"
        :di-action #'(lambda (b)
          (print "Added B to structure")
          (setf (block-list (om::object editor)) (append (block-list (om::object editor)) (list (make-instance 'B :parent (om::object editor) (om::object editor)))))
          (change-sublocks-bar-length (om::object editor) (bar-length (om::object editor)))
          (change-sublocks-chord-key (om::object editor) (chord-key (om::object editor)))
          (change-sublocks-min-pitch (om::object editor) (min-pitch-flag (om::object editor)) (min-pitch (om::object editor)))
          (change-sublocks-max-pitch (om::object editor) (max-pitch-flag (om::object editor)) (max-pitch (om::object editor)))
          ;; (print (block-list (om::object editor)))
        )
    )
    
        ;;     :size (om::om-make-point 450 140)
        ;; :position (om::om-make-point 5 5)

    (om::om-make-dialog-item
        'om::om-button
        (om::om-make-point 5 95) ; position (horizontal, vertical)
        (om::om-make-point 150 40) ; size (horizontal, vertical)
        "Done"
        :di-action #'(lambda (b)
          (print "Finished structure")
          (mp:process-run-function ; start a new thread for the execution of the next method
            "next thread" ; name of the thread, not necessary but useful for debugging
            nil ; process initialization keywords, not needed here
            (lambda () ; function to call
              (make-my-interface editor)
            )
          )
        )
    )

    (om::om-make-dialog-item
        'om::om-button
        (om::om-make-point 5 140) ; position (horizontal, vertical)
        (om::om-make-point 150 40) ; size (horizontal, vertical)
        "Clear structure"
        :di-action #'(lambda (b)
          (print "Cleared structure")
          (mp:process-run-function ; start a new thread for the execution of the next method
            "next thread" ; name of the thread, not necessary but useful for debugging
            nil ; process initialization keywords, not needed here
            (lambda () ; function to call
              (setf (block-list (om::object editor)) nil)
              (make-my-interface editor)
            )
          )
        )
    )
  )
)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                  STRUCTURE INTERFACE                        ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; if we have access to the rock-editor
(defun make-structure-panel (editor structure-panel)

  (setq loop-index 0)
  (defvar subview-list '())
  (loop for x in (block-list (om::object editor))
    do 
      (if (typep x 'mldz::a)
        (setf subview-list (append subview-list (list (om::om-make-dialog-item
          'om::om-button
          (om::om-make-point 5 (+ 10 (* 50 loop-index))) ; position (horizontal, vertical)
          (om::om-make-point 100 50) ; size (horizontal, vertical)
          "A"
          :di-action #'(lambda (b)

            (print "Selected A")

            (mp:process-run-function ; start a new thread for the execution of the next method
              "next thread" ; name of the thread, not necessary but useful for debugging
              nil ; process initialization keywords, not needed here
              #'(lambda () ; function to call
                (om::openeditorframe ; open a window displaying the editor of the first A block
                  (om::omNG-make-new-instance (nth (position b subview-list) (block-list (om::object editor))) (concatenate 'string "Window A" (write-to-string (position b subview-list))))
                )
              )
            )
          )
        ))))
      )

      (if (typep x 'mldz::b)
        (setf subview-list (append subview-list (list (om::om-make-dialog-item
          'om::om-button
          (om::om-make-point 5 (+ 10 (* 50 loop-index))) ; position (horizontal, vertical)
          (om::om-make-point 100 50) ; size (horizontal, vertical)
          "B"
          :di-action #'(lambda (b)
            (print "Selected B")

            (mp:process-run-function ; start a new thread for the execution of the next method
              "next thread" ; name of the thread, not necessary but useful for debugging
              nil ; process initialization keywords, not needed here
              #'(lambda () ; function to call
                (om::openeditorframe ; open a window displaying the editor of the first A block
                  (om::omNG-make-new-instance (nth (position b subview-list) (block-list (om::object editor))) (concatenate 'string "Window B" (write-to-string (position b subview-list))))
                )
              )
            )
          )
        ))))
      )
      (setq loop-index (+ loop-index 1))
  )

  (if (not subview-list)
    (om::om-add-subviews
      structure-panel
    )
    (loop for x in subview-list
      do
        (om::om-add-subviews
          structure-panel
          x
        )
    )
    ;; (om::om-add-subviews
    ;;   structure-panel
    ;;   (multiple-value-list subview-list)
    ;; )
  )
)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                        A INTERFACE                          ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



(defun make-A-panel (editor A-panel)
  (om::om-add-subviews
    A-panel
    (om::om-make-dialog-item
        'om::om-button
        (om::om-make-point 5 10) ; position (horizontal, vertical)
        (om::om-make-point 100 50) ; size (horizontal, vertical)
        "s"
        :di-action #'(lambda (b)
          (print "Selected s")
          (mp:process-run-function ; start a new thread for the execution of the next method
            "next thread" ; name of the thread, not necessary but useful for debugging
            nil ; process initialization keywords, not needed here
            (lambda () ; function to call
              ;; (setf (s-block (om::object editor)) (make-instance 's :parent (om::object editor) (om::object editor)))
              (om::openeditorframe ; open a window displaying the editor of the first A block
                (om::omNG-make-new-instance (s-block (om::object editor)) "Window s")
              )
            )
          )
        )
    )
    (om::om-make-dialog-item
        'om::om-button
        (om::om-make-point 115 10) ; position (horizontal, vertical)
        (om::om-make-point 100 50) ; size (horizontal, vertical)
        "r"
        :di-action #'(lambda (b)
          (print "Selected r")
          (mp:process-run-function ; start a new thread for the execution of the next method
            "next thread" ; name of the thread, not necessary but useful for debugging
            nil ; process initialization keywords, not needed here
            (lambda () ; function to call
              ;; (setf (r-block (om::object editor)) (make-instance 'r :parent (om::object editor) (om::object editor)))
              (om::openeditorframe ; open a window displaying the editor of the first A block
                (om::omNG-make-new-instance (r-block (om::object editor)) "Window r")
              )
            )
          )
        )
    )
    (om::om-make-dialog-item
        'om::om-button
        (om::om-make-point 225 10) ; position (horizontal, vertical)
        (om::om-make-point 100 50) ; size (horizontal, vertical)
        "d"
        :di-action #'(lambda (b)
          (print "Selected d")
          (mp:process-run-function ; start a new thread for the execution of the next method
            "next thread" ; name of the thread, not necessary but useful for debugging
            nil ; process initialization keywords, not needed here
            (lambda () ; function to call
              ;; (setf (d-block (om::object editor)) (make-instance 'd :parent (om::object editor) (om::object editor)))
              (om::openeditorframe ; open a window displaying the editor of the first A block
                (om::omNG-make-new-instance (d-block (om::object editor)) "Window d")
              )
            )
          )
        )
    )
    (om::om-make-dialog-item
        'om::om-button
        (om::om-make-point 335 10) ; position (horizontal, vertical)
        (om::om-make-point 100 50) ; size (horizontal, vertical)
        "c"
        :di-action #'(lambda (b)
          (print "Selected c")
          (mp:process-run-function ; start a new thread for the execution of the next method
            "next thread" ; name of the thread, not necessary but useful for debugging
            nil ; process initialization keywords, not needed here
            (lambda () ; function to call
              ;; (setf (c-block (om::object editor)) (make-instance 'c :parent (om::object editor) (om::object editor)))
              (om::openeditorframe ; open a window displaying the editor of the first A block
                (om::omNG-make-new-instance (c-block (om::object editor)) "Window c")
              )
            )
          )
        )
    )
  )
)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                        B INTERFACE                          ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



(defun make-B-panel (editor B-panel)
  (om::om-add-subviews
    B-panel
    (om::om-make-dialog-item
        'om::om-button
        (om::om-make-point 5 10) ; position (horizontal, vertical)
        (om::om-make-point 100 50) ; size (horizontal, vertical)
        "s"
        :di-action #'(lambda (b)
          (print "Selected s")
          (mp:process-run-function ; start a new thread for the execution of the next method
            "next thread" ; name of the thread, not necessary but useful for debugging
            nil ; process initialization keywords, not needed here
            (lambda () ; function to call
              ;; (setf (s-block (om::object editor)) (make-instance 's :parent (om::object editor) (om::object editor)))
              (om::openeditorframe ; open a window displaying the editor of the first A block
                (om::omNG-make-new-instance (s-block (om::object editor)) "Window s")
              )
            )
          )
        )
    )
    (om::om-make-dialog-item
        'om::om-button
        (om::om-make-point 115 10) ; position (horizontal, vertical)
        (om::om-make-point 100 50) ; size (horizontal, vertical)
        "r"
        :di-action #'(lambda (b)
          (print "Selected r")
          (mp:process-run-function ; start a new thread for the execution of the next method
            "next thread" ; name of the thread, not necessary but useful for debugging
            nil ; process initialization keywords, not needed here
            (lambda () ; function to call
              ;; (setf (r-block (om::object editor)) (make-instance 'r :parent (om::object editor) (om::object editor)))
              (om::openeditorframe ; open a window displaying the editor of the first A block
                (om::omNG-make-new-instance (r-block (om::object editor)) "Window r")
              )
            )
          )
        )
    )
    (om::om-make-dialog-item
        'om::om-button
        (om::om-make-point 225 10) ; position (horizontal, vertical)
        (om::om-make-point 100 50) ; size (horizontal, vertical)
        "d"
        :di-action #'(lambda (b)
          (print "Selected d")
          (mp:process-run-function ; start a new thread for the execution of the next method
            "next thread" ; name of the thread, not necessary but useful for debugging
            nil ; process initialization keywords, not needed here
            (lambda () ; function to call
              ;; (setf (d-block (om::object editor)) (make-instance 'd :parent (om::object editor) (om::object editor)))
              (om::openeditorframe ; open a window displaying the editor of the first A block
                (om::omNG-make-new-instance (d-block (om::object editor)) "Window d")
              )
            )
          )
        )
    )
    (om::om-make-dialog-item
        'om::om-button
        (om::om-make-point 335 10) ; position (horizontal, vertical)
        (om::om-make-point 100 50) ; size (horizontal, vertical)
        "c"
        :di-action #'(lambda (b)
          (print "Selected c")
          (mp:process-run-function ; start a new thread for the execution of the next method
            "next thread" ; name of the thread, not necessary but useful for debugging
            nil ; process initialization keywords, not needed here
            (lambda () ; function to call
              ;; (setf (c-block (om::object editor)) (make-instance 'c :parent (om::object editor) (om::object editor)))
              (om::openeditorframe ; open a window displaying the editor of the first A block
                (om::omNG-make-new-instance (c-block (om::object editor)) "Window c")
              )
            )
          )
        )
    )
  )
)

(defun make-constraints-panel (editor panel)
  (print "constraints")
  (print (bar-length-range (om::object editor)))
  (om::om-add-subviews
    panel
    (om::om-make-dialog-item
      'om::om-static-text
      (om::om-make-point 15 2)
      (om::om-make-point 120 20)
      "Block constraints"
      :font om::*om-default-font1b*
    )

    (om::om-make-dialog-item
      'om::om-static-text
      (om::om-make-point 15 50)
      (om::om-make-point 200 20)
      "Number of bars"
      :font om::*om-default-font1b*
    )

    (om::om-make-dialog-item
      'om::pop-up-menu
      (om::om-make-point 170 50)
      (om::om-make-point 80 20)
      "Bar length"
      :range (bar-length-range (om::object editor))
      :value (number-to-string (bar-length (om::object editor)))
      :di-action #'(lambda (m)
        (setq check (nth (om::om-get-selected-item-index m) (om::om-get-item-list m)))
        (if (string= check "None")
          (setf (bar-length (om::object editor)) 0)
          (setf (bar-length (om::object editor)) (string-to-number check)))
        ;; (setf (bar-length (om::object editor)) (string-to-number (nth (om::om-get-selected-item-index m) (om::om-get-item-list m))))
        ;; (print (bar-length (om::object editor)))
        (change-sublocks-bar-length (om::object editor) (bar-length (om::object editor)))
      )
    )

    (om::om-make-dialog-item
      'om::om-static-text
      (om::om-make-point 15 150)
      (om::om-make-point 200 20)
      "Minimum pushed notes"
      :font om::*om-default-font1b*
    )

    (om::om-make-dialog-item
      'om::pop-up-menu
      (om::om-make-point 170 150)
      (om::om-make-point 80 20)
      "Minimum pushed notes"
      :range (append '("None") (loop :for n :from 0 :upto 10 collect n))
      :di-action #'(lambda (m)
        (setq check (nth (om::om-get-selected-item-index m) (om::om-get-item-list m)))
        (if (typep check 'string)
          (setf (min-pushed-notes (om::object editor)) nil)
          (setf (min-pushed-notes (om::object editor)) check))
      )
    )

    (om::om-make-dialog-item
      'om::om-static-text
      (om::om-make-point 15 200)
      (om::om-make-point 200 20)
      "Maximum pushed notes"
      :font om::*om-default-font1b*
    )

    (om::om-make-dialog-item
      'om::pop-up-menu
      (om::om-make-point 170 200)
      (om::om-make-point 80 20)
      "Maximum pushed notes"
      :range (append '("None") (loop :for n :from 0 :upto 10 collect n))
      :di-action #'(lambda (m)
        (setq check (nth (om::om-get-selected-item-index m) (om::om-get-item-list m)))
        (if (typep check 'string)
          (setf (max-pushed-notes (om::object editor)) nil)
          (setf (max-pushed-notes (om::object editor)) check))
      )
    )


    (om::om-make-dialog-item
      'om::om-static-text
      (om::om-make-point 300 2)
      (om::om-make-point 120 20)
      "Time constraints"
      :font om::*om-default-font1b*
    )

    (om::om-make-dialog-item
      'om::om-static-text
      (om::om-make-point 300 50)
      (om::om-make-point 200 20)
      "Minimum note length"
      :font om::*om-default-font1b*
    )

    (om::om-make-dialog-item
      'om::om-check-box
      (om::om-make-point 430 50)
      (om::om-make-point 20 20)
      ""
      :di-action #'(lambda (c)
                    (if (om::om-checked-p c)
                      (setf (min-note-length-flag (om::object editor)) 1)
                      (setf (min-note-length-flag (om::object editor)) nil)
                    )
      )
    )

    ; slider to express how different the solutions should be (100 = completely different, 1 = almost no difference)
    (om::om-make-dialog-item
      'om::om-slider
      (om::om-make-point 450 50)
      (om::om-make-point 80 20); size
      "Minimum note length"
      :range '(0 192)
      :increment 1
      :di-action #'(lambda (s)
        (setf (min-note-length (om::object editor)) (om::om-slider-value s))
      )
    )

    (om::om-make-dialog-item
      'om::om-static-text
      (om::om-make-point 300 100)
      (om::om-make-point 200 20)
      "Maximum note length"
      :font om::*om-default-font1b*
    )

    (om::om-make-dialog-item
      'om::om-check-box
      (om::om-make-point 430 100)
      (om::om-make-point 20 20)
      ""
      :di-action #'(lambda (c)
                    (if (om::om-checked-p c)
                      (setf (max-note-length-flag (om::object editor)) 1)
                      (setf (max-note-length-flag (om::object editor)) nil)
                    )
      )
    )

    (om::om-make-dialog-item
      'om::om-slider
      (om::om-make-point 450 100)
      (om::om-make-point 80 20); size
      "Maximum note length"
      :range '(0 192)
      :increment 1
      :di-action #'(lambda (s)
        (setf (max-note-length (om::object editor)) (om::om-slider-value s))
      )
    )

    (om::om-make-dialog-item
      'om::om-static-text
      (om::om-make-point 550 2)
      (om::om-make-point 200 20)
      "Pitch constraints"
      :font om::*om-default-font1b*
    )

    ; Key

    (om::om-make-dialog-item
      'om::om-static-text
      (om::om-make-point 550 50)
      (om::om-make-point 200 20)
      "Key selection"
      :font om::*om-default-font1b*
    )

    (om::om-make-dialog-item
      'om::pop-up-menu
      (om::om-make-point 670 50)
      (om::om-make-point 80 20)
      "Key selection"
      :range '("None" "C" "C#" "D" "Eb" "E" "F" "F#" "G" "Ab" "A" "Bb" "B")
      :di-action #'(lambda (m)
        (setq check (nth (om::om-get-selected-item-index m) (om::om-get-item-list m)))
        (if (string= check "None")
          (setf (key-selection (om::object editor)) nil)
          (setf (key-selection (om::object editor)) check))
      )
    )



    (om::om-make-dialog-item
      'om::om-static-text
      (om::om-make-point 550 100)
      (om::om-make-point 200 20)
      "Chord key"
      :font om::*om-default-font1b*
    )

    (om::om-make-dialog-item
      'om::pop-up-menu
      (om::om-make-point 670 100)
      (om::om-make-point 80 20)
      "Chord key"
      :range '("None" "C" "C#" "D" "Eb" "E" "F" "F#" "G" "Ab" "A" "Bb" "B")
      :value (chord-key (om::object editor))
      :di-action #'(lambda (m)
        (setq check (nth (om::om-get-selected-item-index m) (om::om-get-item-list m)))
        (if (string= check "None")
          (setf (chord-key (om::object editor)) nil)
          (setf (chord-key (om::object editor)) check))
          (change-sublocks-chord-key (om::object editor) check)
      )
    )
     (om::om-make-dialog-item
      'om::om-static-text
      (om::om-make-point 550 150)
      (om::om-make-point 200 20)
      "Minimum pitch"
      :font om::*om-default-font1b*
    )

    (om::om-make-dialog-item
      'om::om-check-box
      (om::om-make-point 670 150)
      (om::om-make-point 20 20)
      ""
      :checked-p (min-pitch-flag (om::object editor))
      :di-action #'(lambda (c)
                    (if (om::om-checked-p c)
                      (setf (min-pitch-flag (om::object editor)) 1)
                      (setf (min-pitch-flag (om::object editor)) nil)
                    )
                    (change-sublocks-min-pitch (om::object editor) (min-pitch-flag (om::object editor)) (min-pitch (om::object editor)))
      )
    )

    (om::om-make-dialog-item
      'om::slider
      (om::om-make-point 690 150)
      (om::om-make-point 80 20)
      "Minimum pitch"
      :range '(1 127)
      :increment 1
      :value (min-pitch (om::object editor))
      :di-action #'(lambda (s)
        (setf (min-pitch (om::object editor)) (om::om-slider-value s))
        (change-sublocks-min-pitch (om::object editor) (min-pitch-flag (om::object editor)) (min-pitch (om::object editor)))
      )
    )

    (om::om-make-dialog-item
      'om::om-static-text
      (om::om-make-point 550 200)
      (om::om-make-point 200 20)
      "Maximum pitch"
      :font om::*om-default-font1b*
    )

    (om::om-make-dialog-item
      'om::om-check-box
      (om::om-make-point 670 200)
      (om::om-make-point 20 20)
      ""
      :checked-p (max-pitch-flag (om::object editor))
      :di-action #'(lambda (c)
                    (if (om::om-checked-p c)
                      (setf (max-pitch-flag (om::object editor)) 1)
                      (setf (max-pitch-flag (om::object editor)) nil)
                    )
                    (change-sublocks-max-pitch (om::object editor) (max-pitch-flag (om::object editor)) (max-pitch (om::object editor)))
      )
    )

    (om::om-make-dialog-item
      'om::slider
      (om::om-make-point 690 200)
      (om::om-make-point 80 20)
      "Maximum pitch"
      :range '(1 127)
      :increment 1
      :value (max-pitch (om::object editor))
      :di-action #'(lambda (s)
        (setf (max-pitch (om::object editor)) (om::om-slider-value s))
        (change-sublocks-max-pitch (om::object editor) (max-pitch-flag (om::object editor)) (max-pitch (om::object editor)))
      )
    )




    ;; (om::om-make-dialog-item
    ;;   'om::om-static-text
    ;;   (om::om-make-point 15 100)
    ;;   (om::om-make-point 200 20)
    ;;   "Voices"
    ;;   :font om::*om-default-font1b*
    ;; )

    ;; (om::om-make-dialog-item
    ;;   'om::pop-up-menu
    ;;   (om::om-make-point 170 100)
    ;;   (om::om-make-point 200 20)
    ;;   "Voices"
    ;;   :range (append '("None") (loop :for n :from 0 :upto 15 collect n))
    ;;   :di-action #'(lambda (m)
    ;;     (setq check (nth (om::om-get-selected-item-index m) (om::om-get-item-list m)))
    ;;     (if (typep check 'string)
    ;;       (setf (voices (om::object editor)) nil)
    ;;       (setf (voices (om::object editor)) check))
    ;;   )
    ;; )

    ;; (om::om-make-dialog-item
    ;;   'om::om-static-text
    ;;   (om::om-make-point 15 150)
    ;;   (om::om-make-point 200 20)
    ;;   "Chord key"
    ;;   :font om::*om-default-font1b*
    ;; )

    ;; (om::om-make-dialog-item
    ;;   'om::pop-up-menu
    ;;   (om::om-make-point 170 150)
    ;;   (om::om-make-point 200 20)
    ;;   "Chord key"
    ;;   :range '("None" "C" "C#" "D" "Eb" "E" "F" "F#" "G" "Ab" "A" "Bb" "B")
    ;;   :value (chord-key (om::object editor))
    ;;   :di-action #'(lambda (m)
    ;;     (setq check (nth (om::om-get-selected-item-index m) (om::om-get-item-list m)))
    ;;     (if (string= check "None")
    ;;       (setf (chord-key (om::object editor)) nil)
    ;;       (setf (chord-key (om::object editor)) check))
    ;;       (change-sublocks-chord-key (om::object editor) check)
    ;;   )
    ;; )
    ;;  (om::om-make-dialog-item
    ;;   'om::om-static-text
    ;;   (om::om-make-point 15 250)
    ;;   (om::om-make-point 200 20)
    ;;   "Minimum pitch"
    ;;   :font om::*om-default-font1b*
    ;; )

    ;; (om::om-make-dialog-item
    ;;   'om::om-check-box
    ;;   (om::om-make-point 170 250)
    ;;   (om::om-make-point 20 20)
    ;;   ""
    ;;   :checked-p (min-pitch-flag (om::object editor))
    ;;   :di-action #'(lambda (c)
    ;;                 (if (om::om-checked-p c)
    ;;                   (setf (min-pitch-flag (om::object editor)) 1)
    ;;                   (setf (min-pitch-flag (om::object editor)) nil)
    ;;                 )
    ;;                 (change-sublocks-min-pitch (om::object editor) (min-pitch-flag (om::object editor)) (min-pitch (om::object editor)))
    ;;   )
    ;; )

    ;; (om::om-make-dialog-item
    ;;   'om::slider
    ;;   (om::om-make-point 190 250)
    ;;   (om::om-make-point 180 20)
    ;;   "Minimum pitch"
    ;;   :range '(1 127)
    ;;   :increment 1
    ;;   :value (min-pitch (om::object editor))
    ;;   :di-action #'(lambda (s)
    ;;     (setf (min-pitch (om::object editor)) (om::om-slider-value s))
    ;;     (change-sublocks-min-pitch (om::object editor) (min-pitch-flag (om::object editor)) (min-pitch (om::object editor)))
    ;;   )
    ;; )

    ;; (om::om-make-dialog-item
    ;;   'om::om-static-text
    ;;   (om::om-make-point 15 300)
    ;;   (om::om-make-point 200 20)
    ;;   "Maximum pitch"
    ;;   :font om::*om-default-font1b*
    ;; )

    ;; (om::om-make-dialog-item
    ;;   'om::om-check-box
    ;;   (om::om-make-point 170 300)
    ;;   (om::om-make-point 20 20)
    ;;   ""
    ;;   :checked-p (max-pitch-flag (om::object editor))
    ;;   :di-action #'(lambda (c)
    ;;                 (if (om::om-checked-p c)
    ;;                   (setf (max-pitch-flag (om::object editor)) 1)
    ;;                   (setf (max-pitch-flag (om::object editor)) nil)
    ;;                 )
    ;;                 (change-sublocks-max-pitch (om::object editor) (max-pitch-flag (om::object editor)) (max-pitch (om::object editor)))
    ;;   )
    ;; )

    ;; (om::om-make-dialog-item
    ;;   'om::slider
    ;;   (om::om-make-point 190 300)
    ;;   (om::om-make-point 180 20)
    ;;   "Maximum pitch"
    ;;   :range '(1 127)
    ;;   :increment 1
    ;;   :value (max-pitch (om::object editor))
    ;;   :di-action #'(lambda (s)
    ;;     (setf (max-pitch (om::object editor)) (om::om-slider-value s))
    ;;     (change-sublocks-max-pitch (om::object editor) (max-pitch-flag (om::object editor)) (max-pitch (om::object editor)))
    ;;   )
    ;; )



    ;; (om::om-make-dialog-item
    ;;   'om::om-static-text
    ;;   (om::om-make-point 15 400)
    ;;   (om::om-make-point 200 20)
    ;;   "Maximum notes"
    ;;   :font om::*om-default-font1b*
    ;; )

    ;; (om::om-make-dialog-item
    ;;   'om::pop-up-menu
    ;;   (om::om-make-point 170 400)
    ;;   (om::om-make-point 200 20)
    ;;   "Maximum notes"
    ;;   :range (append '("None") (loop :for n :from 0 :upto 100 collect n))
    ;;   ;; :value (max-notes (om::object editor))
    ;;   :di-action #'(lambda (m)
    ;;     (setq check (nth (om::om-get-selected-item-index m) (om::om-get-item-list m)))
    ;;     (if (typep check 'string)
    ;;       (setf (max-notes (om::object editor)) nil)
    ;;       (setf (max-notes (om::object editor)) check))
    ;;   )
    ;; )

  )

)

(defun make-rock-search-panel (editor search-panel)
  (om::om-add-subviews
    search-panel
    (om::om-make-dialog-item
      'om::om-static-text
      (om::om-make-point 145 2)
      (om::om-make-point 120 20)
      "Search Parameters"
      :font om::*om-default-font1b*
    )

    (om::om-make-dialog-item
      'om::om-button
      (om::om-make-point 5 50) ; position (horizontal, vertical)
      (om::om-make-point 130 20) ; size (horizontal, vertical)
      "Start"
      :di-action #'(lambda (b)
        (let ((init (rock-solver (om::object editor) (percent-diff (om::object editor)) (branching (om::object editor)))))
          ;; (setq init (new-melodizer (block-csp (om::object editor)) (percent-diff (om::object editor)) (branching (om::object editor))))
          (setf (result (om::object editor)) init)
          ; TO TEST THE GOLOMB RULER PROGRAM
          ;(setq init (golomb-ruler 5))
          ;; (setf (result (om::object editor)) init)
        )
      )
    )

    (om::om-make-dialog-item
      'om::om-button
      (om::om-make-point 135 50) ; position
      (om::om-make-point 130 20) ; size
      "Next"
      :di-action #'(lambda (b)
        (if (typep (result (om::object editor)) 'null); if the problem is not initialized
          (error "The problem has not been initialized. Please set the input and press Start.")
        )
        (print "Searching for the next solution")
        ;reset the boolean because we want to continue the search
        (setf (stop-search (om::object editor)) nil)
        ;get the next solution
        (mp:process-run-function ; start a new thread for the execution of the next method
          "next thread" ; name of the thread, not necessary but useful for debugging
          nil ; process initialization keywords, not needed here
          (lambda () ; function to call
            (setf (solution (om::object editor)) (new-rock-next (result (om::object editor)) (om::object editor)))
            ;TO TEST THE GOLOMB-RULER PROGRAM
            ;(setf (solution (om::object editor)) (search-next-golomb-ruler (result (om::object editor))))
            (om::openeditorframe ; open a voice window displaying the solution
              (om::omNG-make-new-instance (solution (om::object editor)) "current solution")
            )
          )
        )
      )
    )

    (om::om-make-dialog-item
        'om::om-button
        (om::om-make-point 265 50) ; position (horizontal, vertical)
        (om::om-make-point 130 20) ; size (horizontal, vertical)
        "Stop"
        :di-action #'(lambda (b)
          (setf (stop-search (om::object editor)) t)
        )
    )

    (om::om-make-dialog-item
      'om::om-static-text
      (om::om-make-point 15 100)
      (om::om-make-point 200 20)
      "Tempo (BPM)"
      :font om::*om-default-font1b*
    )

    (om::om-make-dialog-item
      'om::pop-up-menu
      (om::om-make-point 170 100)
      (om::om-make-point 200 20)
      "Tempo"
      :range (loop :for n :from 30 :upto 200 collect n)
      :di-action #'(lambda (m)
        (setf (tempo (om::object editor)) (nth (om::om-get-selected-item-index m) (om::om-get-item-list m)))
      )
    )

    (om::om-make-dialog-item
      'om::om-static-text
      (om::om-make-point 15 150)
      (om::om-make-point 200 20)
      "Branching"
      :font om::*om-default-font1b*
    )

    (om::om-make-dialog-item
      'om::pop-up-menu
      (om::om-make-point 170 150)
      (om::om-make-point 200 20)
      "Branching"
      :range '("Top down" "Full" "Top down random")
      :di-action #'(lambda (m)
        (setf (branching (om::object editor)) (nth (om::om-get-selected-item-index m) (om::om-get-item-list m)))
      )
    )


    (om::om-make-dialog-item
      'om::om-static-text
      (om::om-make-point 15 200)
      (om::om-make-point 200 20)
      "Difference Percentage"
      :font om::*om-default-font1b*
    )

    (om::om-make-dialog-item
      'om::slider
      (om::om-make-point 170 200)
      (om::om-make-point 200 20)
      "Difference Percentage"
      :range '(0 100)
      :increment 1
      :di-action #'(lambda (s)
        (setf (percent-diff (om::object editor)) (om::om-slider-value s))
      )
    )
  )

)

