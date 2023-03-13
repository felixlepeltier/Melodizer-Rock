(in-package :mldz)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                         ROCK CLASS                          ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; block list should be composed of the meta blocks forming the song
;; such as A A B A in our case ?
;; same thing should be the case for A and B with their sub blocks
;; s r d c for example
(om::defclass! rock ()
    (
    ;; add the blocks in block-list instead of using the A-first, A- ... for the 
    ;; fixed AABA structure
    ;; (setq L '(1 2 3))
    ;; >> (1 2 3)
    ;; (setq L (append L '(4)))
    ;; >> (1 2 3 4)
    (block-list :accessor block-list :initarg :block-list :initform nil :documentation "")
    ;; (struct-list :accessor struct-list :initarg :struct-list :initform nil :documentation "")
    ;; (A-first :accessor A-first :initarg :A-first :initform nil :documentation "")
    ;; (A-second :accessor A-second :initarg :A-second :initform nil :documentation "")
    ;; (object-B :accessor object-B :initarg :object-B :initform nil :documentation "")
    ;; (A-third :accessor A-third :initarg :A-third :initform nil :documentation "")
    (voices :accessor voices :initform nil :type integer)
    (chord-key :accessor chord-key :initform nil :type string)
    (bar-length :accessor bar-length :initform 0 :type integer)
    (min-pitch :accessor min-pitch :initform 1 :type integer)
    (min-pitch-flag :accessor min-pitch-flag :initform nil :type integer)
    (max-pitch :accessor max-pitch :initform 127 :type integer)
    (max-pitch-flag :accessor max-pitch-flag :initform nil :type integer)
    (beat-length :accessor beat-length :initform 0 :type integer)
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
  ;; (if (not (slot-value self 'block-list))
  ;;   (let*
  ;;     (
  ;;       ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;       ;;; setting the different regions of the tool ;;;
  ;;       ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  ;;       (rock-panel (om::om-make-view 'om::om-view
  ;;         :size (om::om-make-point 500 150)
  ;;         :position (om::om-make-point 5 5)
  ;;         :bg-color om::*azulito*)
  ;;       )
  ;;       (constraints-panel (om::om-make-view 'om::om-view
  ;;         :size (om::om-make-point 500 500)
  ;;         :position (om::om-make-point 5 160)
  ;;         :bg-color om::*azulito*)
  ;;       )
  ;;       ;; (structure-panel (om::om-make-view 'om::om-view
  ;;       ;;   :size (om::om-make-point 200 655)
  ;;       ;;   :position (om::om-make-point 510 5)
  ;;       ;;   :bg-color om::*azulito*)
  ;;       ;; )
  ;;     )

  ;;     (setf elements-rock-panel (make-rock-panel self rock-panel))
  ;;     (setf elements-constraints-panel (make-constraints-panel self constraints-panel))
  ;;     (setf elements-structure-panel (make-structure-panel self structure-panel))

  ;;     ; add the subviews for the different parts into the main view
  ;;     (om::om-add-subviews
  ;;       self
  ;;       rock-panel
  ;;       constraints-panel
  ;;       structure-panel
  ;;     )
  ;;   )
  ;; )
  ;; (if ((slot-value self 'block-list))
  ;;   (let*
  ;;     (
  ;;       ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;       ;;; setting the different regions of the tool ;;;
  ;;       ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  ;;       (rock-panel (om::om-make-view 'om::om-view
  ;;         :size (om::om-make-point 500 150)
  ;;         :position (om::om-make-point 5 5)
  ;;         :bg-color om::*azulito*)
  ;;       )
  ;;       (constraints-panel (om::om-make-view 'om::om-view
  ;;         :size (om::om-make-point 500 500)
  ;;         :position (om::om-make-point 5 160)
  ;;         :bg-color om::*azulito*)
  ;;       )
  ;;       (structure-panel (om::om-make-view 'om::om-view
  ;;         :size (om::om-make-point 200 655)
  ;;         :position (om::om-make-point 510 5)
  ;;         :bg-color om::*azulito*)
  ;;       )
  ;;     )

  ;;     (setf elements-rock-panel (make-rock-panel self rock-panel))
  ;;     (setf elements-constraints-panel (make-constraints-panel self constraints-panel))
  ;;     (setf elements-structure-panel (make-structure-panel self structure-panel))

  ;;     ; add the subviews for the different parts into the main view
  ;;     (om::om-add-subviews
  ;;       self
  ;;       rock-panel
  ;;       constraints-panel
  ;;       structure-panel
  ;;     )
  ;;   )
  ;; )

  (let*
    (
      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      ;;; setting the different regions of the tool ;;;
      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

      (rock-panel (om::om-make-view 'om::om-view
        :size (om::om-make-point 500 150)
        :position (om::om-make-point 5 5)
        :bg-color om::*azulito*)
      )
      (constraints-panel (om::om-make-view 'om::om-view
        :size (om::om-make-point 500 500)
        :position (om::om-make-point 5 160)
        :bg-color om::*azulito*)
      )
      (structure-panel (om::om-make-view 'om::om-view
        :size (om::om-make-point 200 655)
        :position (om::om-make-point 510 5)
        :bg-color om::*azulito*)
      )
    )

    (setf elements-rock-panel (make-rock-panel self rock-panel))
    (setf elements-constraints-panel (make-constraints-panel self constraints-panel))
    (setf elements-structure-panel (make-structure-panel self structure-panel))

    ; add the subviews for the different parts into the main view
    (om::om-add-subviews
      self
      rock-panel
      constraints-panel
      structure-panel
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
      (s-block :accessor s-block :initarg :s-block :initform nil :documentation "")
      (r-block :accessor r-block :initarg :r-block :initform nil :documentation "")
      (d-block :accessor d-block :initarg :d-block :initform nil :documentation "")
      (c-block :accessor c-block :initarg :c-block :initform nil :documentation "")
      (parent :accessor parent :initarg :parent :initform nil :documentation "")
      (chord-key :accessor chord-key :initform nil :type string)
      (bar-length :accessor bar-length :initform 0 :type integer)
      (min-pitch :accessor min-pitch :initform 1 :type integer)
      (min-pitch-flag :accessor min-pitch-flag :initform nil :type integer)
      (max-pitch :accessor max-pitch :initform 127 :type integer)
      (max-pitch-flag :accessor max-pitch-flag :initform nil :type integer)
      (beat-length :accessor beat-length :initform 0 :type integer)
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
        :size (om::om-make-point 600 150)
        :position (om::om-make-point 5 5)
        :bg-color om::*azulito*)
      )
       (constraints-panel (om::om-make-view 'om::om-view
        :size (om::om-make-point 500 500)
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
      (s-block :accessor s-block :initarg :s-block :initform nil :documentation "")
      (r-block :accessor r-block :initarg :r-block :initform nil :documentation "")
      (d-block :accessor d-block :initarg :d-block :initform nil :documentation "")
      (c-block :accessor c-block :initarg :c-block :initform nil :documentation "")
      (parent :accessor parent :initarg :parent :initform nil :documentation "")
      (chord-key :accessor chord-key :initform nil :type string)
      (bar-length :accessor bar-length :initform 0 :type integer)
      (min-pitch :accessor min-pitch :initform 1 :type integer)
      (min-pitch-flag :accessor min-pitch-flag :initform nil :type integer)
      (max-pitch :accessor max-pitch :initform 127 :type integer)
      (max-pitch-flag :accessor max-pitch-flag :initform nil :type integer)
      (beat-length :accessor beat-length :initform 0 :type integer)
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
        :size (om::om-make-point 600 150)
        :position (om::om-make-point 5 5)
        :bg-color om::*azulito*)
      )
       (constraints-panel (om::om-make-view 'om::om-view
        :size (om::om-make-point 500 500)
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
    ((block-list :accessor block-list :initarg :block-list :initform nil :documentation "")
    (parent :accessor parent :initarg :parent :initform nil :documentation "")
    (chord-key :accessor chord-key :initform nil :type string)
    (bar-length :accessor bar-length :initform 0 :type integer)
    (min-pitch :accessor min-pitch :initform 1 :type integer)
    (min-pitch-flag :accessor min-pitch-flag :initform nil :type integer)
    (max-pitch :accessor max-pitch :initform 127 :type integer)
    (max-pitch-flag :accessor max-pitch-flag :initform nil :type integer)
    (beat-length :accessor beat-length :initform 0 :type integer)
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
        :size (om::om-make-point 500 500)
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
    ((block-list :accessor block-list :initarg :block-list :initform nil :documentation "")
    (parent :accessor parent :initarg :parent :initform nil :documentation "")
    (chord-key :accessor chord-key :initform nil :type string)
    (bar-length :accessor bar-length :initform 0 :type integer)
    (min-pitch :accessor min-pitch :initform 1 :type integer)
    (min-pitch-flag :accessor min-pitch-flag :initform nil :type integer)
    (max-pitch :accessor max-pitch :initform 127 :type integer)
    (max-pitch-flag :accessor max-pitch-flag :initform nil :type integer)
    (beat-length :accessor beat-length :initform 0 :type integer)
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
        :size (om::om-make-point 500 500)
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
    ((block-list :accessor block-list :initarg :block-list :initform nil :documentation "")
    (parent :accessor parent :initarg :parent :initform nil :documentation "")
    (chord-key :accessor chord-key :initform nil :type string)
    (bar-length :accessor bar-length :initform 0 :type integer)
    (min-pitch :accessor min-pitch :initform 1 :type integer)
    (min-pitch-flag :accessor min-pitch-flag :initform nil :type integer)
    (max-pitch :accessor max-pitch :initform 127 :type integer)
    (max-pitch-flag :accessor max-pitch-flag :initform nil :type integer)
    (beat-length :accessor beat-length :initform 0 :type integer)
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
        :size (om::om-make-point 500 500)
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
    ((block-list :accessor block-list :initarg :block-list :initform nil :documentation "")
    (parent :accessor parent :initarg :parent :initform nil :documentation "")
    (chord-key :accessor chord-key :initform nil :type string)
    (bar-length :accessor bar-length :initform 0 :type integer)
    (min-pitch :accessor min-pitch :initform 1 :type integer)
    (min-pitch-flag :accessor min-pitch-flag :initform nil :type integer)
    (max-pitch :accessor max-pitch :initform 127 :type integer)
    (max-pitch-flag :accessor max-pitch-flag :initform nil :type integer)
    (beat-length :accessor beat-length :initform 0 :type integer)
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
        :size (om::om-make-point 500 500)
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


    ;; (setq L '(1 2 3))
    ;; >> (1 2 3)
    ;; (setq L (append L '(4)))
    ;; >> (1 2 3 4)


(defun make-rock-panel (editor rock-panel)
  (om::om-add-subviews
    rock-panel
    ;; (om::om-make-dialog-item
    ;;     'om::om-button
    ;;     (om::om-make-point 5 10) ; position (horizontal, vertical)
    ;;     (om::om-make-point 150 50) ; size (horizontal, vertical)
    ;;     "Add A to structure"
    ;;     :di-action #'(lambda (b)
    ;;       (print "Added A to structure")
    ;;       (setf (struct-list (om::object editor)) (append (struct-list (om::object editor)) '(nil)))
    ;;       (print (struct-list (om::object editor)))
    ;;     )
    ;; )

    ;; (om::om-make-dialog-item
    ;;     'om::om-button
    ;;     (om::om-make-point 5 65) ; position (horizontal, vertical)
    ;;     (om::om-make-point 150 50) ; size (horizontal, vertical)
    ;;     "Add B to structure"
    ;;     :di-action #'(lambda (b)
    ;;       (print "Added B to structure")
    ;;       (setf (struct-list (om::object editor)) (append (struct-list (om::object editor)) '(t)))
    ;;       (print (struct-list (om::object editor)))
    ;;     )
    ;; )

    (om::om-make-dialog-item
        'om::om-button
        (om::om-make-point 5 10) ; position (horizontal, vertical)
        (om::om-make-point 150 50) ; size (horizontal, vertical)
        "Add A to structure"
        :di-action #'(lambda (b)
        
          (print "Added A to structure")
          (setf (block-list (om::object editor)) (append (block-list (om::object editor)) (list (make-instance 'A :parent (om::object editor) (om::object editor)))))
          ;; (lambda () ; function to call
            
          ;;   ;; (print (block-list (om::object editor)))
          ;;   (print "after appending new block to block-list")
          ;; )
          ;; (print "after lambda function")
          ;; (print (block-list (om::object editor)))
          (print (block-list (om::object editor)))
        )
    )

    

    (om::om-make-dialog-item
        'om::om-button
        (om::om-make-point 5 65) ; position (horizontal, vertical)
        (om::om-make-point 150 50) ; size (horizontal, vertical)
        "Add B to structure"
        :di-action #'(lambda (b)
          ;; check if block-list is nill before adding; it is
          ;; (print (block-list (om::object editor)))
          (print "Added B to structure")
          ;; (let (new-b-block ((make-instance 'B :parent (om::object editor) (om::object editor)))))
          (setf (block-list (om::object editor)) (append (block-list (om::object editor)) (list (make-instance 'B :parent (om::object editor) (om::object editor)))))
          ;; (lambda () ; function to call
            
          ;;   ;; (print (block-list (om::object editor)))
          ;;   (print "after appending new block to block-list")
          ;; )
          ;; (print "after lambda function")
          ;; (print (block-list (om::object editor)))
          
          ;; (if (block-list (om::object editor))
          ;;   (loop for x in (block-list (om::object editor))
          ;;     do (print (typep x 'b))
          ;;   )
          ;; )
          (print (block-list (om::object editor)))
        )
    )
    

    (om::om-make-dialog-item
        'om::om-button
        (om::om-make-point 5 120) ; position (horizontal, vertical)
        (om::om-make-point 150 50) ; size (horizontal, vertical)
        "Done"
        :di-action #'(lambda (b)
          (print "Finished structure")
          (mp:process-run-function ; start a new thread for the execution of the next method
            "next thread" ; name of the thread, not necessary but useful for debugging
            nil ; process initialization keywords, not needed here
            (lambda () ; function to call
              (make-my-interface editor)
              (print "after make-my-interface in lambda function")
            )
          )
        )
    )
    ;; (loop for x in (block-list (om::object editor))
    ;;   collect (* x 10))

    
    ;; (om::om-make-dialog-item
    ;;     'om::om-button
    ;;     (om::om-make-point 5 10) ; position (horizontal, vertical)
    ;;     (om::om-make-point 100 50) ; size (horizontal, vertical)
    ;;     "A"
    ;;     :di-action #'(lambda (b)
        
    ;;       (print "Selected A1")

    ;;       (mp:process-run-function ; start a new thread for the execution of the next method
    ;;         "next thread" ; name of the thread, not necessary but useful for debugging
    ;;         nil ; process initialization keywords, not needed here
    ;;         (lambda () ; function to call
    ;;           (setf (A-first (om::object editor)) (make-instance 'A :parent (om::object editor) (om::object editor)))
    ;;           (om::openeditorframe ; open a window displaying the editor of the first A block
    ;;             (om::omNG-make-new-instance (A-first (om::object editor)) "Window A1")
    ;;           )
    ;;         )
    ;;       )
    ;;     )
    ;; )


    ;; (om::om-make-dialog-item
    ;;     'om::om-button
    ;;     (om::om-make-point 115 10) ; position (horizontal, vertical)
    ;;     (om::om-make-point 100 50) ; size (horizontal, vertical)
    ;;     "A"
    ;;     :di-action #'(lambda (b)
        
    ;;       (print "Selected A2")

    ;;       (mp:process-run-function ; start a new thread for the execution of the next method
    ;;         "next thread" ; name of the thread, not necessary but useful for debugging
    ;;         nil ; process initialization keywords, not needed here
    ;;         (lambda () ; function to call
    ;;           (setf (A-second (om::object editor)) (make-instance 'A :parent (om::object editor) (om::object editor)))
    ;;           (om::openeditorframe ; open a voice window displaying the solution
    ;;             (om::omNG-make-new-instance (A-second (om::object editor)) "Window A2")
    ;;           )
    ;;         )
    ;;       )
    ;;     )
    ;; )
    ;; (om::om-make-dialog-item
    ;;     'om::om-button
    ;;     (om::om-make-point 225 10) ; position (horizontal, vertical)
    ;;     (om::om-make-point 100 50) ; size (horizontal, vertical)
    ;;     "B"
    ;;     :di-action #'(lambda (b)
        
    ;;       (print "Selected B")

    ;;       (mp:process-run-function ; start a new thread for the execution of the next method
    ;;         "next thread" ; name of the thread, not necessary but useful for debugging
    ;;         nil ; process initialization keywords, not needed here
    ;;         (lambda () ; function to call
    ;;           (setf (object-B (om::object editor)) (make-instance 'B :parent (om::object editor) (om::object editor)))
    ;;           (om::openeditorframe ; open a voice window displaying the solution
    ;;             (om::omNG-make-new-instance (object-B (om::object editor)) "Window B")
    ;;           )
    ;;         )
    ;;       )
    ;;     )
    ;; )
    ;; (om::om-make-dialog-item
    ;;     'om::om-button
    ;;     (om::om-make-point 335 10) ; position (horizontal, vertical)
    ;;     (om::om-make-point 100 50) ; size (horizontal, vertical)
    ;;     "A"
    ;;     :di-action #'(lambda (b)
        
    ;;       (print "Selected A3")

    ;;       (mp:process-run-function ; start a new thread for the execution of the next method
    ;;         "next thread" ; name of the thread, not necessary but useful for debugging
    ;;         nil ; process initialization keywords, not needed here
    ;;         (lambda () ; function to call
    ;;           (setf (A-third (om::object editor)) (make-instance 'A :parent (om::object editor) (om::object editor)))
    ;;           (om::openeditorframe ; open a voice window displaying the solution
    ;;             (om::omNG-make-new-instance (A-third (om::object editor)) "Window A3")
    ;;           )
    ;;         )
    ;;       )
    ;;     )
    ;; )
  )
)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                  STRUCTURE INTERFACE                        ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; if we have access to the rock-editor
(defun make-structure-panel (editor structure-panel)
  ;; (print "in make-structure-panel")
  ;; (print (struct-list (om::object editor)))
  (defvar loop-index 0)
  (defvar subview-list '())
  (loop for x in (block-list (om::object editor))
    do 
      (setq loop-index (+ loop-index 1))
      (print (type-of x))
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
              (lambda () ; function to call
                (om::openeditorframe ; open a window displaying the editor of the first A block
                  (om::omNG-make-new-instance (x) (concatenate 'string "Window A" (write-to-string loop-in)))
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
              (lambda () ; function to call
                (om::openeditorframe ; open a window displaying the editor of the first A block
                  (om::omNG-make-new-instance (x) (concatenate 'string "Window B" (write-to-string loop-in)))
                )
              )
            )
          )
        ))))
      )

  )
  ;; Make a list of subviews based off of the info gathered in struct-list
  ;; then give this list of subviews to om-add-subviews
  ;; in struct-list the blocks are encoded as such 
  ;; A == nil
  ;; B == t

  (print "before printing subview list")
  (print subview-list)
  

  (if (not subview-list)
    (om::om-add-subviews
      structure-panel
    )
    
    (om::om-add-subviews
      structure-panel
      subview-list
    )
    ;; (
    ;;   (remove nil subview-list)
    ;;   (om::om-add-subviews
    ;;     structure-panel
    ;;     subview-list
    ;;   )
    ;; )
  )
)

  ;; (om::om-make-dialog-item
  ;;     'om::om-button
  ;;     (om::om-make-point 5 10) ; position (horizontal, vertical)
  ;;     (om::om-make-point 100 50) ; size (horizontal, vertical)
  ;;     "A"
  ;;     :di-action #'(lambda (b)
      
  ;;       (print "Selected A1")

  ;;       (mp:process-run-function ; start a new thread for the execution of the next method
  ;;         "next thread" ; name of the thread, not necessary but useful for debugging
  ;;         nil ; process initialization keywords, not needed here
  ;;         (lambda () ; function to call
  ;;           (setf (A-first (om::object editor)) (make-instance 'A :parent (om::object editor) (om::object editor)))
  ;;           (om::openeditorframe ; open a window displaying the editor of the first A block
  ;;             (om::omNG-make-new-instance (A-first (om::object editor)) "Window A1")
  ;;           )
  ;;         )
  ;;       )
  ;;     )
  ;; )

;;     (om::om-make-dialog-item
;;         'om::om-button
;;         (om::om-make-point 5 10) ; position (horizontal, vertical)
;;         (om::om-make-point 150 50) ; size (horizontal, vertical)
;;         "Add A to structure"
;;         :di-action #'(lambda (b)
        
;;           (print "Added A to structure")
;;           ;; (om::om-make-dialog-item
;;           ;;     'om::om-button
;;           ;;     (om::om-make-point 5 70) ; position (horizontal, vertical)
;;           ;;     (om::om-make-point 100 50) ; size (horizontal, vertical)
;;           ;;     "New button"
;;           ;; )

;;         )
;;     )

;;     (om::om-make-dialog-item
;;         'om::om-button
;;         (om::om-make-point 5 65) ; position (horizontal, vertical)
;;         (om::om-make-point 150 50) ; size (horizontal, vertical)
;;         "Add B to structure"
;;         :di-action #'(lambda (b)
        
;;           (print "Added B to structure")
;;           ;; (om::om-make-dialog-item
;;           ;;     'om::om-button
;;           ;;     (om::om-make-point 5 70) ; position (horizontal, vertical)
;;           ;;     (om::om-make-point 100 50) ; size (horizontal, vertical)
;;           ;;     "New button"
;;           ;; )

;;         )
;;     )

;;     (om::om-make-dialog-item
;;         'om::om-button
;;         (om::om-make-point 5 120) ; position (horizontal, vertical)
;;         (om::om-make-point 150 50) ; size (horizontal, vertical)
;;         "Done"
;;         :di-action #'(lambda (b)
;;           (print "Finished structure")
;;           (mp:process-run-function ; start a new thread for the execution of the next method
;;             "next thread" ; name of the thread, not necessary but useful for debugging
;;             nil ; process initialization keywords, not needed here
;;             (lambda () ; function to call
;;               ;; (make-my-interface )
;;             )
;;           )
;;         )
;;     )
;;   )
;; )



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
              (setf (s-block (om::object editor)) (make-instance 's :parent (om::object editor) (om::object editor)))
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
              (setf (r-block (om::object editor)) (make-instance 'r :parent (om::object editor) (om::object editor)))
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
              (setf (d-block (om::object editor)) (make-instance 'd :parent (om::object editor) (om::object editor)))
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
              (setf (c-block (om::object editor)) (make-instance 'c :parent (om::object editor) (om::object editor)))
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
              (setf (s-block (om::object editor)) (make-instance 's :parent (om::object editor) (om::object editor)))
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
              (setf (r-block (om::object editor)) (make-instance 'r :parent (om::object editor) (om::object editor)))
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
              (setf (d-block (om::object editor)) (make-instance 'd :parent (om::object editor) (om::object editor)))
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
              (setf (c-block (om::object editor)) (make-instance 'c :parent (om::object editor) (om::object editor)))
              (om::openeditorframe ; open a window displaying the editor of the first A block
                (om::omNG-make-new-instance (c-block (om::object editor)) "Window c")
              )
            )
          )
        )
    )
  )
)

(defun make-s-panel (editor s-panel)

)

(defun make-r-panel (editor r-panel)

)

(defun make-d-panel (editor d-panel)

)

(defun make-c-panel (editor c-panel)

)

(defun make-constraints-panel (editor panel)
  (om::om-add-subviews
    panel
    (om::om-make-dialog-item
      'om::om-static-text
      (om::om-make-point 150 2)
      (om::om-make-point 120 20)
      "Block constraints"
      :font om::*om-default-font1b*
    )

    (om::om-make-dialog-item
      'om::om-static-text
      (om::om-make-point 15 50)
      (om::om-make-point 200 20)
      "Bar length"
      :font om::*om-default-font1b*
    )

    (om::om-make-dialog-item
      'om::pop-up-menu
      (om::om-make-point 170 50)
      (om::om-make-point 200 20)
      "Bar length"
      :range (loop :for n :from 0 :upto 32 collect n)
      :di-action #'(lambda (m)
        (setf (bar-length (om::object editor)) (nth (om::om-get-selected-item-index m) (om::om-get-item-list m)))
      )
    )

    (om::om-make-dialog-item
      'om::om-static-text
      (om::om-make-point 15 100)
      (om::om-make-point 200 20)
      "Voices"
      :font om::*om-default-font1b*
    )

    (om::om-make-dialog-item
      'om::pop-up-menu
      (om::om-make-point 170 100)
      (om::om-make-point 200 20)
      "Voices"
      :range (append '("None") (loop :for n :from 0 :upto 15 collect n))
      :di-action #'(lambda (m)
        (setq check (nth (om::om-get-selected-item-index m) (om::om-get-item-list m)))
        (if (typep check 'string)
          (setf (voices (om::object editor)) nil)
          (setf (voices (om::object editor)) check))
      )
    )

    (om::om-make-dialog-item
      'om::om-static-text
      (om::om-make-point 15 150)
      (om::om-make-point 200 20)
      "Chord key"
      :font om::*om-default-font1b*
    )

    (om::om-make-dialog-item
      'om::pop-up-menu
      (om::om-make-point 170 150)
      (om::om-make-point 200 20)
      "Chord key"
      :range '("None" "C" "C#" "D" "Eb" "E" "F" "F#" "G" "Ab" "A" "Bb" "B")
      :di-action #'(lambda (m)
        (setq check (nth (om::om-get-selected-item-index m) (om::om-get-item-list m)))
        (if (string= check "None")
          (setf (chord-key (om::object editor)) nil)
          (setf (chord-key (om::object editor)) check))
      )
    )
     (om::om-make-dialog-item
      'om::om-static-text
      (om::om-make-point 15 250)
      (om::om-make-point 200 20)
      "Minimum pitch"
      :font om::*om-default-font1b*
    )

    (om::om-make-dialog-item
      'om::om-check-box
      (om::om-make-point 170 250)
      (om::om-make-point 20 20)
      ""
      :di-action #'(lambda (c)
                    (if (om::om-checked-p c)
                      (setf (min-pitch-flag (om::object editor)) 1)
                      (setf (min-pitch-flag (om::object editor)) nil)
                    )
      )
    )

    (om::om-make-dialog-item
      'om::slider
      (om::om-make-point 190 250)
      (om::om-make-point 180 20)
      "Minimum pitch"
      :range '(1 127)
      :increment 1
      :di-action #'(lambda (s)
        (setf (min-pitch (om::object editor)) (om::om-slider-value s))
      )
    )

    (om::om-make-dialog-item
      'om::om-static-text
      (om::om-make-point 15 300)
      (om::om-make-point 200 20)
      "Maximum pitch"
      :font om::*om-default-font1b*
    )

    (om::om-make-dialog-item
      'om::om-check-box
      (om::om-make-point 170 300)
      (om::om-make-point 20 20)
      ""
      :di-action #'(lambda (c)
                    (if (om::om-checked-p c)
                      (setf (max-pitch-flag (om::object editor)) 1)
                      (setf (max-pitch-flag (om::object editor)) nil)
                    )
      )
    )

    (om::om-make-dialog-item
      'om::slider
      (om::om-make-point 190 300)
      (om::om-make-point 180 20)
      "Maximum pitch"
      :range '(1 127)
      :increment 1
      :di-action #'(lambda (s)
        (setf (max-pitch (om::object editor)) (om::om-slider-value s))
      )
    )

  )

)

