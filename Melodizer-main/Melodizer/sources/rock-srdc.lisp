(in-package :mldz)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                          s CLASS                            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(om::defclass! s ()
    (
      (parent :accessor parent :initarg :parent :initform nil :documentation "")
      (accomp :accessor accomp :initarg :accomp :initform (make-instance 'accompaniment))
      (relative-to-parent :accessor relative-to-parent :initarg :relative-to-parent :initform 1 :type integer)
      (melody-source :accessor melody-source :initarg :melody-source :initform nil :documentation "")
      (bar-length :accessor bar-length :initform 0 :type integer)
      (min-simultaneous-notes :accessor min-simultaneous-notes :initform 0 :type integer)
      (diff-max-sim :accessor diff-max-sim :initform 0 :type integer :documentation "Difference for relative changes")
      (max-simultaneous-notes :accessor max-simultaneous-notes :initform 10 :type integer)
      (diff-min-sim :accessor diff-min-sim :initform 0 :type integer :documentation "Difference for relative changes")
      (min-notes :accessor min-notes :initform nil :type integer)
      (max-notes :accessor max-notes :initform nil :type integer)
      (min-note-length-flag :accessor min-note-length-flag :initform nil :type integer)
      (min-note-length :accessor min-note-length :initform 1 :type integer)
      (diff-min-length :accessor diff-min-length :initform 0 :type integer :documentation "Difference for relative changes")
      (max-note-length-flag :accessor max-note-length-flag :initform nil :type integer)
      (max-note-length :accessor max-note-length :initform 16 :type integer)
      (diff-max-length :accessor diff-max-length :initform 0 :type integer :documentation "Difference for relative changes")
      ;; (quantification :accessor quantification :initform nil :type string)
      (key-selection :accessor key-selection :initform nil :type string)
      (diff-key-selection :accessor diff-key-selection :initform 0 :type integer :documentation "Difference for relative changes")
      (mode-selection :accessor mode-selection :initform nil :type string)
      (diff-mode-selection :accessor diff-mode-selection :initform 0 :type integer :documentation "Difference for relative changes")
      (chord-key :accessor chord-key :initform nil :type string)
      (diff-chord-key :accessor diff-chord-key :initform 0 :type integer :documentation "Difference for relative changes")
      (chord-quality :accessor chord-quality :initform nil :type string)
      (diff-chord-quality :accessor diff-chord-quality :initform 0 :type integer :documentation "Difference for relative changes")
      (min-pitch :accessor min-pitch :initform 1 :type integer)
      (diff-min-pitch :accessor diff-min-pitch :initform 0 :type integer :documentation "Difference for relative changes")
      (min-pitch-flag :accessor min-pitch-flag :initform nil :type integer)
      (max-pitch :accessor max-pitch :initform 127 :type integer)
      (diff-max-pitch :accessor diff-max-pitch :initform 0 :type integer :documentation "Difference for relative changes")
      (max-pitch-flag :accessor max-pitch-flag :initform nil :type integer)
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
      (accomp :accessor accomp :initarg :accomp :initform (make-instance 'accompaniment))
      (relative-to-parent :accessor relative-to-parent :initarg :relative-to-parent :initform 1 :type integer)
      (melody-source :accessor melody-source :initarg :melody-source :initform nil :documentation "")
      (bar-length :accessor bar-length :initform 0 :type integer)
      (min-simultaneous-notes :accessor min-simultaneous-notes :initform 0 :type integer)
      (diff-max-sim :accessor diff-max-sim :initform 0 :type integer :documentation "Difference for relative changes")
      (max-simultaneous-notes :accessor max-simultaneous-notes :initform 10 :type integer)
      (diff-min-sim :accessor diff-min-sim :initform 0 :type integer :documentation "Difference for relative changes")
      (min-notes :accessor min-notes :initform nil :type integer)
      (max-notes :accessor max-notes :initform nil :type integer)
      (min-note-length-flag :accessor min-note-length-flag :initform nil :type integer)
      (min-note-length :accessor min-note-length :initform 1 :type integer)
      (diff-min-length :accessor diff-min-length :initform 0 :type integer :documentation "Difference for relative changes")
      (max-note-length-flag :accessor max-note-length-flag :initform nil :type integer)
      (max-note-length :accessor max-note-length :initform 16 :type integer)
      (diff-max-length :accessor diff-max-length :initform 0 :type integer :documentation "Difference for relative changes")
      ;; (quantification :accessor quantification :initform nil :type string)
      (key-selection :accessor key-selection :initform nil :type string)
      (diff-key-selection :accessor diff-key-selection :initform 0 :type integer :documentation "Difference for relative changes")
      (mode-selection :accessor mode-selection :initform nil :type string)
      (diff-mode-selection :accessor diff-mode-selection :initform 0 :type integer :documentation "Difference for relative changes")
      (chord-key :accessor chord-key :initform nil :type string)
      (diff-chord-key :accessor diff-chord-key :initform 0 :type integer :documentation "Difference for relative changes")
      (chord-quality :accessor chord-quality :initform nil :type string)
      (diff-chord-quality :accessor diff-chord-quality :initform 0 :type integer :documentation "Difference for relative changes")
      (min-pitch :accessor min-pitch :initform 1 :type integer)
      (diff-min-pitch :accessor diff-min-pitch :initform 0 :type integer :documentation "Difference for relative changes")
      (min-pitch-flag :accessor min-pitch-flag :initform nil :type integer)
      (max-pitch :accessor max-pitch :initform 127 :type integer)
      (diff-max-pitch :accessor diff-max-pitch :initform 0 :type integer :documentation "Difference for relative changes")
      (max-pitch-flag :accessor max-pitch-flag :initform nil :type integer)
      (similarity-percent-s :accessor similarity-percent-s :initform 1 :type integer)
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
      (r-constraints-panel (om::om-make-view 'om::om-view
        :size (om::om-make-point 300 500)
        :position (om::om-make-point 820 5)
        :bg-color om::*azulito*)
      )
    )

    (setf elements-constraints-panel (make-constraints-panel self constraints-panel))
    (setf elements-r-constraints-panel (make-r-constraints-panel self r-constraints-panel))

    ; add the subviews for the different parts into the main view
    (om::om-add-subviews
      self
      constraints-panel
      r-constraints-panel
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
      (parent :accessor parent :initarg :parent :initform nil :documentation "")
      (accomp :accessor accomp :initarg :accomp :initform (make-instance 'accompaniment))
      (relative-to-parent :accessor relative-to-parent :initarg :relative-to-parent :initform 1 :type integer)
      (melody-source :accessor melody-source :initarg :melody-source :initform nil :documentation "")
      (bar-length :accessor bar-length :initform 0 :type integer)
      (min-simultaneous-notes :accessor min-simultaneous-notes :initform 0 :type integer)
      (diff-max-sim :accessor diff-max-sim :initform 0 :type integer :documentation "Difference for relative changes")
      (max-simultaneous-notes :accessor max-simultaneous-notes :initform 10 :type integer)
      (diff-min-sim :accessor diff-min-sim :initform 0 :type integer :documentation "Difference for relative changes")
      (min-notes :accessor min-notes :initform nil :type integer)
      (max-notes :accessor max-notes :initform nil :type integer)
      (min-note-length-flag :accessor min-note-length-flag :initform nil :type integer)
      (min-note-length :accessor min-note-length :initform 1 :type integer)
      (diff-min-length :accessor diff-min-length :initform 0 :type integer :documentation "Difference for relative changes")
      (max-note-length-flag :accessor max-note-length-flag :initform nil :type integer)
      (max-note-length :accessor max-note-length :initform 16 :type integer)
      (diff-max-length :accessor diff-max-length :initform 0 :type integer :documentation "Difference for relative changes")
      ;; (quantification :accessor quantification :initform nil :type string)
      (key-selection :accessor key-selection :initform nil :type string)
      (diff-key-selection :accessor diff-key-selection :initform 0 :type integer :documentation "Difference for relative changes")
      (mode-selection :accessor mode-selection :initform nil :type string)
      (diff-mode-selection :accessor diff-mode-selection :initform 0 :type integer :documentation "Difference for relative changes")
      (chord-key :accessor chord-key :initform nil :type string)
      (diff-chord-key :accessor diff-chord-key :initform 0 :type integer :documentation "Difference for relative changes")
      (chord-quality :accessor chord-quality :initform nil :type string)
      (diff-chord-quality :accessor diff-chord-quality :initform 0 :type integer :documentation "Difference for relative changes")
      (min-pitch :accessor min-pitch :initform 1 :type integer)
      (diff-min-pitch :accessor diff-min-pitch :initform 0 :type integer :documentation "Difference for relative changes")
      (min-pitch-flag :accessor min-pitch-flag :initform nil :type integer)
      (max-pitch :accessor max-pitch :initform 127 :type integer)
      (diff-max-pitch :accessor diff-max-pitch :initform 0 :type integer :documentation "Difference for relative changes")
      (max-pitch-flag :accessor max-pitch-flag :initform nil :type integer)
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
      (parent :accessor parent :initarg :parent :initform nil :documentation "")
      (accomp :accessor accomp :initarg :accomp :initform (make-instance 'accompaniment))
      (relative-to-parent :accessor relative-to-parent :initarg :relative-to-parent :initform 1 :type integer)
      (melody-source :accessor melody-source :initarg :melody-source :initform nil :documentation "")
      (bar-length :accessor bar-length :initform 0 :type integer)
      (min-simultaneous-notes :accessor min-simultaneous-notes :initform 0 :type integer)
      (diff-max-sim :accessor diff-max-sim :initform 0 :type integer :documentation "Difference for relative changes")
      (max-simultaneous-notes :accessor max-simultaneous-notes :initform 10 :type integer)
      (diff-min-sim :accessor diff-min-sim :initform 0 :type integer :documentation "Difference for relative changes")
      (min-notes :accessor min-notes :initform nil :type integer)
      (max-notes :accessor max-notes :initform nil :type integer)
      (min-note-length-flag :accessor min-note-length-flag :initform nil :type integer)
      (min-note-length :accessor min-note-length :initform 1 :type integer)
      (diff-min-length :accessor diff-min-length :initform 0 :type integer :documentation "Difference for relative changes")
      (max-note-length-flag :accessor max-note-length-flag :initform nil :type integer)
      (max-note-length :accessor max-note-length :initform 16 :type integer)
      (diff-max-length :accessor diff-max-length :initform 0 :type integer :documentation "Difference for relative changes")
      ;; (quantification :accessor quantification :initform nil :type string)
      (key-selection :accessor key-selection :initform nil :type string)
      (diff-key-selection :accessor diff-key-selection :initform 0 :type integer :documentation "Difference for relative changes")
      (mode-selection :accessor mode-selection :initform nil :type string)
      (diff-mode-selection :accessor diff-mode-selection :initform 0 :type integer :documentation "Difference for relative changes")
      (chord-key :accessor chord-key :initform nil :type string)
      (diff-chord-key :accessor diff-chord-key :initform 0 :type integer :documentation "Difference for relative changes")
      (chord-quality :accessor chord-quality :initform nil :type string)
      (diff-chord-quality :accessor diff-chord-quality :initform 0 :type integer :documentation "Difference for relative changes")
      (min-pitch :accessor min-pitch :initform 1 :type integer)
      (diff-min-pitch :accessor diff-min-pitch :initform 0 :type integer :documentation "Difference for relative changes")
      (min-pitch-flag :accessor min-pitch-flag :initform nil :type integer)
      (max-pitch :accessor max-pitch :initform 127 :type integer)
      (diff-max-pitch :accessor diff-max-pitch :initform 0 :type integer :documentation "Difference for relative changes")
      (max-pitch-flag :accessor max-pitch-flag :initform nil :type integer)
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

;; r-constraints-panel interface building

(defun make-r-constraints-panel (editor panel)
  (om::om-add-subviews
    panel
    (om::om-make-dialog-item
      'om::om-static-text
      (om::om-make-point 10 10)
      (om::om-make-point 200 20)
      "Similarity with s block"
      :font om::*om-default-font1b*
    )
    (om::om-make-dialog-item
      'om::slider
      (om::om-make-point 10 40)
      (om::om-make-point 80 20)
      "Similarity with s block"
      :range '(1 100)
      :increment 1
      :value (similarity-percent-s (om::object editor))
      :di-action #'(lambda (s)
        (setf (similarity-percent-s (om::object editor)) (om::om-slider-value s))
        (print "similarity-percent-s: ")
        (print (similarity-percent-s (om::object editor)))
      )
    )
  )
)

(defun make-constraints-srdc-panel (editor panel)
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
        (setf (bar-length (om::object editor)) (string-to-number check))
        (set-bar-length-up (om::object editor))
      )
    )

    (om::om-make-dialog-item
      'om::om-static-text
      (om::om-make-point 15 150)
      (om::om-make-point 200 20)
      "Minimum simultaneous notes"
      :font om::*om-default-font1b*
    )

    (om::om-make-dialog-item
      'om::pop-up-menu
      (om::om-make-point 170 150)
      (om::om-make-point 80 20)
      "Minimum simultaneous notes"
      :range (loop :for n :from 0 :upto 10 collect (number-to-string n))
      :value (number-to-string (min-simultaneous-notes (om::object editor)))
      :di-action #'(lambda (m)
          (setq check (nth (om::om-get-selected-item-index m) (om::om-get-item-list m)))
          (setf (min-simultaneous-notes (om::object editor)) (string-to-number check))
          (change-subblocks-values (om::object editor) 
                                  :min-simultaneous-notes (min-simultaneous-notes (om::object editor)))
      )
    )

    (om::om-make-dialog-item
      'om::om-static-text
      (om::om-make-point 15 200)
      (om::om-make-point 200 20)
      "Maximum simultaneous notes"
      :font om::*om-default-font1b*
    )

    (om::om-make-dialog-item
      'om::pop-up-menu
      (om::om-make-point 170 200)
      (om::om-make-point 80 20)
      "Maximum simultaneous notes"
      :range  (loop :for n :from 0 :upto 10 :collect (number-to-string n))
      :value (number-to-string (max-simultaneous-notes (om::object editor)))
      :di-action #'(lambda (m)
          (setq check (nth (om::om-get-selected-item-index m) (om::om-get-item-list m)))
          (setf (max-simultaneous-notes (om::object editor)) (string-to-number check))
          (change-subblocks-values (om::object editor) 
                                  :max-simultaneous-notes (max-simultaneous-notes (om::object editor)))
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
      :checked-p (min-note-length-flag (om::object editor))
      :di-action #'(lambda (c)
                    (if (om::om-checked-p c)
                      (setf (min-note-length-flag (om::object editor)) 1)
                      (setf (min-note-length-flag (om::object editor)) nil)
                    )
                    (change-subblocks-values (om::object editor) 
                                :min-note-length-flag (min-note-length-flag (om::object editor)) 
                                :min-note-length (min-note-length (om::object editor)))     
      )
    )

    (om::om-make-dialog-item
      'om::pop-up-menu
      (om::om-make-point 450 50)
      (om::om-make-point 80 20); size
      "Minimum note length"
      :range  (loop :for n :from 0 :upto 4 :collect (number-to-string (expt 2 n)))
      :value (number-to-string (min-note-length (om::object editor)))
      :di-action #'(lambda (m)
          (setq check (nth (om::om-get-selected-item-index m) (om::om-get-item-list m)))
          (setf (min-note-length (om::object editor)) (string-to-number check))
          (change-subblocks-values (om::object editor) 
                                  :min-note-length-flag (min-note-length-flag (om::object editor))
                                  :min-note-length (min-note-length (om::object editor)))
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
      :checked-p (max-note-length-flag (om::object editor))
      :di-action #'(lambda (c)
                    (if (om::om-checked-p c)
                      (setf (max-note-length-flag (om::object editor)) 1)
                      (setf (max-note-length-flag (om::object editor)) nil)
                    )
                    (change-subblocks-values (om::object editor) 
                                :max-note-length-flag (max-note-length-flag (om::object editor)) 
                                :max-note-length (max-note-length (om::object editor)))
      )
    )

    (om::om-make-dialog-item
      'om::pop-up-menu
      (om::om-make-point 450 100)
      (om::om-make-point 80 20); size
      "Maximum note length"
      :range  (loop :for n :from 0 :upto 4 :collect (number-to-string (expt 2 n)))
      :value (number-to-string (max-note-length (om::object editor)))
      :di-action #'(lambda (m)
          (setq check (nth (om::om-get-selected-item-index m) (om::om-get-item-list m)))
          (setf (max-note-length (om::object editor)) (string-to-number check))
          (change-subblocks-values (om::object editor) 
                                  :max-note-length-flag (max-note-length-flag (om::object editor))
                                  :max-note-length (max-note-length (om::object editor)))
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
      :value (key-selection (om::object editor))
      :di-action #'(lambda (m)
        (setq check (nth (om::om-get-selected-item-index m) (om::om-get-item-list m)))
        (if (string= check "None")
          (setf (key-selection (om::object editor)) nil)
          (setf (key-selection (om::object editor)) check)
        )
      )
    )

    ; Mode
    (om::om-make-dialog-item
      'om::om-static-text
      (om::om-make-point 550 100)
      (om::om-make-point 200 20)
      "Mode selection"
      :font om::*om-default-font1b*
    )

    (om::om-make-dialog-item
      'om::pop-up-menu
      (om::om-make-point 670 100)
      (om::om-make-point 80 20)
      "Mode selection"
      :value (mode-selection (om::object editor))
      :range '("None" "ionian (major)" "dorian" "phrygian" "lydian" "mixolydian" "aeolian (natural minor)" "locrian" "pentatonic" "harmonic minor" "chromatic")
      :di-action #'(lambda (m)
        (setq check (nth (om::om-get-selected-item-index m) (om::om-get-item-list m)))
        (if (string= check "None")
          (setf (mode-selection (om::object editor)) nil)
          (setf (mode-selection (om::object editor)) check))
      )
    )

    (om::om-make-dialog-item
      'om::om-static-text
      (om::om-make-point 550 150)
      (om::om-make-point 200 20)
      "Chord key"
      :font om::*om-default-font1b*
    )

    (om::om-make-dialog-item
      'om::pop-up-menu
      (om::om-make-point 670 150)
      (om::om-make-point 80 20)
      "Chord key"
      :range '("None" "C" "C#" "D" "Eb" "E" "F" "F#" "G" "Ab" "A" "Bb" "B")
      :value (chord-key (om::object editor))
      :di-action #'(lambda (m)
        (setq check (nth (om::om-get-selected-item-index m) (om::om-get-item-list m)))
        (if (string= check "None")
          (setf (chord-key (om::object editor)) nil)
          (setf (chord-key (om::object editor)) check)
        )
        
        (change-subblocks-values (om::object editor) :chord-key check)
        
      )
    )

    (om::om-make-dialog-item
      'om::om-static-text
      (om::om-make-point 550 200)
      (om::om-make-point 200 20)
      "Chord quality"
      :font om::*om-default-font1b*
    )

    (om::om-make-dialog-item
      'om::pop-up-menu
      (om::om-make-point 670 200)
      (om::om-make-point 80 20)
      "Chord quality"
      :value (chord-quality (om::object editor))
      :range '("None" "Major" "Minor" "Augmented" "Diminished" "Major 7" "Minor 7" "Dominant 7" "Minor 7 flat 5" "Diminished 7" "Minor-major 7"
        "Major 9" "Minor 9" "9 Augmented 5" "9 flatted 5" "7 flat 9" "Augmented 9" "Minor 11" "Major 11" "Dominant 11" "Dominant # 11" "Major # 11")
      :di-action #'(lambda (m)
        (setq check (nth (om::om-get-selected-item-index m) (om::om-get-item-list m)))
        (if (string= check "None")
          (setf (chord-quality (om::object editor)) nil)
          (setf (chord-quality (om::object editor)) check))
        
      )
    )

     (om::om-make-dialog-item
      'om::om-static-text
      (om::om-make-point 550 250)
      (om::om-make-point 200 20)
      "Minimum pitch"
      :font om::*om-default-font1b*
    )

    (om::om-make-dialog-item
      'om::om-check-box
      (om::om-make-point 670 250)
      (om::om-make-point 20 20)
      ""
      :checked-p (min-pitch-flag (om::object editor))
      :di-action #'(lambda (c)
                    (if (om::om-checked-p c)
                      (setf (min-pitch-flag (om::object editor)) 1)
                      (setf (min-pitch-flag (om::object editor)) nil)
                    )
                    (change-subblocks-values (om::object editor) 
                                :min-pitch-flag (min-pitch-flag (om::object editor)) 
                                :min-pitch (min-pitch (om::object editor)))
      )
    )

    (om::om-make-dialog-item
      'om::slider
      (om::om-make-point 690 250)
      (om::om-make-point 80 20)
      "Minimum pitch"
      :range '(1 127)
      :increment 1
      :value (min-pitch (om::object editor))
      :di-action #'(lambda (s)
        (setf (min-pitch (om::object editor)) (om::om-slider-value s))
          (change-subblocks-values (om::object editor) 
                                  :min-pitch-flag (min-pitch-flag (om::object editor)) 
                                  :min-pitch (min-pitch (om::object editor)))
      )
    )

    (om::om-make-dialog-item
      'om::om-static-text
      (om::om-make-point 550 300)
      (om::om-make-point 200 20)
      "Maximum pitch"
      :font om::*om-default-font1b*
    )

    (om::om-make-dialog-item
      'om::om-check-box
      (om::om-make-point 670 300)
      (om::om-make-point 20 20)
      ""
      :checked-p (max-pitch-flag (om::object editor))
      :di-action #'(lambda (c)
                    (if (om::om-checked-p c)
                      (setf (max-pitch-flag (om::object editor)) 1)
                      (setf (max-pitch-flag (om::object editor)) nil)
                    )
                    (change-subblocks-values (om::object editor) 
                                            :max-pitch-flag (max-pitch-flag (om::object editor)) 
                                            :max-pitch (max-pitch (om::object editor)))
      )
    )

    (om::om-make-dialog-item
      'om::slider
      (om::om-make-point 690 300)
      (om::om-make-point 80 20)
      "Maximum pitch"
      :range '(1 127)
      :increment 1
      :value (max-pitch (om::object editor))
      :di-action #'(lambda (s)
        (setf (max-pitch (om::object editor)) (om::om-slider-value s))
          (change-subblocks-values (om::object editor) 
                                  :max-pitch-flag (max-pitch-flag (om::object editor)) 
                                  :max-pitch (max-pitch (om::object editor)))
      )
    )
  )

)
