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
      (bar-length :accessor bar-length :initform 0 :type integer)
      (min-note-length-flag :accessor min-note-length-flag :initform nil :type integer)
      (min-note-length :accessor min-note-length :initform 1 :type integer)
      (diff-min-length :accessor diff-min-length :initform 0 :type integer :documentation "Difference for relative changes")
      (max-note-length-flag :accessor max-note-length-flag :initform nil :type integer)
      (max-note-length :accessor max-note-length :initform 16 :type integer)
      (diff-max-length :accessor diff-max-length :initform 0 :type integer :documentation "Difference for relative changes")
      (chord-key :accessor chord-key :initform "C" :type string)
      (diff-chord-key :accessor diff-chord-key :initform 0 :type integer :documentation "Difference for relative changes")
      (chord-quality :accessor chord-quality :initform "Major" :type string)
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
        :size (om::om-make-point 500 300)
        :position (om::om-make-point 5 5)
        :bg-color om::*azulito*)
      )
      (accompaniment-panel (om::om-make-view 'om::om-view
        :size (om::om-make-point 300 300)
        :position (om::om-make-point 510 5)
        :bg-color om::*azulito*)
      )
    )

    (setf elements-constraints-panel (make-constraints-srdc-panel self constraints-panel))
    (setf elements-accompaniment-panel (make-accompaniment-panel self accompaniment-panel))

    ; add the subviews for the different parts into the main view
    (om::om-add-subviews
      self
      constraints-panel
      accompaniment-panel
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
      (bar-length :accessor bar-length :initform 0 :type integer)
      (min-note-length-flag :accessor min-note-length-flag :initform nil :type integer)
      (min-note-length :accessor min-note-length :initform 1 :type integer)
      (diff-min-length :accessor diff-min-length :initform 0 :type integer :documentation "Difference for relative changes")
      (max-note-length-flag :accessor max-note-length-flag :initform nil :type integer)
      (max-note-length :accessor max-note-length :initform 16 :type integer)
      (diff-max-length :accessor diff-max-length :initform 0 :type integer :documentation "Difference for relative changes")
      (chord-key :accessor chord-key :initform "C" :type string)
      (diff-chord-key :accessor diff-chord-key :initform 0 :type integer :documentation "Difference for relative changes")
      (chord-quality :accessor chord-quality :initform "Major" :type string)
      (diff-chord-quality :accessor diff-chord-quality :initform 0 :type integer :documentation "Difference for relative changes")
      (min-pitch :accessor min-pitch :initform 1 :type integer)
      (diff-min-pitch :accessor diff-min-pitch :initform 0 :type integer :documentation "Difference for relative changes")
      (min-pitch-flag :accessor min-pitch-flag :initform nil :type integer)
      (max-pitch :accessor max-pitch :initform 127 :type integer)
      (diff-max-pitch :accessor diff-max-pitch :initform 0 :type integer :documentation "Difference for relative changes")
      (max-pitch-flag :accessor max-pitch-flag :initform nil :type integer)
      (similarity-percent-s :accessor similarity-percent-s :initform 50 :type integer)
      (semitones :accessor semitones :initform 0 :type integer :documentation "Semitones of transposition from key")
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
        :size (om::om-make-point 500 195)
        :position (om::om-make-point 5 5)
        :bg-color om::*azulito*)
      )
      (r-constraints-panel (om::om-make-view 'om::om-view
        :size (om::om-make-point 500 100)
        :position (om::om-make-point 5 205)
        :bg-color om::*azulito*)
      )
      (accompaniment-panel (om::om-make-view 'om::om-view
        :size (om::om-make-point 300 300)
        :position (om::om-make-point 510 5)
        :bg-color om::*azulito*)
      )
    )

    (setf elements-constraints-panel (make-constraints-srdc-panel self constraints-panel))
    (setf elements-accompaniment-panel (make-accompaniment-panel self accompaniment-panel))
    (setf elements-r-constraints-panel (make-r-constraints-panel self r-constraints-panel))

    ; add the subviews for the different parts into the main view
    (om::om-add-subviews
      self
      constraints-panel
      accompaniment-panel
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
      (bar-length :accessor bar-length :initform 0 :type integer)
      (min-note-length-flag :accessor min-note-length-flag :initform nil :type integer)
      (min-note-length :accessor min-note-length :initform 1 :type integer)
      (diff-min-length :accessor diff-min-length :initform 0 :type integer :documentation "Difference for relative changes")
      (max-note-length-flag :accessor max-note-length-flag :initform nil :type integer)
      (max-note-length :accessor max-note-length :initform 16 :type integer)
      (diff-max-length :accessor diff-max-length :initform 0 :type integer :documentation "Difference for relative changes")
      (chord-key :accessor chord-key :initform "C" :type string)
      (diff-chord-key :accessor diff-chord-key :initform 0 :type integer :documentation "Difference for relative changes")
      (chord-quality :accessor chord-quality :initform "Major" :type string)
      (diff-chord-quality :accessor diff-chord-quality :initform 0 :type integer :documentation "Difference for relative changes")
      (min-pitch :accessor min-pitch :initform 1 :type integer)
      (diff-min-pitch :accessor diff-min-pitch :initform 0 :type integer :documentation "Difference for relative changes")
      (min-pitch-flag :accessor min-pitch-flag :initform nil :type integer)
      (max-pitch :accessor max-pitch :initform 127 :type integer)
      (diff-max-pitch :accessor diff-max-pitch :initform 0 :type integer :documentation "Difference for relative changes")
      (max-pitch-flag :accessor max-pitch-flag :initform nil :type integer)
      (difference-percent-s :accessor difference-percent-s :initform 75 :type integer)
      (semitones :accessor semitones :initform 0 :type integer :documentation "Semitones of transposition from key")
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
        :size (om::om-make-point 500 195)
        :position (om::om-make-point 5 5)
        :bg-color om::*azulito*)
      )
      (accompaniment-panel (om::om-make-view 'om::om-view
        :size (om::om-make-point 300 300)
        :position (om::om-make-point 510 5)
        :bg-color om::*azulito*)
      )
      (d-constraints-panel (om::om-make-view 'om::om-view
        :size (om::om-make-point 500 100)
        :position (om::om-make-point 5 205)
        :bg-color om::*azulito*)
      )
    )

    (setf elements-d-constraints-panel (make-d-constraints-panel self d-constraints-panel))

    ; add the subviews for the different parts into the main view
    (setf elements-constraints-panel (make-constraints-srdc-panel self constraints-panel))
    (setf elements-accompaniment-panel (make-accompaniment-panel self accompaniment-panel))

    ; add the subviews for the different parts into the main view
    (om::om-add-subviews
      self
      constraints-panel
      accompaniment-panel
      d-constraints-panel
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
      (bar-length :accessor bar-length :initform 0 :type integer)
      (min-note-length-flag :accessor min-note-length-flag :initform nil :type integer)
      (min-note-length :accessor min-note-length :initform 1 :type integer)
      (diff-min-length :accessor diff-min-length :initform 0 :type integer :documentation "Difference for relative changes")
      (max-note-length-flag :accessor max-note-length-flag :initform nil :type integer)
      (max-note-length :accessor max-note-length :initform 16 :type integer)
      (diff-max-length :accessor diff-max-length :initform 0 :type integer :documentation "Difference for relative changes")
      (chord-key :accessor chord-key :initform "C" :type string)
      (diff-chord-key :accessor diff-chord-key :initform 0 :type integer :documentation "Difference for relative changes")
      (chord-quality :accessor chord-quality :initform "Major" :type string)
      (diff-chord-quality :accessor diff-chord-quality :initform 0 :type integer :documentation "Difference for relative changes")
      (min-pitch :accessor min-pitch :initform 1 :type integer)
      (diff-min-pitch :accessor diff-min-pitch :initform 0 :type integer :documentation "Difference for relative changes")
      (min-pitch-flag :accessor min-pitch-flag :initform nil :type integer)
      (max-pitch :accessor max-pitch :initform 127 :type integer)
      (diff-max-pitch :accessor diff-max-pitch :initform 0 :type integer :documentation "Difference for relative changes")
      (max-pitch-flag :accessor max-pitch-flag :initform nil :type integer)
      (cadence-type :accessor cadence-type :initform "Perfect" :type string :documentation "Type of cadence used in the current block")
      (min-note-length-mult :accessor min-note-length-mult :initform 2 :type integer)
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
        :size (om::om-make-point 500 300)
        :position (om::om-make-point 5 5)
        :bg-color om::*azulito*)
      )
      ;; (accompaniment-panel (om::om-make-view 'om::om-view
      ;;   :size (om::om-make-point 300 300)
      ;;   :position (om::om-make-point 510 5)
      ;;   :bg-color om::*azulito*)
      ;; )
      (c-constraints-panel (om::om-make-view 'om::om-view
        :size (om::om-make-point 500 100)
        :position (om::om-make-point 5 310)
        :bg-color om::*azulito*)
      )
    )


    (setf elements-c-constraints-panel (make-c-constraints-panel self c-constraints-panel))

    ; add the subviews for the different parts into the main view
    (setf elements-constraints-panel (make-constraints-srdc-panel self constraints-panel))
    ;; (setf elements-accompaniment-panel (make-accompaniment-panel self accompaniment-panel))

    ; add the subviews for the different parts into the main view
    (om::om-add-subviews
      self
      constraints-panel
      ;; accompaniment-panel
      c-constraints-panel
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
      (om::om-make-point 150 20)
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

    (om::om-make-dialog-item
      'om::om-static-text
      (om::om-make-point 200 10)
      (om::om-make-point 100 50)
      "Semitones from s block"
      :font om::*om-default-font1b*
    )

    (om::om-make-dialog-item
      'om::pop-up-menu
      (om::om-make-point 300 10)
      (om::om-make-point 80 20)
      "semitones from s block"
      :range (loop :for i :from -12 :below 12 :collect (number-to-string i))
      :value (number-to-string (semitones (om::object editor)))
      :di-action #'(lambda (m)
        (setq check (nth (om::om-get-selected-item-index m) (om::om-get-item-list m)))
        (setf (semitones (om::object editor)) (string-to-number check))
      )
    )
  )
)

;; c-constraints-panel interface building

(defun make-c-constraints-panel (editor panel)
  (om::om-add-subviews
    panel
    (om::om-make-dialog-item
      'om::om-static-text
      (om::om-make-point 10 10)
      (om::om-make-point 200 20)
      "Cadence choice"
      :font om::*om-default-font1b*
    )
    (om::om-make-dialog-item
      'om::pop-up-menu
      (om::om-make-point 10 40)
      (om::om-make-point 150 20)
      "Cadence choice"
      :range '("Perfect" "Plagal" "Semi" "None")
      :value (cadence-type (om::object editor))
      :di-action #'(lambda (m)
        (setq check (nth (om::om-get-selected-item-index m) (om::om-get-item-list m)))
        (if (string= check "None")
          (setf (cadence-type (om::object editor)) "None")
          (setf (cadence-type (om::object editor)) check)
        )
      )
    )
  )
)

(defun make-d-constraints-panel (editor panel)
  (om::om-add-subviews
    panel
    (om::om-make-dialog-item
      'om::om-static-text
      (om::om-make-point 10 10)
      (om::om-make-point 200 20)
      "Difference with s block"
      :font om::*om-default-font1b*
    )
    (om::om-make-dialog-item
      'om::slider
      (om::om-make-point 10 40)
      (om::om-make-point 150 20)
      "Difference with s block"
      :range '(1 100)
      :increment 1
      :value (difference-percent-s (om::object editor))
      :di-action #'(lambda (s)
        (setf (difference-percent-s (om::object editor)) (om::om-slider-value s))
        (print "difference-percent-s: ")
        (print (difference-percent-s (om::object editor)))
      )
    )
    (om::om-make-dialog-item
      'om::om-static-text
      (om::om-make-point 200 10)
      (om::om-make-point 100 50)
      "Semitones from s block"
      :font om::*om-default-font1b*
    )

    (om::om-make-dialog-item
      'om::pop-up-menu
      (om::om-make-point 300 10)
      (om::om-make-point 80 20)
      "semitones from s block"
      :range (loop :for i :from -12 :below 12 :collect (number-to-string i))
      :value (number-to-string (semitones (om::object editor)))
      :di-action #'(lambda (m)
        (setq check (nth (om::om-get-selected-item-index m) (om::om-get-item-list m)))
        (setf (semitones (om::object editor)) (string-to-number check))
      )
    )
  )
)

(defun make-constraints-srdc-panel (editor panel)
  (om::om-add-subviews
    panel
    (om::om-make-dialog-item
      'om::om-static-text
      (om::om-make-point 15 10)
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



    ;; (om::om-make-dialog-item
    ;;   'om::om-static-text
    ;;   (om::om-make-point 300 2)
    ;;   (om::om-make-point 120 20)
    ;;   "Time constraints"
    ;;   :font om::*om-default-font1b*
    ;; )

    (om::om-make-dialog-item
      'om::om-static-text
      (om::om-make-point 15 100)
      (om::om-make-point 200 20)
      "Min note length"
      :font om::*om-default-font1b*
    )

    (om::om-make-dialog-item
      'om::om-check-box
      (om::om-make-point 120 100)
      (om::om-make-point 20 20)
      ""
      :checked-p (min-note-length-flag (om::object editor))
      :di-action #'(lambda (c)
                    (if (om::om-checked-p c)
                      (setf (min-note-length-flag (om::object editor)) 1)
                      (setf (min-note-length-flag (om::object editor)) nil)
                    )
      )
    )

    (om::om-make-dialog-item
      'om::pop-up-menu
      (om::om-make-point 170 100)
      (om::om-make-point 80 20); size
      "Minimum note length"
      :range  (loop :for n :from 0 :upto 4 :collect (number-to-string (expt 2 n)))
      :value (number-to-string (min-note-length (om::object editor)))
      :di-action #'(lambda (m)
        (let ((old-diff 0))
          (setq check (nth (om::om-get-selected-item-index m) (om::om-get-item-list m)))
          (if (relative-to-same (om::object editor))
              (setq old-diff (diff-min-length (om::object editor)))
          )
          (setf (min-note-length (om::object editor)) (string-to-number check))
        )
      )
    )

    (om::om-make-dialog-item
      'om::om-static-text
      (om::om-make-point 15 150)
      (om::om-make-point 200 20)
      "Max note length"
      :font om::*om-default-font1b*
    )

    (om::om-make-dialog-item
      'om::om-check-box
      (om::om-make-point 120 150)
      (om::om-make-point 20 20)
      ""
      :checked-p (max-note-length-flag (om::object editor))
      :di-action #'(lambda (c)
                    (if (om::om-checked-p c)
                      (setf (max-note-length-flag (om::object editor)) 1)
                      (setf (max-note-length-flag (om::object editor)) nil)
                    )
      )
    )

    (om::om-make-dialog-item
      'om::pop-up-menu
      (om::om-make-point 170 150)
      (om::om-make-point 80 20); size
      "Maximum note length"
      :range  (loop :for n :from 0 :upto 4 :collect (number-to-string (expt 2 n)))
      :value (number-to-string (max-note-length (om::object editor)))
      :di-action #'(lambda (m)
        (let ((old-diff 0))
          (setq check (nth (om::om-get-selected-item-index m) (om::om-get-item-list m)))
          (if (relative-to-same (om::object editor))
              (setq old-diff (diff-max-length (om::object editor)))
          )
          (setf (max-note-length (om::object editor)) (string-to-number check))        )
      )
    )

    (om::om-make-dialog-item
      'om::om-static-text
      (om::om-make-point 300 10)
      (om::om-make-point 200 20)
      "Pitch constraints"
      :font om::*om-default-font1b*
    )

    ; Key
    
    ;; (om::om-make-dialog-item
    ;;   'om::om-static-text
    ;;   (om::om-make-point 300 50)
    ;;   (om::om-make-point 200 20)
    ;;   "Chord key"
    ;;   :font om::*om-default-font1b*
    ;; )

    ;; (om::om-make-dialog-item
    ;;   'om::pop-up-menu
    ;;   (om::om-make-point 400 50)
    ;;   (om::om-make-point 80 20)
    ;;   "Chord key"
    ;;   :range '("C" "C#" "D" "Eb" "E" "F" "F#" "G" "Ab" "A" "Bb" "B")
    ;;   :value (chord-key (om::object editor))
    ;;   :di-action #'(lambda (m)
    ;;     (setq check (nth (om::om-get-selected-item-index m) (om::om-get-item-list m)))
    ;;     (if (string= check "None")
    ;;       (setf (chord-key (om::object editor)) nil)
    ;;       (setf (chord-key (om::object editor)) check)
    ;;     )      )
    ;; )

    ;; (om::om-make-dialog-item
    ;;   'om::om-static-text
    ;;   (om::om-make-point 300 100)
    ;;   (om::om-make-point 200 20)
    ;;   "Chord quality"
    ;;   :font om::*om-default-font1b*
    ;; )

    ;; (om::om-make-dialog-item
    ;;   'om::pop-up-menu
    ;;   (om::om-make-point 400 100)
    ;;   (om::om-make-point 80 20)
    ;;   "Chord quality"
    ;;   :value (chord-quality (om::object editor))
    ;;   :range '("Major" "Minor" "Augmented" "Diminished")
    ;;   :di-action #'(lambda (m)
    ;;     (setq check (nth (om::om-get-selected-item-index m) (om::om-get-item-list m)))
    ;;     (if (string= check "None")
    ;;       (setf (chord-quality (om::object editor)) nil)
    ;;       (setf (chord-quality (om::object editor)) check))        
    ;;   )
    ;; )

     (om::om-make-dialog-item
      'om::om-static-text
      (om::om-make-point 300 50)
      (om::om-make-point 200 20)
      "Minimum pitch"
      :font om::*om-default-font1b*
    )

    ;; (om::om-make-dialog-item
    ;;   'om::om-check-box
    ;;   (om::om-make-point 400 150)
    ;;   (om::om-make-point 20 20)
    ;;   ""
    ;;   :checked-p (min-pitch-flag (om::object editor))
    ;;   :di-action #'(lambda (c)
    ;;                 (if (om::om-checked-p c)
    ;;                   (setf (min-pitch-flag (om::object editor)) 1)
    ;;                   (setf (min-pitch-flag (om::object editor)) nil)
    ;;                 )
    ;;                 (change-subblocks-values (om::object editor) 
    ;;                             :min-pitch-flag (min-pitch-flag (om::object editor)) 
    ;;                             :min-pitch (min-pitch (om::object editor)))
    ;;   )
    ;; )

    (om::om-make-dialog-item
      'om::slider
      (om::om-make-point 300 70)
      (om::om-make-point 150 20)
      "Minimum pitch"
      :range '(1 127)
      :increment 1
      :value (min-pitch (om::object editor))
      :di-action #'(lambda (s)
        (setf (min-pitch (om::object editor)) (om::om-slider-value s))
      )
    )

    (om::om-make-dialog-item
      'om::om-static-text
      (om::om-make-point 300 120)
      (om::om-make-point 200 20)
      "Maximum pitch"
      :font om::*om-default-font1b*
    )

    ;; (om::om-make-dialog-item
    ;;   'om::om-check-box
    ;;   (om::om-make-point 400 220)
    ;;   (om::om-make-point 20 20)
    ;;   ""
    ;;   :checked-p (max-pitch-flag (om::object editor))
    ;;   :di-action #'(lambda (c)
    ;;                 (if (om::om-checked-p c)
    ;;                   (setf (max-pitch-flag (om::object editor)) 1)
    ;;                   (setf (max-pitch-flag (om::object editor)) nil)
    ;;                 )
    ;;                 (change-subblocks-values (om::object editor) 
    ;;                                         :max-pitch-flag (max-pitch-flag (om::object editor)) 
    ;;                                         :max-pitch (max-pitch (om::object editor)))
    ;;   )
    ;; )

    (om::om-make-dialog-item
      'om::slider
      (om::om-make-point 300 140)
      (om::om-make-point 150 20)
      "Maximum pitch"
      :range '(1 127)
      :increment 1
      :value (max-pitch (om::object editor))
      :di-action #'(lambda (s)
        (setf (max-pitch (om::object editor)) (om::om-slider-value s))
      )
    )
  )

)