(in-package :mldz)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                      ACCOMPANIMENT CLASS                    ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(om::defclass! accompaniment ()
    (
      (parent 
          :accessor parent :initarg :parent :initform nil 
          :documentation "parent block containing the instance of this block")
      (relative-to-parent 
          :accessor relative-to-parent :initarg :relative-to-parent :initform 1 :type integer
          :documentation "Flag to now if the block attributes are reltive to its parent's")
      (bar-length 
          :accessor bar-length :initform 0 :type integer
          :documentation "Number of bars of the block")
      (min-simultaneous-notes 
          :accessor min-simultaneous-notes :initform 3 :type integer
          :documentation "Minimum notes played simultaneously")
      (diff-max-sim 
          :accessor diff-max-sim :initform 0 :type integer 
          :documentation "Difference for relative changes")
      (max-simultaneous-notes 
          :accessor max-simultaneous-notes :initform 3 :type integer
          :documentation "Maximum notes played simultaneously")
      (diff-min-sim 
          :accessor diff-min-sim :initform 0 :type integer 
          :documentation "Difference for relative changes")
      (min-note-length-flag 
          :accessor min-note-length-flag :initform 1 :type integer
          :documentation "Flag stating if the note-min-length constrain must be posted")
      (min-note-length 
          :accessor min-note-length :initform 16 :type integer
          :documentation "Minimum note length value")
      (diff-min-length 
          :accessor diff-min-length :initform 0 :type integer 
          :documentation "Difference for relative changes")
      (max-note-length-flag 
          :accessor max-note-length-flag :initform 1 :type integer
          :documentation "Flag stating if the note-max-length constrain must be posted")
      (max-note-length 
          :accessor max-note-length :initform 16 :type integer
          :documentation "Maximum note length value")
      (diff-max-length 
          :accessor diff-max-length :initform 0 :type integer 
          :documentation "Difference for relative changes")
      (chord-key 
          :accessor chord-key :initform "C" :type string
          :documentation "Chord key to set the scale in")
      (diff-chord-key 
          :accessor diff-chord-key :initform 0 :type integer 
          :documentation "Difference for relative changes")
      (chord-quality 
          :accessor chord-quality :initform "Major" :type string
          :documentation "Quality to set the scale in")
      (diff-chord-quality 
          :accessor diff-chord-quality :initform 0 :type integer 
          :documentation "Difference for relative changes")
      (min-pitch 
          :accessor min-pitch :initform 1 :type integer
          :documentation "Minimum pitch value")
      (diff-min-pitch 
          :accessor diff-min-pitch :initform 0 :type integer 
          :documentation "Difference for relative changes")
      (max-pitch 
          :accessor max-pitch :initform 127 :type integer
          :documentation "Maximum pitch value")
      (diff-max-pitch 
          :accessor diff-max-pitch :initform 0 :type integer 
          :documentation "Difference for relative changes")
    )
)

(defun make-accompaniment-panel (editor panel)
  (om::om-add-subviews
    panel
  (om::om-make-dialog-item
      'om::om-static-text
      (om::om-make-point 15 10)
      (om::om-make-point 200 20)
      "Accompaniment constraints"
      :font om::*om-default-font1b*
    )

    (om::om-make-dialog-item
      'om::om-static-text
      (om::om-make-point 15 50)
      (om::om-make-point 200 20)
      "Min note length"
      :font om::*om-default-font1b*
    )

    (om::om-make-dialog-item
      'om::om-check-box
      (om::om-make-point 145 50)
      (om::om-make-point 20 20)
      ""
      :checked-p (min-note-length-flag (accomp (om::object editor)))
      :di-action #'(lambda (c)
                    (if (om::om-checked-p c)
                      (setf (min-note-length-flag (accomp (om::object editor))) 1)
                      (setf (min-note-length-flag (accomp (om::object editor))) nil)
                    )  
      )
    )

    (om::om-make-dialog-item
      'om::pop-up-menu
      (om::om-make-point 165 50)
      (om::om-make-point 80 20); size
      "Min note length"
      :range  (loop :for n :from 0 :upto 4 :collect (number-to-string (expt 2 n)))
      :value (number-to-string (min-note-length (accomp (om::object editor))))
      :di-action #'(lambda (m)
          (setq check (nth (om::om-get-selected-item-index m) (om::om-get-item-list m)))
          (setf (min-note-length (accomp (om::object editor))) (string-to-number check))
      )
    )

    (om::om-make-dialog-item
      'om::om-static-text
      (om::om-make-point 15 100)
      (om::om-make-point 200 20)
      "Max note length"
      :font om::*om-default-font1b*
    )

    (om::om-make-dialog-item
      'om::om-check-box
      (om::om-make-point 145 100)
      (om::om-make-point 20 20)
      ""
      :checked-p (max-note-length-flag (accomp (om::object editor)))
      :di-action #'(lambda (c)
                    (if (om::om-checked-p c)
                      (setf (max-note-length-flag (accomp (om::object editor))) 1)
                      (setf (max-note-length-flag (accomp (om::object editor))) nil)
                    )
      )
    )

    (om::om-make-dialog-item
      'om::pop-up-menu
      (om::om-make-point 165 100)
      (om::om-make-point 80 20); size
      "Max note length"
      :range  (loop :for n :from 0 :upto 4 :collect (number-to-string (expt 2 n)))
      :value (number-to-string (max-note-length (accomp (om::object editor))))
      :di-action #'(lambda (m)
          (setq check (nth (om::om-get-selected-item-index m) (om::om-get-item-list m)))
          (setf (max-note-length (accomp (om::object editor))) (string-to-number check))
      )
    )

    ; Key

    (om::om-make-dialog-item
      'om::om-static-text
      (om::om-make-point 15 150)
      (om::om-make-point 200 20)
      "Chord key"
      :font om::*om-default-font1b*
    )

    (om::om-make-dialog-item
      'om::pop-up-menu
      (om::om-make-point 165 150)
      (om::om-make-point 80 20)
      "Chord key"
      :range '("C" "C#" "D" "Eb" "E" "F" "F#" "G" "Ab" "A" "Bb" "B")
      :value (chord-key (accomp (om::object editor)))
      :di-action #'(lambda (m)
        (setq check (nth (om::om-get-selected-item-index m) (om::om-get-item-list m)))
        (if (string= check "None")
          (setf (chord-key (accomp (om::object editor))) nil)
          (setf (chord-key (accomp (om::object editor))) check)
        )
      )
    )

    (om::om-make-dialog-item
      'om::om-static-text
      (om::om-make-point 15 200)
      (om::om-make-point 200 20)
      "Chord quality"
      :font om::*om-default-font1b*
    )

    (om::om-make-dialog-item
      'om::pop-up-menu
      (om::om-make-point 165 200)
      (om::om-make-point 80 20)
      "Chord quality"
      :value (chord-quality (accomp (om::object editor)))
      :range '("Major" "Minor" "Augmented" "Diminished")
      :di-action #'(lambda (m)
        (setq check (nth (om::om-get-selected-item-index m) (om::om-get-item-list m)))
        (if (string= check "None")
          (setf (chord-quality (accomp (om::object editor))) nil)
          (setf (chord-quality (accomp (om::object editor))) check))
      )
    )
  )
)