(in-package :mldz)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                          accompniment CLASS                 ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(om::defclass! accompaniment ()
    (
      (parent :accessor parent :initarg :parent :initform nil :documentation "")
      (relative-to-parent :accessor relative-to-parent :initarg :relative-to-parent :initform 1 :type integer)
      (melody-source :accessor melody-source :initarg :melody-source :initform nil :documentation "")
      (bar-length :accessor bar-length :initform 0 :type integer)
      (min-simultaneous-notes :accessor min-simultaneous-notes :initform 0 :type integer)
      (diff-max-sim :accessor diff-max-sim :initform 0 :type integer :documentation "Difference for relative changes")
      (max-simultaneous-notes :accessor max-simultaneous-notes :initform 3 :type integer)
      (diff-min-sim :accessor diff-min-sim :initform 0 :type integer :documentation "Difference for relative changes")
      (min-note-length-flag :accessor min-note-length-flag :initform 1 :type integer)
      (min-note-length :accessor min-note-length :initform 16 :type integer)
      (diff-min-length :accessor diff-min-length :initform 0 :type integer :documentation "Difference for relative changes")
      (max-note-length-flag :accessor max-note-length-flag :initform 1 :type integer)
      (max-note-length :accessor max-note-length :initform 16 :type integer)
      (diff-max-length :accessor diff-max-length :initform 0 :type integer :documentation "Difference for relative changes")
      ;(quantification :accessor quantification :initform nil :type string)
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