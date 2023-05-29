(in-package :mldz)

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
      (relative-to-parent :accessor relative-to-parent :initarg :relative-to-parent :initform 1 :type integer)
      (relative-to-same :accessor relative-to-same :initarg :relative-to-same :initform nil :type integer)
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
      ;; (cadence-type :accessor cadence-type :initform "Perfect" :type string :documentation "Type of cadence used in the current block")
      (block-position :accessor block-position :initform -1 :type integer :documentation "Index of the A or B block within the global structure")
      (similarity-percent-A0 :accessor similarity-percent-A0 :initform 50 :type integer :documentation "Percentage of resemblance with first A")
      (block-position-A :accessor block-position-A :initform -1 :type integer :documentation "Index of the A or B block within the global structure")
      (block-position-B :accessor block-position-B :initform -1 :type integer :documentation "Index of the A or B block within the global structure")
      (semitones :accessor semitones :initform 0 :type integer :documentation "Semitones of transposition from key")
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
        :size (om::om-make-point 500 50)
        :position (om::om-make-point 5 5)
        :bg-color om::*azulito*)
      )
      (changes-panel (om::om-make-view 'om::om-view
        :size (om::om-make-point 500 100)
        :position (om::om-make-point 5 60)
        :bg-color om::*azulito*)
      )
      (constraints-panel (om::om-make-view 'om::om-view
        :size (om::om-make-point 500 300)
        :position (om::om-make-point 5 165)
        :bg-color om::*azulito*)
      )
      
    )

    (setf elements-A-panel (make-A-panel self A-panel))
    (print (block-position (om::object self)))
    (print (idx-first-a (parent (om::object self))))
    (if (= (block-position (om::object self)) (idx-first-a (parent (om::object self))))
      (setf elements-constraints-panel (make-constraints-AB-panel self constraints-panel))
      (setf elements-constraints-panel (make-constraints-not-first-panel self constraints-panel))
    )
    
    (setf elements-changes-panel (make-changes-panel self changes-panel))

    ; add the subviews for the different parts into the main view
    (om::om-add-subviews
      self
      A-panel
      changes-panel
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
      (relative-to-parent :accessor relative-to-parent :initarg :relative-to-parent :initform 1 :type integer)
      (relative-to-same :accessor relative-to-same :initarg :relative-to-same :initform nil :type integer)
      (bar-length :accessor bar-length :initform 0 :type integer)
      (min-notes :accessor min-notes :initform nil :type integer)
      (max-notes :accessor max-notes :initform nil :type integer)
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
      ;; (cadence-type :accessor cadence-type :initform "Perfect" :type string :documentation "Type of cadence used in the current block")
      (block-position :accessor block-position :initform -1 :type integer :documentation "Index of the A or B block within the global structure")
      (similarity-percent-B0 :accessor similarity-percent-B0 :initform 50 :type integer :documentation "Percentage of resemblance with first B")
      (block-position-A :accessor block-position-A :initform -1 :type integer :documentation "Index of the A or B block within the global structure")
      (block-position-B :accessor block-position-B :initform -1 :type integer :documentation "Index of the A or B block within the global structure")
      (semitones :accessor semitones :initform 0 :type integer :documentation "Semitones of transposition from key")
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
        :size (om::om-make-point 500 50)
        :position (om::om-make-point 5 5)
        :bg-color om::*azulito*)
      )
      (changes-panel (om::om-make-view 'om::om-view
        :size (om::om-make-point 500 100)
        :position (om::om-make-point 5 60)
        :bg-color om::*azulito*)
      )
      (constraints-panel (om::om-make-view 'om::om-view
        :size (om::om-make-point 500 300)
        :position (om::om-make-point 5 170)
        :bg-color om::*azulito*)
      )
      
    )

    (setf elements-B-panel (make-B-panel self B-panel))
    (if (= (block-position (om::object self)) (idx-first-b (parent (om::object self))))
      (setf elements-constraints-panel (make-constraints-AB-panel self constraints-panel))
      (setf elements-constraints-panel (make-constraints-not-first-panel self constraints-panel))
    )
    (setf elements-changes-panel (make-changes-panel self changes-panel))

    ; add the subviews for the different parts into the main view
    (om::om-add-subviews
      self
      B-panel
      changes-panel
      constraints-panel
    )
  )
  ; return the editor
  self
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                        A INTERFACE                          ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



(defun make-A-panel (editor A-panel)
  ;; (print "Block-position")
  ;; (print (block-position (om::object editor)))
  (om::om-add-subviews
    A-panel
    (om::om-make-dialog-item
        'om::om-button
        (om::om-make-point 5 10) ; position (horizontal, vertical)
        (om::om-make-point 80 25) ; size (horizontal, vertical)
        "s"
        :di-action #'(lambda (b)
          (print "Selected s")
          (mp:process-run-function ; start a new thread for the execution of the next method
            "next thread" ; name of the thread, not necessary but useful for debugging
            nil ; process initialization keywords, not needed here
            (lambda () ; function to call
                (setf (parent (s-block (om::object editor))) (om::object editor))
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
        (om::om-make-point 80 25) ; size (horizontal, vertical)
        "r"
        :di-action #'(lambda (b)
          (print "Selected r")
          (mp:process-run-function ; start a new thread for the execution of the next method
            "next thread" ; name of the thread, not necessary but useful for debugging
            nil ; process initialization keywords, not needed here
            (lambda () ; function to call
            (setf (parent (r-block (om::object editor))) (om::object editor))
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
        (om::om-make-point 80 25) ; size (horizontal, vertical)
        "d"
        :di-action #'(lambda (b)
          (print "Selected d")
          (mp:process-run-function ; start a new thread for the execution of the next method
            "next thread" ; name of the thread, not necessary but useful for debugging
            nil ; process initialization keywords, not needed here
            (lambda () ; function to call
                (setf (parent (d-block (om::object editor))) (om::object editor))
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
        (om::om-make-point 80 25) ; size (horizontal, vertical)
        "c"
        :di-action #'(lambda (b)
          (print "Selected c")
          (mp:process-run-function ; start a new thread for the execution of the next method
            "next thread" ; name of the thread, not necessary but useful for debugging
            nil ; process initialization keywords, not needed here
            (lambda () ; function to call
                (setf (parent (c-block (om::object editor))) (om::object editor))
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
  ;; (print "Block-position")
  ;; (print (block-position (om::object editor)))
  (om::om-add-subviews
    B-panel
    (om::om-make-dialog-item
        'om::om-button
        (om::om-make-point 5 10) ; position (horizontal, vertical)
        (om::om-make-point 80 25) ; size (horizontal, vertical)
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
        (om::om-make-point 80 25) ; size (horizontal, vertical)
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
        (om::om-make-point 80 25) ; size (horizontal, vertical)
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
        (om::om-make-point 80 25) ; size (horizontal, vertical)
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

(defun make-changes-panel (editor panel)
  (om::om-add-subviews
    panel
    (om::om-make-dialog-item
      'om::om-static-text
      (om::om-make-point 10 10)
      (om::om-make-point 300 20)
      "Types of changes"
      :font om::*om-default-font1b*
    )
    
    (om::om-make-dialog-item
      'om::om-check-box
      (om::om-make-point 10 30)
      (om::om-make-point 300 20)
      "Relative to rock"
      :checked-p (relative-to-parent (om::object editor))
      :di-action #'(lambda (c)
                    (if (om::om-checked-p c)
                      (setf (relative-to-parent (om::object editor)) 1)
                      (setf (relative-to-parent (om::object editor)) nil)
                    )
      )
    )

    (om::om-make-dialog-item
      'om::om-check-box
      (om::om-make-point 10 50)
      (om::om-make-point 300 20)
      "Relative to same type blocks"
      :checked-p (relative-to-same (om::object editor))
      :di-action #'(lambda (c)
                    (if (om::om-checked-p c)
                      (setf (relative-to-same (om::object editor)) 1)
                      (setf (relative-to-same (om::object editor)) nil)
                    )
      )
    )

    

  )
)

(defun make-constraints-AB-panel (editor panel)
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
        (change-subblocks-values (om::object editor) :bar-length (bar-length (om::object editor)))
        (propagate-bar-length-srdc (om::object editor))
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
                    (change-subblocks-values (om::object editor) 
                                :min-note-length-flag (min-note-length-flag (om::object editor)) 
                                :min-note-length (min-note-length (om::object editor)))     
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
          (change-subblocks-values (om::object editor) 
                                  :min-note-length-flag (min-note-length-flag (om::object editor))
                                  :min-note-length (min-note-length (om::object editor)))
          (if (relative-to-same (om::object editor))
              (propagate-AB (om::object editor) :diff-min-length (- old-diff (diff-min-length (om::object editor))))
          )
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
                    (change-subblocks-values (om::object editor) 
                                :max-note-length-flag (max-note-length-flag (om::object editor)) 
                                :max-note-length (max-note-length (om::object editor)))
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
          (setf (max-note-length (om::object editor)) (string-to-number check))
          (change-subblocks-values (om::object editor) 
                                  :max-note-length-flag (max-note-length-flag (om::object editor))
                                  :max-note-length (max-note-length (om::object editor)))
          (if (relative-to-same (om::object editor))
              (propagate-AB (om::object editor) :diff-max-length (- old-diff (diff-max-length (om::object editor))))
          )
        )
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

    (om::om-make-dialog-item
      'om::om-static-text
      (om::om-make-point 300 50)
      (om::om-make-point 200 20)
      "Chord key"
      :font om::*om-default-font1b*
    )

    (om::om-make-dialog-item
      'om::pop-up-menu
      (om::om-make-point 400 50)
      (om::om-make-point 80 20)
      "Chord key"
      :range '("C" "C#" "D" "Eb" "E" "F" "F#" "G" "Ab" "A" "Bb" "B")
      :value (chord-key (om::object editor))
      :di-action #'(lambda (m)
        (setq check (nth (om::om-get-selected-item-index m) (om::om-get-item-list m)))
        (if (string= check "None")
          (setf (chord-key (om::object editor)) nil)
          (setf (chord-key (om::object editor)) check)
        )
        (let ((old-diff 0))
          (if (relative-to-same (om::object editor))
              (setq old-diff (diff-chord-key (om::object editor)))
          )
          (change-subblocks-values (om::object editor) :chord-key check)
          (if (relative-to-same (om::object editor))
            (propagate-AB (om::object editor) :diff-chord-key (- old-diff (diff-chord-key (om::object editor))))
          )
        )
      )
    )

    (om::om-make-dialog-item
      'om::om-static-text
      (om::om-make-point 300 100)
      (om::om-make-point 200 20)
      "Chord quality"
      :font om::*om-default-font1b*
    )

    (om::om-make-dialog-item
      'om::pop-up-menu
      (om::om-make-point 400 100)
      (om::om-make-point 80 20)
      "Chord quality"
      :value (chord-quality (om::object editor))
      :range '("Major" "Minor" "Augmented" "Diminished")
      :di-action #'(lambda (m)
        (setq check (nth (om::om-get-selected-item-index m) (om::om-get-item-list m)))
        (if (string= check "None")
          (setf (chord-quality (om::object editor)) nil)
          (setf (chord-quality (om::object editor)) check))
        (change-subblocks-values (om::object editor) :chord-quality check)
        
      )
    )

     (om::om-make-dialog-item
      'om::om-static-text
      (om::om-make-point 300 150)
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
      (om::om-make-point 300 170)
      (om::om-make-point 150 20)
      "Minimum pitch"
      :range '(1 127)
      :increment 1
      :value (min-pitch (om::object editor))
      :di-action #'(lambda (s)
        (setf (min-pitch (om::object editor)) (om::om-slider-value s))
        (let ((old-diff 0))
          (if (relative-to-same (om::object editor))
              (setq old-diff (diff-min-pitch (om::object editor)))
          )
          (change-subblocks-values (om::object editor) 
                                  :min-pitch-flag (min-pitch-flag (om::object editor)) 
                                  :min-pitch (min-pitch (om::object editor)))
          (if (relative-to-same (om::object editor))
              (propagate-AB (om::object editor) :diff-min-pitch (- old-diff (diff-min-pitch (om::object editor))))
          )
        )
      )
    )

    (om::om-make-dialog-item
      'om::om-static-text
      (om::om-make-point 300 220)
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
      (om::om-make-point 300 240)
      (om::om-make-point 150 20)
      "Maximum pitch"
      :range '(1 127)
      :increment 1
      :value (max-pitch (om::object editor))
      :di-action #'(lambda (s)
        (setf (max-pitch (om::object editor)) (om::om-slider-value s))
        (let ((old-diff 0))
          (if (relative-to-same (om::object editor))
              (setq old-diff (diff-max-pitch (om::object editor)))
          )
          (change-subblocks-values (om::object editor) 
                                  :max-pitch-flag (max-pitch-flag (om::object editor)) 
                                  :max-pitch (max-pitch (om::object editor)))
          (if (relative-to-same (om::object editor))
              (propagate-AB (om::object editor) :diff-max-pitch (- old-diff (diff-max-pitch (om::object editor))))
          )
        )
      )
    )
  )

)

(defun make-constraints-not-first-panel (editor panel)
  (let ((subviews '()))
    (setf subviews (append subviews (list
      (om::om-make-dialog-item
      'om::om-static-text
      (om::om-make-point 250 10)
      (om::om-make-point 200 20)
      "Pitch constraints"
      :font om::*om-default-font1b*
      )

    ; Key
    ;; (om::om-make-dialog-item
    ;;   'om::om-static-text
    ;;   (om::om-make-point 250 50)
    ;;   (om::om-make-point 100 50)
    ;;   "Semitones from first A"
    ;;   :font om::*om-default-font1b*
    ;; )

    ;; (om::om-make-dialog-item
    ;;   'om::pop-up-menu
    ;;   (om::om-make-point 350 50)
    ;;   (om::om-make-point 80 20)
    ;;   "semitones from key"
    ;;   :range (loop :for i :from -12 :below 12 :collect (number-to-string i))
    ;;   :value (number-to-string (semitones (om::object editor)))
    ;;   :di-action #'(lambda (m)
    ;;     (setq check (nth (om::om-get-selected-item-index m) (om::om-get-item-list m)))
    ;;     (setf (semitones (om::object editor)) (string-to-number check))
    ;;     ;; (change-subblocks-values (om::object editor) :semitones (semitones (om::object editor)))
    ;;   )
    ;; )

    (om::om-make-dialog-item
      'om::om-static-text
      (om::om-make-point 250 50)
      (om::om-make-point 200 20)
      "Chord key"
      :font om::*om-default-font1b*
    )

    (om::om-make-dialog-item
      'om::pop-up-menu
      (om::om-make-point 350 50)
      (om::om-make-point 80 20)
      "Chord key"
      :range '("C" "C#" "D" "Eb" "E" "F" "F#" "G" "Ab" "A" "Bb" "B")
      :value (chord-key (om::object editor))
      :di-action #'(lambda (m)
        (setq check (nth (om::om-get-selected-item-index m) (om::om-get-item-list m)))
        (if (string= check "None")
          (setf (chord-key (om::object editor)) nil)
          (setf (chord-key (om::object editor)) check)
        )
        (let ((old-diff 0))
          (if (relative-to-same (om::object editor))
              (setq old-diff (diff-chord-key (om::object editor)))
          )
          (change-subblocks-values (om::object editor) :chord-key check)
          (if (relative-to-same (om::object editor))
            (propagate-AB (om::object editor) :diff-chord-key (- old-diff (diff-chord-key (om::object editor))))
          )
        )
      )
    )

    (om::om-make-dialog-item
      'om::om-static-text
      (om::om-make-point 250 100)
      (om::om-make-point 200 20)
      "Chord quality"
      :font om::*om-default-font1b*
    )

    (om::om-make-dialog-item
      'om::pop-up-menu
      (om::om-make-point 350 100)
      (om::om-make-point 80 20)
      "Chord quality"
      :value (chord-quality (om::object editor))
      :range '("Major" "Minor" "Augmented" "Diminished")
      :di-action #'(lambda (m)
        (setq check (nth (om::om-get-selected-item-index m) (om::om-get-item-list m)))
        (if (string= check "None")
          (setf (chord-quality (om::object editor)) nil)
          (setf (chord-quality (om::object editor)) check))
        (change-subblocks-values (om::object editor) :chord-quality check)
        
      )
    )

     (om::om-make-dialog-item
      'om::om-static-text
      (om::om-make-point 250 150)
      (om::om-make-point 200 20)
      "Minimum pitch"
      :font om::*om-default-font1b*
    )

    ;; (om::om-make-dialog-item
    ;;   'om::om-check-box
    ;;   (om::om-make-point 350 150)
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
      (om::om-make-point 250 170)
      (om::om-make-point 150 20)
      "Minimum pitch"
      :range '(1 127)
      :increment 1
      :value (min-pitch (om::object editor))
      :di-action #'(lambda (s)
        (setf (min-pitch (om::object editor)) (om::om-slider-value s))
        (let ((old-diff 0))
          (if (relative-to-same (om::object editor))
              (setq old-diff (diff-min-pitch (om::object editor)))
          )
          (change-subblocks-values (om::object editor) 
                                  :min-pitch-flag (min-pitch-flag (om::object editor)) 
                                  :min-pitch (min-pitch (om::object editor)))
          (if (relative-to-same (om::object editor))
              (propagate-AB (om::object editor) :diff-min-pitch (- old-diff (diff-min-pitch (om::object editor))))
          )
        )
      )
    )

    (om::om-make-dialog-item
      'om::om-static-text
      (om::om-make-point 250 220)
      (om::om-make-point 200 20)
      "Maximum pitch"
      :font om::*om-default-font1b*
    )

    ;; (om::om-make-dialog-item
    ;;   'om::om-check-box
    ;;   (om::om-make-point 350 220)
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
      (om::om-make-point 250 240)
      (om::om-make-point 150 20)
      "Maximum pitch"
      :range '(1 127)
      :increment 1
      :value (max-pitch (om::object editor))
      :di-action #'(lambda (s)
        (setf (max-pitch (om::object editor)) (om::om-slider-value s))
        (let ((old-diff 0))
          (if (relative-to-same (om::object editor))
              (setq old-diff (diff-max-pitch (om::object editor)))
          )
          (change-subblocks-values (om::object editor) 
                                  :max-pitch-flag (max-pitch-flag (om::object editor)) 
                                  :max-pitch (max-pitch (om::object editor)))
          (if (relative-to-same (om::object editor))
              (propagate-AB (om::object editor) :diff-max-pitch (- old-diff (diff-max-pitch (om::object editor))))
          )
        )
      )
    )

    )
    ))
    
    (if (typep (om::object editor) 'mldz::a)
      (setf subviews (append subviews (list
          (om::om-make-dialog-item
            'om::om-static-text
            (om::om-make-point 10 10)
            (om::om-make-point 200 20)
            "Similarity with first A block"
            :font om::*om-default-font1b*
          )
          (om::om-make-dialog-item
            'om::slider
            (om::om-make-point 10 40)
            (om::om-make-point 150 20)
            "Similarity with first A block"
            :range '(1 100)
            :increment 1
            :value (similarity-percent-A0 (om::object editor))
            :di-action #'(lambda (s)
              (setf (similarity-percent-A0 (om::object editor)) (om::om-slider-value s))
              (print "similarity-percent-A0: ")
              (print (similarity-percent-A0 (om::object editor)))
            )
          )
      )))
      (setf subviews (append subviews (list
          (om::om-make-dialog-item
            'om::om-static-text
            (om::om-make-point 10 10)
            (om::om-make-point 200 20)
            "Similarity with first B block"
            :font om::*om-default-font1b*
          )
          (om::om-make-dialog-item
            'om::slider
            (om::om-make-point 10 40)
            (om::om-make-point 150 20)
            "Similarity with first B block"
            :range '(1 100)
            :increment 1
            :value (similarity-percent-B0 (om::object editor))
            :di-action #'(lambda (s)
              (setf (similarity-percent-B0 (om::object editor)) (om::om-slider-value s))
              (print "similarity-percent-B0: ")
              (print (similarity-percent-B0 (om::object editor)))
            )
          )
      )))
    )

    (loop :for x :in subviews :do
      (om::om-add-subviews
          panel
          x
      )
    )
  )
)
