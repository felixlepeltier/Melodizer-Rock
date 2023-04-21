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
      (relative-to-same :accessor relative-to-same :initarg :relative-to-same :initform 1 :type integer)
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
        :size (om::om-make-point 810 50)
        :position (om::om-make-point 5 5)
        :bg-color om::*azulito*)
      )
      (changes-panel (om::om-make-view 'om::om-view
        :size (om::om-make-point 810 100)
        :position (om::om-make-point 5 60)
        :bg-color om::*azulito*)
      )
      (constraints-panel (om::om-make-view 'om::om-view
        :size (om::om-make-point 810 500)
        :position (om::om-make-point 5 165)
        :bg-color om::*azulito*)
      )
      
    )

    (setf elements-A-panel (make-A-panel self A-panel))
    (setf elements-constraints-panel (make-constraints-AB-panel self constraints-panel))
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
      (relative-to-same :accessor relative-to-same :initarg :relative-to-same :initform 1 :type integer)
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
        :size (om::om-make-point 810 50)
        :position (om::om-make-point 5 5)
        :bg-color om::*azulito*)
      )
      (changes-panel (om::om-make-view 'om::om-view
        :size (om::om-make-point 810 100)
        :position (om::om-make-point 5 60)
        :bg-color om::*azulito*)
      )
      (constraints-panel (om::om-make-view 'om::om-view
        :size (om::om-make-point 810 500)
        :position (om::om-make-point 5 170)
        :bg-color om::*azulito*)
      )
      
    )

    (setf elements-B-panel (make-B-panel self B-panel))
    (setf elements-constraints-panel (make-constraints-AB-panel self constraints-panel))
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
        (om::om-make-point 100 50) ; size (horizontal, vertical)
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
        (om::om-make-point 100 50) ; size (horizontal, vertical)
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
        (om::om-make-point 100 50) ; size (horizontal, vertical)
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

(defun make-changes-panel (editor panel)
  (om::om-add-subviews
    panel
    (om::om-make-dialog-item
      'om::om-static-text
      (om::om-make-point 10 10)
      (om::om-make-point 200 20)
      "Types of changes"
      :font om::*om-default-font1b*
    )
    
    (om::om-make-dialog-item
      'om::om-check-box
      (om::om-make-point 10 30)
      (om::om-make-point 200 20)
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
      (om::om-make-point 200 20)
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
        (let ((old-diff 0))
          (setq check (nth (om::om-get-selected-item-index m) (om::om-get-item-list m)))
          (if (relative-to-same (om::object editor))
              (setq old-diff (diff-min-sim (om::object editor)))
          )
          (setf (min-simultaneous-notes (om::object editor)) (string-to-number check))
          (change-subblocks-values (om::object editor) 
                                  :min-simultaneous-notes (min-simultaneous-notes (om::object editor)))
          (if (relative-to-same (om::object editor))
              (propagate-AB (om::object editor) :diff-min-sim (- old-diff (diff-min-sim (om::object editor))))
          )
        )
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
        (let ((old-diff 0))
          (setq check (nth (om::om-get-selected-item-index m) (om::om-get-item-list m)))
          (if (relative-to-same (om::object editor))
              (setq old-diff (diff-max-sim (om::object editor)))
          )
          (setf (max-simultaneous-notes (om::object editor)) (string-to-number check))
          (change-subblocks-values (om::object editor) 
                                  :max-simultaneous-notes (max-simultaneous-notes (om::object editor)))
          (if (relative-to-same (om::object editor))
              (propagate-AB (om::object editor) :diff-max-sim (- old-diff (diff-max-sim (om::object editor))))
          )
        )
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
        (let ((old-diff 0))
          (if (relative-to-same (om::object editor))
              (setq old-diff (diff-key-selection (om::object editor)))
          )
          (change-subblocks-values (om::object editor) :key-selection check)
          (if (relative-to-same (om::object editor))
            (propagate-AB (om::object editor) :diff-key-selection (- old-diff (diff-key-selection (om::object editor))))
          )
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
        (change-subblocks-values (om::object editor) :mode-selection check)
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
        (change-subblocks-values (om::object editor) :chord-quality check)
        
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
