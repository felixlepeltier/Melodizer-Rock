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
    (bar-length :accessor bar-length :initform 0 :type integer)
    (min-simultaneous-notes :accessor min-simultaneous-notes :initform 0 :type integer)
    (max-simultaneous-notes :accessor max-simultaneous-notes :initform 10 :type integer)
    (min-notes :accessor min-notes :initform nil :type integer)
    (max-notes :accessor max-notes :initform nil :type integer)
    (min-note-length-flag :accessor min-note-length-flag :initform nil :type integer)
    (min-note-length :accessor min-note-length :initform 1 :type integer)
    (max-note-length-flag :accessor max-note-length-flag :initform nil :type integer)
    (max-note-length :accessor max-note-length :initform 16 :type integer)
    ;; (quantification :accessor quantification :initform nil :type string)
    (key-selection :accessor key-selection :initform nil :type string)
    (mode-selection :accessor mode-selection :initform nil :type string)
    (chord-key :accessor chord-key :initform nil :type string)
    (chord-quality :accessor chord-quality :initform nil :type string)
    (min-pitch :accessor min-pitch :initform 1 :type integer)
    (min-pitch-flag :accessor min-pitch-flag :initform nil :type integer)
    (max-pitch :accessor max-pitch :initform 127 :type integer)
    (max-pitch-flag :accessor max-pitch-flag :initform nil :type integer)
    (solution :accessor solution :initarg :solution :initform nil :documentation "The current solution of the CSP in the form of a voice object.")
    (result :accessor result
      :result :initform (list) :documentation
      "A temporary list holder to store the result of the call to the CSPs, shouldn't be touched.")
    (stop-search :accessor stop-search :stop-search :initform nil :documentation
      "A boolean variable to tell if the user wishes to stop the search or not.")
    (input-rhythm :accessor input-rhythm :input-rhythm :initform (make-instance 'voice) :documentation
      "The rhythm of the melody or a melody in the form of a voice object. ")
    (tempo :accessor tempo :initform 80 :type integer :documentation
      "The tempo (BPM) of the project")
    (branching :accessor branching :initform "Top down" :type string :documentation
      "The tempo (BPM) of the project")
    (percent-diff :accessor percent-diff :initform 1 :type integer)
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
          
          (let ((bar-length 0) (new-block (make-instance 'A :parent (om::object editor) (om::object editor))))
            (setf (block-list (om::object editor)) (append (block-list (om::object editor)) (list new-block)))
            (if (= (length (block-list (om::object editor))) 1)
              (setq bar-length 0)
              (setq bar-length (bar-length (first (block-list (om::object editor)))))
            )
            (setf (bar-length (om::object editor)) (+ bar-length (bar-length (om::object editor))))
            (change-subblocks-values (om::object editor) 
                                      :bar-length (bar-length (om::object editor))
                                      :chord-key (chord-key (om::object editor))
                                      :min-pitch-flag (min-pitch-flag (om::object editor))
                                      :min-pitch (min-pitch (om::object editor))
                                      :max-pitch-flag (max-pitch-flag (om::object editor))
                                      :max-pitch (max-pitch (om::object editor))
                                      :min-note-length-flag (min-note-length-flag (om::object editor))
                                      :min-note-length (min-note-length (om::object editor))
                                      :max-note-length-flag (max-note-length-flag (om::object editor))
                                      :max-note-length (max-note-length (om::object editor))
                                      :min-simultaneous-notes (min-simultaneous-notes (om::object editor))
                                      :max-simultaneous-notes (max-simultaneous-notes (om::object editor))
                                      :key-selection (key-selection (om::object editor))
                                      :mode-selection (mode-selection (om::object editor))
                                      :chord-quality (chord-quality (om::object editor))
            )
          )
          (om::om-remove-subviews rock-panel)
          (make-my-interface editor)
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
          (let ((bar-length 0))
            (if (= (length (block-list (om::object editor))) 1)
              (setq bar-length 0)
              (setq bar-length (bar-length (first (block-list (om::object editor)))))
            )
            (setf (bar-length (om::object editor)) (+ bar-length (bar-length (om::object editor))))
            (change-subblocks-values (om::object editor) 
                                      :bar-length (bar-length (om::object editor))
                                      :chord-key (chord-key (om::object editor))
                                      :min-pitch-flag (min-pitch-flag (om::object editor))
                                      :min-pitch (min-pitch (om::object editor))
                                      :max-pitch-flag (max-pitch-flag (om::object editor))
                                      :max-pitch (max-pitch (om::object editor))
                                      :min-note-length-flag (min-note-length-flag (om::object editor))
                                      :min-note-length (min-note-length (om::object editor))
                                      :max-note-length-flag (max-note-length-flag (om::object editor))
                                      :max-note-length (max-note-length (om::object editor))
                                      :min-simultaneous-notes (min-simultaneous-notes (om::object editor))
                                      :max-simultaneous-notes (max-simultaneous-notes (om::object editor))
                                      :key-selection (key-selection (om::object editor))
                                      :mode-selection (mode-selection (om::object editor))
                                      :chord-quality (chord-quality (om::object editor))
            )
          )
          (om::om-remove-subviews rock-panel)
          (make-my-interface editor)
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
            "clear struct" ; name of the thread, not necessary but useful for debugging
            nil ; process initialization keywords, not needed here
            (lambda () ; function to call
              (setf (bar-length (om::object editor)) 0)
              (setf (block-list (om::object editor)) nil)
              ;; (om::om-remove-subviews rock-panel)
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

  (let ((loop-index 0) (subview-list '()))
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
    
  )
  )
)


(defun make-constraints-panel (editor panel)
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
        (if (not (typep (om::object editor) 'mldz::rock))
          (progn
            (propagate-bar-length-srdc (om::object editor))
            (set-bar-length-up (om::object editor))
          )
        )
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
        (change-subblocks-values (om::object editor) :key-selection check)
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
          ;; (setq init (new-melodizer (block-csp (om::object editor)) (percent-diff (om::object editor)) (branching (om::object editor))))
          (setf (result (om::object editor)) 
                (rock-solver (om::object editor) 
                            (percent-diff (om::object editor)) 
                            (branching (om::object editor))))
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
            (let ((res (new-rock-next (result (om::object editor)) (om::object editor))))
              (setf (solution (om::object editor)) (first res) (result (om::object editor)) (cdr res))
              (om::openeditorframe ; open a voice window displaying the solution
                (om::omNG-make-new-instance (solution (om::object editor)) "current solution")
              )
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
      :range (loop :for n :from 30 :upto 200 :collect (number-to-string n))
      :value (number-to-string (tempo (om::object editor)))
      :di-action #'(lambda (m)
        (setf (tempo (om::object editor)) (string-to-number (nth (om::om-get-selected-item-index m) (om::om-get-item-list m))))
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
      :value (percent-diff (om::object editor))
      :di-action #'(lambda (s)
        (setf (percent-diff (om::object editor)) (om::om-slider-value s))
      )
    )
  )

)

