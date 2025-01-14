(in-package :mldz)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                         ROCK CLASS                          ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Define a rock object containing the constraints
;; and attributes necessary for the search
(om::defclass! rock ()
    (
      (block-list 
          :accessor block-list :initarg :block-list :initform nil 
          :documentation "Block list containing the global musical structure")
      (melody-source-A  
          :accessor melody-source-A :initarg :melody-source-A :initform nil 
          :documentation "Source melody for s of the first A block")
      (melody-source-B  
          :accessor melody-source-B :initarg :melody-source-B :initform nil 
          :documentation "Source melody for s of the first B block")
      (bar-length 
          :accessor bar-length :initform 0 :type integer
           :documentation "Number of bars contained in the block")
      (nb-a 
          :accessor nb-a  :initform 0 :type integer 
          :documentation "number of block A in the structure")
      (nb-b 
          :accessor nb-b  :initform 0 :type integer 
          :documentation "number of block B in the structure")
      (idx-first-a  
          :accessor idx-first-a :initform 0 :type integer 
          :documentation "index of the first block A in the structure")
      (idx-first-b  
          :accessor idx-first-b  :initform 0 :type integer 
          :documentation "index of the first block B in the structure")
      (min-note-length-flag 
          :accessor min-note-length-flag :initform nil :type integer
          :documentation "Flag stating if the note-min-length constrain must be posted")
      (min-note-length  
          :accessor min-note-length :initform 1 :type integer
          :documentation "Minimum note length value")
      (max-note-length-flag 
          :accessor max-note-length-flag :initform nil :type integer
          :documentation "Flag stating if the note-max-length constrain must be posted")
      (max-note-length  
          :accessor max-note-length :initform 16 :type integer
          :documentation "Maximum note length value")
      (chord-key  
          :accessor chord-key :initform "C" :type string
          :documentation "Chord key to set the scale in")
      (chord-quality  
          :accessor chord-quality :initform "Major" :type string
          :documentation "Quality to set the scale in")
      (min-pitch  
          :accessor min-pitch :initform 1 :type integer
          :documentation "Minimum pitch value")
      (max-pitch 
          :accessor max-pitch :initform 127 :type integer
          :documentation "Maximum pitch value")
      (solution 
          :accessor solution :initarg :solution :initform nil 
          :documentation "The current solution of the CSP in the form of a voice object.")
      (result :accessor result
          :result :initform (list) 
          :documentation "A list holder to store the result of the call to the CSPs")
      (stop-search 
          :accessor stop-search :stop-search :initform nil 
          :documentation "booleanto tell if the user wishes to stop the search or not.")
      (input-rhythm 
          :accessor input-rhythm :input-rhythm :initform (make-instance 'voice) 
          :documentation "The rhythm of the melody or a melody in the form of a voice object. ")
      (tempo 
          :accessor tempo :initform 80 :type integer 
          :documentation "The tempo (BPM) of the project")
      (branching 
          :accessor branching :initform "Top down" :type string 
          :documentation "The tempo (BPM) of the project")
      (percent-diff 
          :accessor percent-diff :initform 1 :type integer
          :documentation "The minimum difference percentage between solutions")
    )
)


(defclass rock-editor (om::editorview) ())

(defmethod om::class-has-editor-p ((self rock)) t)
(defmethod om::get-editor-class ((self rock)) 'rock-editor)
(defmethod om::om-draw-contents ((view rock-editor))
  (let* ((object (om::object view)))
    (om::om-with-focused-view
      view
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
        :size (om::om-make-point 130 200)
        :position (om::om-make-point 5 5)
        :bg-color om::*azulito*)
      )
      (constraints-panel (om::om-make-view 'om::om-view
        :size (om::om-make-point 510 200)
        :position (om::om-make-point 5 210)
        :bg-color om::*azulito*)
      )
      (structure-panel (om::om-make-view 'om::om-view
        :size (om::om-make-point 100 200)
        :position (om::om-make-point 140 5)
        :bg-color om::*azulito*)
      )
      (search-panel (om::om-make-view 'om::om-view
        :size (om::om-make-point 270 200)
        :position (om::om-make-point 245 5)
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
;;                        ROCK PANEL                           ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun make-rock-panel (editor rock-panel)
  (om::om-add-subviews
    rock-panel

    ;; Button to add a block A at the end of the current block-list
    (om::om-make-dialog-item
        'om::om-button
        (om::om-make-point 5 10) ; position (horizontal, vertical)
        (om::om-make-point 100 20) ; size (horizontal, vertical)
        "Add A"
        :di-action #'(lambda (b)
          (print "Added A to structure")
          ;;Create the block and set its values
          (let ((bar-length 0) (new-block (make-instance 'A :parent (om::object editor) (om::object editor))))
            (setf (block-position new-block) (length (block-list (om::object editor))))
            (setf (block-position-A new-block) (count-A-block-list (block-list (parent new-block))))
            (setf (block-list (om::object editor)) (append (block-list (om::object editor)) (list new-block)))
            (if (= (length (block-list (om::object editor))) 1)
              (setq bar-length 0)
              (setq bar-length (bar-length (first (block-list (om::object editor)))))
            )
            (if (= (nb-a (om::object editor)) 0)
              (setf (idx-first-a (om::object editor)) (block-position new-block))
            )
            (setf (nb-a (om::object editor)) (+ (nb-a (om::object editor)) 1))
            (setf (bar-length (om::object editor)) (+ bar-length (bar-length (om::object editor))))
            ;; Update the constraints values based on the Rock block
            (change-subblocks-values (om::object editor) 
                                      :bar-length (bar-length (om::object editor))
                                      :chord-key (chord-key (om::object editor))
                                      :min-pitch (min-pitch (om::object editor))
                                      :max-pitch (max-pitch (om::object editor))
                                      :min-note-length-flag (min-note-length-flag (om::object editor))
                                      :min-note-length (min-note-length (om::object editor))
                                      :max-note-length-flag (max-note-length-flag (om::object editor))
                                      :max-note-length (max-note-length (om::object editor))
                                      :chord-quality (chord-quality (om::object editor))
            )
          )
          ;; (om::om-remove-subviews rock-panel)
          (make-my-interface editor)
        )
    )

    
    ;; Button to add a block B at the end of the current block-list
    (om::om-make-dialog-item
        'om::om-button
        (om::om-make-point 5 50) ; position (horizontal, vertical)
        (om::om-make-point 100 20) ; size (horizontal, vertical)
        "Add B"
        :di-action #'(lambda (b)
          (print "Added B to structure")
          ;;Create the block and set its values
          (let ((bar-length 0) (new-block (make-instance 'B :parent (om::object editor) (om::object editor))))
            (setf (block-position new-block) (length (block-list (om::object editor))))
            (setf (block-position-B new-block) (count-B-block-list (block-list (parent new-block))))
            (setf (block-list (om::object editor)) (append (block-list (om::object editor)) (list new-block)))
            (if (= (length (block-list (om::object editor))) 1)
              (setq bar-length 0)
              (setq bar-length (bar-length (first (block-list (om::object editor)))))
            )
            (if (= (nb-b (om::object editor)) 0)
              (setf (idx-first-b (om::object editor)) (block-position new-block))
            )
            (setf (nb-b (om::object editor)) (+ (nb-b (om::object editor)) 1))
            (setf (bar-length (om::object editor)) (+ bar-length (bar-length (om::object editor))))
            ;; Update the constraints values based on the Rock block
            (change-subblocks-values (om::object editor) 
                                      :bar-length (bar-length (om::object editor))
                                      :chord-key (chord-key (om::object editor))
                                      :min-pitch (min-pitch (om::object editor))
                                      :max-pitch (max-pitch (om::object editor))
                                      :min-note-length-flag (min-note-length-flag (om::object editor))
                                      :min-note-length (min-note-length (om::object editor))
                                      :max-note-length-flag (max-note-length-flag (om::object editor))
                                      :max-note-length (max-note-length (om::object editor))
                                      :chord-quality (chord-quality (om::object editor))
            )
          )
          ;; (om::om-remove-subviews rock-panel)
          (make-my-interface editor)
        )
    )
    
    ;; Buton to erase every bit of the current structure
    (om::om-make-dialog-item
        'om::om-button
        (om::om-make-point 5 90) ; position (horizontal, vertical)
        (om::om-make-point 100 20) ; size (horizontal, vertical)
        "Clear"
        :di-action #'(lambda (b)
          (print "Cleared structure")
          (mp:process-run-function ; start a new thread for the execution of the next method
            "clear struct" ; name of the thread, not necessary but useful for debugging
            nil ; process initialization keywords, not needed here
            (lambda () ; function to call
              (setf (bar-length (om::object editor)) 0)
              (setf (block-list (om::object editor)) nil)
              (setf (nb-a (om::object editor)) 0)
              (setf (nb-b (om::object editor)) 0)
              (om::om-remove-subviews rock-panel)
              (make-my-interface editor)
            )
          )
        )
    )
  )
)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                      STRUCTURE PANEL                        ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun make-structure-panel (editor structure-panel)

  (let ((loop-index 0) (subview-list '()))
  ;; Loop on the block-list and create buttons for every block of the structure
  ;; that open the corresponding editor
  (loop for x in (block-list (om::object editor))
    do 
      (if (typep x 'mldz::a)
        (setf subview-list (append subview-list (list (om::om-make-dialog-item
          'om::om-button
          (om::om-make-point 5 (+ 5 (* 30 loop-index))) ; position (horizontal, vertical)
          (om::om-make-point 75 20) ; size (horizontal, vertical)
          "A"
          :di-action #'(lambda (b)

            (print "Selected A")
            (mp:process-run-function ; start a new thread for the execution of the next method
              "next thread" ; name of the thread, not necessary but useful for debugging
              nil ; process initialization keywords, not needed here
              #'(lambda () ; function to call
                (om::openeditorframe ; open a window displaying the editor of the A block
                  (om::omNG-make-new-instance (nth (position b subview-list) 
                  (block-list (om::object editor))) 
                  (concatenate 'string "Window A" (write-to-string (position b subview-list))))
                )
              )
            )
          )
        ))))
      )

      (if (typep x 'mldz::b)
        (setf subview-list (append subview-list (list (om::om-make-dialog-item
          'om::om-button
          (om::om-make-point 5 (+ 5 (* 30 loop-index))) ; position (horizontal, vertical)
          (om::om-make-point 75 20) ; size (horizontal, vertical)
          "B"
          :di-action #'(lambda (b)
            (print "Selected B")
            (mp:process-run-function ; start a new thread for the execution of the next method
              "next thread" ; name of the thread, not necessary but useful for debugging
              nil ; process initialization keywords, not needed here
              #'(lambda () ; function to call
                (om::openeditorframe ; open a window displaying the editor of the B block
                  (om::omNG-make-new-instance (nth (position b subview-list) 
                  (block-list (om::object editor))) 
                  (concatenate 'string "Window B" (write-to-string (position b subview-list))))
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                     CONSTRAINTS PANEL                       ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun make-constraints-panel (editor panel)
  (om::om-add-subviews
    panel
    (om::om-make-dialog-item
      'om::om-static-text
      (om::om-make-point 15 5)
      (om::om-make-point 120 20)
      "Block constraints"
      :font om::*om-default-font1b*
    )

    (om::om-make-dialog-item
      'om::om-static-text
      (om::om-make-point 15 30)
      (om::om-make-point 100 20)
      "Number of bars"
      :font om::*om-default-font1b*
    )

    (om::om-make-dialog-item
      'om::pop-up-menu
      (om::om-make-point 150 30)
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
      (om::om-make-point 15 60)
      (om::om-make-point 100 20)
      "Min note length"
      :font om::*om-default-font1b*
    )

    (om::om-make-dialog-item
      'om::om-check-box
      (om::om-make-point 120 60)
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
      (om::om-make-point 150 60)
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
      (om::om-make-point 15 90)
      (om::om-make-point 100 20)
      "Max note length"
      :font om::*om-default-font1b*
    )

    (om::om-make-dialog-item
      'om::om-check-box
      (om::om-make-point 120 90)
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
      (om::om-make-point 150 90)
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
      (om::om-make-point 250 5)
      (om::om-make-point 200 20)
      "Pitch constraints"
      :font om::*om-default-font1b*
    )

    (om::om-make-dialog-item
      'om::om-static-text
      (om::om-make-point 250 30)
      (om::om-make-point 100 20)
      "Chord key"
      :font om::*om-default-font1b*
    )

    (om::om-make-dialog-item
      'om::pop-up-menu
      (om::om-make-point 350 30)
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
        
        (change-subblocks-values (om::object editor) :chord-key check)
      )
    )

    (om::om-make-dialog-item
      'om::om-static-text
      (om::om-make-point 250 60)
      (om::om-make-point 100 20)
      "Chord quality"
      :font om::*om-default-font1b*
    )

    (om::om-make-dialog-item
      'om::pop-up-menu
      (om::om-make-point 350 60)
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
      (om::om-make-point 250 90)
      (om::om-make-point 100 20)
      "Minimum pitch"
      :font om::*om-default-font1b*
    )


    (om::om-make-dialog-item
      'om::slider
      (om::om-make-point 250 110)
      (om::om-make-point 150 20)
      "Minimum pitch"
      :range '(1 127)
      :increment 1
      :value (min-pitch (om::object editor))
      :di-action #'(lambda (s)
        (setf (min-pitch (om::object editor)) (om::om-slider-value s))
        (change-subblocks-values (om::object editor) 
                                  :min-pitch (min-pitch (om::object editor)))
      )
    )

    (om::om-make-dialog-item
      'om::om-static-text
      (om::om-make-point 250 140)
      (om::om-make-point 100 20)
      "Maximum pitch"
      :font om::*om-default-font1b*
    )

    (om::om-make-dialog-item
      'om::slider
      (om::om-make-point 250 160)
      (om::om-make-point 150 20)
      "Maximum pitch"
      :range '(1 127)
      :increment 1
      :value (max-pitch (om::object editor))
      :di-action #'(lambda (s)
        (setf (max-pitch (om::object editor)) (om::om-slider-value s))
        (change-subblocks-values (om::object editor) 
                                  :max-pitch (max-pitch (om::object editor)))
      )
    )
  )

)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                        SEARCH PANEL                         ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun make-rock-search-panel (editor search-panel)
  (om::om-add-subviews
    search-panel
    (om::om-make-dialog-item
      'om::om-static-text
      (om::om-make-point 75 5)
      (om::om-make-point 120 20)
      "Search Parameters"
      :font om::*om-default-font1b*
    )

    (om::om-make-dialog-item
      'om::om-button
      (om::om-make-point 5 30) ; position (horizontal, vertical)
      (om::om-make-point 80 20) ; size (horizontal, vertical)
      "Start"
      :di-action #'(lambda (b)
          (setf (result (om::object editor)) 
                (rock-solver (om::object editor) 
                            (percent-diff (om::object editor)) 
                            (branching (om::object editor))))
      )
    )

    (om::om-make-dialog-item
      'om::om-button
      (om::om-make-point 90 30) ; position
      (om::om-make-point 80 20) ; size
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
        (om::om-make-point 175 30) ; position (horizontal, vertical)
        (om::om-make-point 80 20) ; size (horizontal, vertical)
        "Stop"
        :di-action #'(lambda (b)
          (setf (stop-search (om::object editor)) t)
        )
    )

    (om::om-make-dialog-item
      'om::om-static-text
      (om::om-make-point 15 75)
      (om::om-make-point 100 20)
      "Tempo (BPM)"
      :font om::*om-default-font1b*
    )

    (om::om-make-dialog-item
      'om::pop-up-menu
      (om::om-make-point 170 75)
      (om::om-make-point 80 20)
      "Tempo"
      :range (loop :for n :from 30 :upto 200 :collect (number-to-string n))
      :value (number-to-string (tempo (om::object editor)))
      :di-action #'(lambda (m)
        (setf (tempo (om::object editor)) (string-to-number (nth (om::om-get-selected-item-index m) (om::om-get-item-list m))))
      )
    )


    (om::om-make-dialog-item
      'om::om-static-text
      (om::om-make-point 15 105)
      (om::om-make-point 200 20)
      "Difference Percentage"
      :font om::*om-default-font1b*
    )

    (om::om-make-dialog-item
      'om::slider
      (om::om-make-point 15 130)
      (om::om-make-point 230 20)
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