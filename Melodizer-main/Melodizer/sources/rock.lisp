(in-package :mldz)

(om::defclass! rock ()
    ((block-list :accessor block-list :initarg :block-list :initform nil :documentation "")
    )
)

;; (om::defclass! ablock ()
;;     ((block-list :accessor block-list :initarg :block-list :initform nil :documentation "")
;;     )
;; )

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
        :size (om::om-make-point 600 150)
        :position (om::om-make-point 5 5)
        :bg-color om::*azulito*)
      )
    )

    (setf elements-rock-panel (make-rock-panel self rock-panel))

    ; add the subviews for the different parts into the main view
    (om::om-add-subviews
      self
      rock-panel
    )
  )
  ; return the editor
  self
)

    ;;;;;;;;;;;;;;;;;
    ;;; main view ;;;
    ;;;;;;;;;;;;;;;;;

; this function creates the elements for the main panel
(defun make-main-view (editor)
  ; background colour
  (om::om-set-bg-color editor om::*om-light-gray-color*) ;pour changer le bg color. om peut fabriquer sa propre couleur: (om-make-color r g b)
)

(defun make-rock-panel (editor rock-panel)
  (om::om-add-subviews
    rock-panel
    (om::om-make-dialog-item
        'om::om-button
        (om::om-make-point 5 10) ; position (horizontal, vertical)
        (om::om-make-point 100 50) ; size (horizontal, vertical)
        "A"
        :di-action #'(lambda (b)
        
          (print "Selected A1")

          (mp:process-run-function ; start a new thread for the execution of the next method
            "next thread" ; name of the thread, not necessary but useful for debugging
            nil ; process initialization keywords, not needed here
            (lambda () ; function to call
            
              (om::openeditorframe ;
                ;; opens a voice window
                ;; (om::omNG-make-new-instance (make-instance 'voice) "Window A1")
                (om::omNG-make-new-instance (make-instance 'pop-up-menu) "Window A1")
              )
            )
          )
        )
    )


    (om::om-make-dialog-item
        'om::om-button
        (om::om-make-point 115 10) ; position (horizontal, vertical)
        (om::om-make-point 100 50) ; size (horizontal, vertical)
        "A"
        :di-action #'(lambda (b)
        
          (print "Selected A2")

          (mp:process-run-function ; start a new thread for the execution of the next method
            "next thread" ; name of the thread, not necessary but useful for debugging
            nil ; process initialization keywords, not needed here
            (lambda () ; function to call
            
              (om::openeditorframe ;
                ;; opens a voice window
                (om::omNG-make-new-instance (make-instance 'pop-up-menu) "Window A2")
                ;; (om::omNG-make-new-instance (make-instance 'text-box) "Window B")
                ;; (om::omNG-make-new-instance (make-instance 'temporalbox) "Window B")

              )
            )
          )
        )
    )
    (om::om-make-dialog-item
        'om::om-button
        (om::om-make-point 225 10) ; position (horizontal, vertical)
        (om::om-make-point 100 50) ; size (horizontal, vertical)
        "B"
        :di-action #'(lambda (b)
        
          (print "Selected B")

          (mp:process-run-function ; start a new thread for the execution of the next method
            "next thread" ; name of the thread, not necessary but useful for debugging
            nil ; process initialization keywords, not needed here
            (lambda () ; function to call
            
              (om::openeditorframe ;
                ;; opens a voice window
                (om::omNG-make-new-instance (make-instance 'pop-up-menu) "Window B")
                ;; (om::omNG-make-new-instance (make-instance 'text-box) "Window B")
                ;; (om::omNG-make-new-instance (make-instance 'temporalbox) "Window B")

              )
            )
          )
        )
    )
    (om::om-make-dialog-item
        'om::om-button
        (om::om-make-point 335 10) ; position (horizontal, vertical)
        (om::om-make-point 100 50) ; size (horizontal, vertical)
        "A"
        :di-action #'(lambda (b)
        
          (print "Selected A3")

          (mp:process-run-function ; start a new thread for the execution of the next method
            "next thread" ; name of the thread, not necessary but useful for debugging
            nil ; process initialization keywords, not needed here
            (lambda () ; function to call
            
              (om::openeditorframe ;
                ;; opens a voice window
                (om::omNG-make-new-instance (make-instance 'pop-up-menu) "Window A3")
                ;; (om::omNG-make-new-instance (make-instance 'text-box) "Window B")
                ;; (om::omNG-make-new-instance (make-instance 'temporalbox) "Window B")

              )
            )
          )
        )
    )
  )
)