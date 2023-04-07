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
      (melody-source :accessor melody-source :initarg :melody-source :initform nil :documentation "")
      (bar-length :accessor bar-length :initform 0 :type integer)
      (min-pushed-notes :accessor min-pushed-notes :initform 0 :type integer)
      (max-pushed-notes :accessor max-pushed-notes :initform 10 :type integer)
      (min-notes :accessor min-notes :initform nil :type integer)
      (max-notes :accessor max-notes :initform nil :type integer)
      (min-note-length-flag :accessor min-note-length-flag :initform nil :type integer)
      (min-note-length :accessor min-note-length :initform 0 :type integer)
      (max-note-length-flag :accessor max-note-length-flag :initform nil :type integer)
      (max-note-length :accessor max-note-length :initform 16 :type integer)
      (quantification :accessor quantification :initform nil :type string)
      (key-selection :accessor key-selection :initform nil :type string)
      (mode-selection :accessor mode-selection :initform nil :type string)
      (chord-key :accessor chord-key :initform nil :type string)
      (chord-quality :accessor chord-quality :initform nil :type string)
      (min-pitch :accessor min-pitch :initform 1 :type integer)
      (min-pitch-flag :accessor min-pitch-flag :initform nil :type integer)
      (max-pitch :accessor max-pitch :initform 127 :type integer)
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
        :size (om::om-make-point 810 150)
        :position (om::om-make-point 5 5)
        :bg-color om::*azulito*)
      )
       (constraints-panel (om::om-make-view 'om::om-view
        :size (om::om-make-point 810 500)
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
      (s-block :accessor s-block :initarg :s-block :initform (make-instance 's) :documentation "")
      (r-block :accessor r-block :initarg :r-block :initform (make-instance 'r) :documentation "")
      (d-block :accessor d-block :initarg :d-block :initform (make-instance 'd) :documentation "")
      (c-block :accessor c-block :initarg :c-block :initform (make-instance 'c) :documentation "")
      (parent :accessor parent :initarg :parent :initform nil :documentation "")
      (melody-source :accessor melody-source :initarg :melody-source :initform nil :documentation "")
      (bar-length :accessor bar-length :initform 0 :type integer)
      (min-pushed-notes :accessor min-pushed-notes :initform 0 :type integer)
      (max-pushed-notes :accessor max-pushed-notes :initform 10 :type integer)
      (min-notes :accessor min-notes :initform nil :type integer)
      (max-notes :accessor max-notes :initform nil :type integer)
      (min-note-length-flag :accessor min-note-length-flag :initform nil :type integer)
      (min-note-length :accessor min-note-length :initform 0 :type integer)
      (max-note-length-flag :accessor max-note-length-flag :initform nil :type integer)
      (max-note-length :accessor max-note-length :initform 16 :type integer)
      (quantification :accessor quantification :initform nil :type string)
      (key-selection :accessor key-selection :initform nil :type string)
      (mode-selection :accessor mode-selection :initform nil :type string)
      (chord-key :accessor chord-key :initform nil :type string)
      (chord-quality :accessor chord-quality :initform nil :type string)
      (min-pitch :accessor min-pitch :initform 1 :type integer)
      (min-pitch-flag :accessor min-pitch-flag :initform nil :type integer)
      (max-pitch :accessor max-pitch :initform 127 :type integer)
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
        :size (om::om-make-point 810 150)
        :position (om::om-make-point 5 5)
        :bg-color om::*azulito*)
      )
       (constraints-panel (om::om-make-view 'om::om-view
        :size (om::om-make-point 810 500)
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
                (setf (parent (s-block (om::object editor))) editor)
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
            (setf (parent (r-block (om::object editor))) editor)
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
                (setf (parent (d-block (om::object editor))) editor)
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
                (setf (parent (c-block (om::object editor))) editor)
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
