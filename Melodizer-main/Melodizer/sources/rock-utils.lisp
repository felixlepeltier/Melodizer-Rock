(in-package :mldz)

(defun change-sublocks-bar-length (rock-block bar-length)
    (if (typep rock-block 'mldz::rock)
        (progn
            (loop :for x in (block-list rock-block) do 
                (setq n-bars (/ bar-length (list-length (block-list rock-block))))
                (setf (bar-length x) n-bars)
                (change-sublocks-bar-length x n-bars)
            )
        )
    )
    (if (or (typep rock-block 'mldz::a) (typep rock-block 'mldz::b))
        (progn
            (setq n-bars (/ bar-length 4))
            (setf (bar-length (s-block rock-block)) n-bars)
            (setf (bar-length (r-block rock-block)) n-bars)
            (setf (bar-length (d-block rock-block)) n-bars)
            (setf (bar-length (c-block rock-block)) n-bars)
        )
    )
)

(defun change-sublocks-chord-key (rock-block chord-key)
    (if (typep rock-block 'mldz::rock)
        (progn
            (loop :for x in (block-list rock-block) do 
                (setf (chord-key x) chord-key)
                (change-sublocks-chord-key x chord-key)
            )
        )
    )
    (if (or (typep rock-block 'mldz::a) (typep rock-block 'mldz::b))
        (progn
            (setf (chord-key (s-block rock-block)) chord-key)
            (setf (chord-key (r-block rock-block)) chord-key)
            (setf (chord-key (d-block rock-block)) chord-key)
            (setf (chord-key (c-block rock-block)) chord-key)
        )
    )
)

(defun change-sublocks-min-pitch (rock-block min-pitch-flag min-pitch)
    (if (typep rock-block 'mldz::rock)
        (progn
            (loop :for x in (block-list rock-block) do 
                (setf (min-pitch-flag x) min-pitch-flag (min-pitch x) min-pitch)
                (change-sublocks-min-pitch x min-pitch-flag min-pitch)
            )
        )
    )
    (if (or (typep rock-block 'mldz::a) (typep rock-block 'mldz::b))
        (progn
            (setf (min-pitch-flag (s-block rock-block)) min-pitch-flag (min-pitch (s-block rock-block)) min-pitch)
            (setf (min-pitch-flag (r-block rock-block)) min-pitch-flag (min-pitch (r-block rock-block)) min-pitch)
            (setf (min-pitch-flag (d-block rock-block)) min-pitch-flag (min-pitch (d-block rock-block)) min-pitch)
            (setf (min-pitch-flag (c-block rock-block)) min-pitch-flag (min-pitch (c-block rock-block)) min-pitch)
        )
    )
)

(defun change-sublocks-max-pitch (rock-block max-pitch-flag max-pitch)
    (if (typep rock-block 'mldz::rock)
        (progn
            (loop :for x in (block-list rock-block) do 
                (setf (max-pitch-flag x) max-pitch-flag (max-pitch x) max-pitch)
                (change-sublocks-max-pitch x max-pitch-flag max-pitch)
            )
        )
    )
    (if (or (typep rock-block 'mldz::a) (typep rock-block 'mldz::b))
        (progn
            (setf (max-pitch-flag (s-block rock-block)) max-pitch-flag (max-pitch (s-block rock-block)) max-pitch)
            (setf (max-pitch-flag (r-block rock-block)) max-pitch-flag (max-pitch (r-block rock-block)) max-pitch)
            (setf (max-pitch-flag (d-block rock-block)) max-pitch-flag (max-pitch (d-block rock-block)) max-pitch)
            (setf (max-pitch-flag (c-block rock-block)) max-pitch-flag (max-pitch (c-block rock-block)) max-pitch)
        )
    )
)


(defun change-sublocks-min-note-length (rock-block min-note-length-flag min-note-length)
    (if (typep rock-block 'mldz::rock)
        (progn
            (loop :for x in (block-list rock-block) do 
                (setf (min-note-length-flag x) min-note-length-flag (min-note-length x) min-note-length)
                (change-sublocks-min-note-length x min-note-length-flag min-note-length)
            )
        )
    )
    (if (or (typep rock-block 'mldz::a) (typep rock-block 'mldz::b))
        (progn
            (setf (min-note-length-flag (s-block rock-block)) min-note-length-flag (min-note-length (s-block rock-block)) min-note-length)
            (setf (min-note-length-flag (r-block rock-block)) min-note-length-flag (min-note-length (r-block rock-block)) min-note-length)
            (setf (min-note-length-flag (d-block rock-block)) min-note-length-flag (min-note-length (d-block rock-block)) min-note-length)
            (setf (min-note-length-flag (c-block rock-block)) min-note-length-flag (min-note-length (c-block rock-block)) min-note-length)
        )
    )
)

(defun change-sublocks-max-note-length (rock-block max-note-length-flag max-note-length)
    (if (typep rock-block 'mldz::rock)
        (progn
            (loop :for x in (block-list rock-block) do 
                (setf (max-note-length-flag x) max-note-length-flag (max-note-length x) max-note-length)
                (change-sublocks-max-note-length x max-note-length-flag max-note-length)
            )
        )
    )
    (if (or (typep rock-block 'mldz::a) (typep rock-block 'mldz::b))
        (progn
            (setf (max-note-length-flag (s-block rock-block)) max-note-length-flag (max-note-length (s-block rock-block)) max-note-length)
            (setf (max-note-length-flag (r-block rock-block)) max-note-length-flag (max-note-length (r-block rock-block)) max-note-length)
            (setf (max-note-length-flag (d-block rock-block)) max-note-length-flag (max-note-length (d-block rock-block)) max-note-length)
            (setf (max-note-length-flag (c-block rock-block)) max-note-length-flag (max-note-length (c-block rock-block)) max-note-length)
        )
    )
)

(defun change-sublocks-max-pushed-notes (rock-block max-pushed-notes)
    (if (typep rock-block 'mldz::rock)
        (progn
            (loop :for x in (block-list rock-block) do 
                (setf (max-pushed-notes x) max-pushed-notes)
                (change-sublocks-chord-key x max-pushed-notes)
            )
        )
    )
    (if (or (typep rock-block 'mldz::a) (typep rock-block 'mldz::b))
        (progn
            (setf (max-pushed-notes (s-block rock-block)) max-pushed-notes)
            (setf (max-pushed-notes (r-block rock-block)) max-pushed-notes)
            (setf (max-pushed-notes (d-block rock-block)) max-pushed-notes)
            (setf (max-pushed-notes (c-block rock-block)) max-pushed-notes)
        )
    )
)

(defun change-sublocks-min-pushed-notes (rock-block min-pushed-notes)
    (if (typep rock-block 'mldz::rock)
        (progn
            (loop :for x in (block-list rock-block) do 
                (setf (min-pushed-notes x) min-pushed-notes)
                (change-sublocks-chord-key x min-pushed-notes)
            )
        )
    )
    (if (or (typep rock-block 'mldz::a) (typep rock-block 'mldz::b))
        (progn
            (setf (min-pushed-notes (s-block rock-block)) min-pushed-notes)
            (setf (min-pushed-notes (r-block rock-block)) min-pushed-notes)
            (setf (min-pushed-notes (d-block rock-block)) min-pushed-notes)
            (setf (min-pushed-notes (c-block rock-block)) min-pushed-notes)
        )
    )
)
(defun bar-length-range (rock-block)
    (if (or (typep rock-block 'mldz::s)
            (typep rock-block 'mldz::r)
            (typep rock-block 'mldz::d)
            (typep rock-block 'mldz::c))
        (loop   :for n 
                :from 0  
                :below 5
                :by 1 
                :collect (number-to-string n))
    
        (let ((sum (bar-length rock-block))(result (list))) 
            (if (typep rock-block 'mldz::rock)
                (if (= sum 0)
                    (if (block-list rock-block) 
                        (progn
                            (setq n-block (list-length (block-list rock-block)))
                            (setq result (append '("0") (loop :for n 
                                                    :from (* 4 n-block)  
                                                    :below (+ (* 16 n-block) 1)
                                                    :by (* 4 n-block) 
                                                    :collect (number-to-string n))))
                        )
                        (setf result '("0"))
                    )
                    (setq result (list (number-to-string sum)))
                )
                ;;(setq result (list (number-to-string (bar-length rock-block))))
            )
            (if (or (typep rock-block 'mldz::a) (typep rock-block 'mldz::b))
                (if (= sum 0)
                    (setq result (append (loop :for n 
                                                :from 0   
                                                :below 17
                                                :by 4 
                                                :collect (number-to-string n))))
                    
                    (setq result (list (number-to-string sum)))
                )
                ;;(setq result (list (number-to-string (bar-length rock-block))))
            )
            (print "bar-length-range")
            (print result)
            result
        )
    )
)

(defun bar-length-sum-rock (rock)
    (print "set-bar-length-sum rock")
    (let ((sum 0))
        (loop :for n :from 0 :below (list-length (block-list rock)) :by 1
        do
            (setq sum (+ sum (bar-length (nth n (block-list rock)))))
        )
        sum
    )
)
(defun bar-length-sum-AB (A)
    (print "set-bar-length-sum A")
    (+  (bar-length (s-block A)) 
        (bar-length (r-block A)) 
        (bar-length (d-block A)) 
        (bar-length (c-block A)))
)
(defun set-bar-length-up (rock-block)
    (print "set-bar-length-up")
    (print (om::object (parent rock-block)))
    (if (or (typep (om::object (parent rock-block)) 'mldz::a) (typep (om::object (parent rock-block)) 'mldz::b))
        (setf (bar-length (om::object (parent rock-block))) (bar-length-sum-AB (om::object (parent rock-block))))
        (setf (bar-length (om::object (parent rock-block))) (bar-length-sum-rock (om::object (parent rock-block))))
    )
    (make-my-interface (parent rock-block))
    (if (not (typep (om::object (parent rock-block)) 'mldz::rock))
        (set-bar-length-up (om::object (parent rock-block)))
    )
)

(defun propagate-bar-length-srdc (rock-block)
    (let ((parent (om::object (parent rock-block))) (nbars (bar-length rock-block)))
        (if (or (typep parent 'mldz::a) (typep parent 'mldz::b))
            (progn
                (setf (bar-length (s-block parent)) nbars)
                (setf (bar-length (r-block parent)) nbars)
                (setf (bar-length (d-block parent)) nbars)
                (setf (bar-length (c-block parent)) nbars)
            )
        )
    )
)

;; http://www.lee-mac.com/sublist.html
;; Sublst  -  Lee Mac
;; The list analog of the substr function
;; lst - [lst] List from which sublist is to be returned
;; idx - [int] Zero-based index at which to start the sublist
;; len - [int] Length of the sublist or nil to return all items following idx

(defun sublst (lst idx len)
    (cond
        (   (null lst) nil)
        (   (< 0  idx) (sublst (cdr lst) (1- idx) len))
        (   (null len) lst)
        (   (< 0  len) (cons (car lst) (sublst (cdr lst) idx (1- len))))
    )
)