(in-package :mldz)

(defun change-subblocks-values (rock-block &key bar-length 
                                                chord-key 
                                                min-pitch-flag
                                                min-pitch 
                                                max-pitch-flag
                                                max-pitch 
                                                min-note-length-flag
                                                min-note-length
                                                max-note-length-flag 
                                                max-note-length
                                                min-simultaneous-notes
                                                max-simultaneous-notes
                                                key-selection
                                                mode-selection
                                                chord-quality)

    (if (typep rock-block 'mldz::rock)
        (progn
            (loop :for x in (block-list rock-block) do
                (if bar-length 
                    (progn
                        (setq n-bars (/ bar-length (list-length (block-list rock-block))))
                        (print n-bars)
                        (setf (bar-length x) n-bars)
                    )
                )
                (if chord-key
                    (setf (chord-key x) chord-key)
                )
                (if (or min-pitch-flag min-pitch)
                    (setf (min-pitch-flag x) min-pitch-flag (min-pitch x) min-pitch)
                )
                (if (or max-pitch-flag max-pitch)
                    (setf (max-pitch-flag x) max-pitch-flag (max-pitch x) max-pitch)
                )
                (if (or min-note-length-flag min-note-length)
                    (setf (min-note-length-flag x) min-note-length-flag (min-note-length x) min-note-length)
                )
                (if (or max-note-length-flag max-note-length)
                    (setf (max-note-length-flag x) max-note-length-flag (max-note-length x) max-note-length)
                )
                (if min-simultaneous-notes
                    (setf (min-simultaneous-notes x) min-simultaneous-notes)
                )
                (if max-simultaneous-notes
                    (setf (max-simultaneous-notes x) max-simultaneous-notes)
                )
                (if key-selection
                    (setf (key-selection x) key-selection)
                )
                (if mode-selection
                    (setf (mode-selection x) mode-selection)
                )
                (if chord-quality
                    (setf (chord-quality x) chord-quality)
                )
                (change-subblocks-values x  :bar-length (bar-length x)
                                            :chord-key (chord-key x)
                                            :min-pitch-flag (min-pitch-flag x)
                                            :min-pitch (min-pitch x)
                                            :max-pitch-flag (max-pitch-flag x)
                                            :max-pitch (max-pitch x)
                                            :min-note-length-flag (min-note-length-flag x)
                                            :min-note-length (min-note-length x)
                                            :max-note-length-flag (max-note-length-flag x)
                                            :max-note-length  (max-note-length x)
                                            :min-simultaneous-notes (min-simultaneous-notes x)
                                            :max-simultaneous-notes (max-simultaneous-notes x)
                                            :key-selection (key-selection x)
                                            :mode-selection (mode-selection x)
                                            :chord-quality (chord-quality x)
                )
            )
            
        )
    )
    (if (or (typep rock-block 'mldz::a) (typep rock-block 'mldz::b))
        (progn
            (if bar-length
                (progn
                    (setq n-bars (/ bar-length 4))
                    (setf (bar-length (s-block rock-block)) n-bars)
                    (setf (bar-length (r-block rock-block)) n-bars)
                    (setf (bar-length (d-block rock-block)) n-bars)
                    (setf (bar-length (c-block rock-block)) n-bars)
                )
            )
            (if chord-key
                (progn 
                    (setf (chord-key (s-block rock-block)) chord-key)
                    (setf (chord-key (r-block rock-block)) chord-key)
                    (setf (chord-key (d-block rock-block)) chord-key)
                    (setf (chord-key (c-block rock-block)) chord-key)
                )
            )
            (if (or min-pitch-flag min-pitch)
                (progn
                    (setf   (min-pitch-flag (s-block rock-block)) min-pitch-flag 
                            (min-pitch (s-block rock-block)) min-pitch)
                    (setf   (min-pitch-flag (r-block rock-block)) min-pitch-flag 
                            (min-pitch (r-block rock-block)) min-pitch)
                    (setf   (min-pitch-flag (d-block rock-block)) min-pitch-flag 
                            (min-pitch (d-block rock-block)) min-pitch)
                    (setf   (min-pitch-flag (c-block rock-block)) min-pitch-flag 
                            (min-pitch (c-block rock-block)) min-pitch)
                )
            )
            (if (or max-pitch-flag max-pitch)
                (progn
                    (setf   (max-pitch-flag (s-block rock-block)) max-pitch-flag 
                            (max-pitch (s-block rock-block)) max-pitch)
                    (setf   (max-pitch-flag (r-block rock-block)) max-pitch-flag 
                            (max-pitch (r-block rock-block)) max-pitch)
                    (setf   (max-pitch-flag (d-block rock-block)) max-pitch-flag 
                            (max-pitch (d-block rock-block)) max-pitch)
                    (setf   (max-pitch-flag (c-block rock-block)) max-pitch-flag 
                            (max-pitch (c-block rock-block)) max-pitch)
                )
            )
            (if (or min-note-length-flag min-note-length)
                (progn
                    (setf   (min-note-length-flag (s-block rock-block)) min-note-length-flag 
                            (min-note-length (s-block rock-block)) min-note-length)
                    (setf   (min-note-length-flag (r-block rock-block)) min-note-length-flag 
                            (min-note-length (r-block rock-block)) min-note-length)
                    (setf   (min-note-length-flag (d-block rock-block)) min-note-length-flag 
                            (min-note-length (d-block rock-block)) min-note-length)
                    (setf   (min-note-length-flag (c-block rock-block)) min-note-length-flag 
                            (min-note-length (c-block rock-block)) min-note-length)
                )
            )
            (if (or max-note-length-flag max-note-length)
                (progn
                    (setf   (max-note-length-flag (s-block rock-block)) max-note-length-flag 
                            (max-note-length (s-block rock-block)) max-note-length)
                    (setf   (max-note-length-flag (r-block rock-block)) max-note-length-flag 
                            (max-note-length (r-block rock-block)) max-note-length)
                    (setf   (max-note-length-flag (d-block rock-block)) max-note-length-flag 
                            (max-note-length (d-block rock-block)) max-note-length)
                    (setf   (max-note-length-flag (c-block rock-block)) max-note-length-flag 
                            (max-note-length (c-block rock-block)) max-note-length)
                )
            )
            (if min-simultaneous-notes
                (progn
                    (setf (min-simultaneous-notes (s-block rock-block)) min-simultaneous-notes)
                    (setf (min-simultaneous-notes (r-block rock-block)) min-simultaneous-notes)
                    (setf (min-simultaneous-notes (d-block rock-block)) min-simultaneous-notes)
                    (setf (min-simultaneous-notes (c-block rock-block)) min-simultaneous-notes)
                )
            )
            (if max-simultaneous-notes
                (progn
                    (setf (max-simultaneous-notes (s-block rock-block)) max-simultaneous-notes)
                    (setf (max-simultaneous-notes (r-block rock-block)) max-simultaneous-notes)
                    (setf (max-simultaneous-notes (d-block rock-block)) max-simultaneous-notes)
                    (setf (max-simultaneous-notes (c-block rock-block)) max-simultaneous-notes)
                )
            )
            (if key-selection
                (progn
                    (setf (key-selection (s-block rock-block)) key-selection)
                    (setf (key-selection (r-block rock-block)) key-selection)
                    (setf (key-selection (d-block rock-block)) key-selection)
                    (setf (key-selection (c-block rock-block)) key-selection)
                )
            )
            (if mode-selection
                (progn
                    (setf (mode-selection (s-block rock-block)) mode-selection)
                    (setf (mode-selection (r-block rock-block)) mode-selection)
                    (setf (mode-selection (d-block rock-block)) mode-selection)
                    (setf (mode-selection (c-block rock-block)) mode-selection)
                )
            )
            (if chord-quality
                (progn
                    (setf (chord-quality (s-block rock-block)) chord-quality)
                    (setf (chord-quality (r-block rock-block)) chord-quality)
                    (setf (chord-quality (d-block rock-block)) chord-quality)
                    (setf (chord-quality (c-block rock-block)) chord-quality)
                )
            )
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