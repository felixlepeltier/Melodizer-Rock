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
                        (setf (bar-length x) n-bars)
                    )
                )
                (if chord-key
                    (setf (chord-key x) chord-key)
                )
                (if min-pitch
                    (cond 
                        ((relative-to-rock x)
                            (setf   (min-pitch-flag x) min-pitch-flag 
                                    (min-pitch x) (- min-pitch (diff-min-pitch x)))
                        )
                    )
                    
                )
                (if max-pitch
                    (cond 
                        ((relative-to-rock x)
                            (setf   (max-pitch-flag x) max-pitch-flag 
                                    (max-pitch x) (- max-pitch (diff-max-pitch x)))
                        )
                    )
                )
                (if min-note-length
                    (cond 
                        ((relative-to-rock x)
                        (progn
                            (setf   (min-note-length-flag x) min-note-length-flag 
                                    (min-note-length x) (floor (expt 2 (- (log min-note-length 2) (diff-min-length x)))))
                            (print (min-note-length x)))
                        )
                    )
                )
                (if max-note-length
                    (cond 
                        ((relative-to-rock x)
                            (setf   (max-note-length-flag x) max-note-length-flag 
                                    (max-note-length x) (floor (expt 2 (- (log max-note-length 2) (diff-max-length x))))))
                    )
                )
                (if min-simultaneous-notes
                    (cond 
                        ((relative-to-rock x)
                            (setf (min-simultaneous-notes x) (- min-simultaneous-notes (diff-min-sim x)))
                        )
                    )
                )
                (if max-simultaneous-notes
                    (cond 
                        ((relative-to-rock x)
                            (setf (max-simultaneous-notes x) (- max-simultaneous-notes (diff-max-sim x)))
                        )
                    )
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
                    (setf (diff-min-pitch rock-block) (- (min-pitch (om::object (parent rock-block))) min-pitch))
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
                    (setf (diff-max-pitch rock-block) (- (max-pitch (om::object (parent rock-block))) max-pitch))
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
                    (setf (diff-min-length rock-block) (- (log (min-note-length (om::object (parent rock-block))) 2) (log min-note-length 2)))
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
                    (setf (diff-max-length rock-block) (- (log (max-note-length (om::object (parent rock-block))) 2) (log max-note-length 2)))
                )
            )
            (if min-simultaneous-notes
                (progn
                    (setf (min-simultaneous-notes (s-block rock-block)) min-simultaneous-notes)
                    (setf (min-simultaneous-notes (r-block rock-block)) min-simultaneous-notes)
                    (setf (min-simultaneous-notes (d-block rock-block)) min-simultaneous-notes)
                    (setf (min-simultaneous-notes (c-block rock-block)) min-simultaneous-notes)
                    (setf (diff-min-sim rock-block) (- (min-simultaneous-notes (om::object (parent rock-block))) min-simultaneous-notes))

                )
            )
            (if max-simultaneous-notes
                (progn
                    (setf (max-simultaneous-notes (s-block rock-block)) max-simultaneous-notes)
                    (setf (max-simultaneous-notes (r-block rock-block)) max-simultaneous-notes)
                    (setf (max-simultaneous-notes (d-block rock-block)) max-simultaneous-notes)
                    (setf (max-simultaneous-notes (c-block rock-block)) max-simultaneous-notes)
                    (setf (diff-max-sim rock-block) (- (max-simultaneous-notes (om::object (parent rock-block))) max-simultaneous-notes))
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
            result
        )
    )
)

(defun bar-length-sum-rock (rock)
    (let ((sum 0))
        (loop :for n :from 0 :below (list-length (block-list rock)) :by 1
        do
            (setq sum (+ sum (bar-length (nth n (block-list rock)))))
        )
        sum
    )
)
(defun bar-length-sum-AB (A)
    (+  (bar-length (s-block A)) 
        (bar-length (r-block A)) 
        (bar-length (d-block A)) 
        (bar-length (c-block A)))
)
(defun set-bar-length-up (rock-block)
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

;; each diff argument is the difference between the old diff and new diff of the changed block A or B
;; For example, if a block A goes from diff-max-pitch 5 to diff-max-pitch 3, the argument diff-max-pitch is 2
(defun propagate-AB (AB-block &key  diff-min-sim 
                                    diff-max-sim
                                    diff-min-length
                                    diff-max-length
                                    diff-key-selection
                                    diff-mode-selection
                                    diff-chord-key
                                    diff-chord-quality
                                    diff-min-pitch
                                    diff-max-pitch)
    (let (
        (parent (om::object (parent AB-block)))
        (type-block (type-of AB-block))
        block-list
        )
        (setf block-list (block-list parent))
        (loop :for x in block-list do
            (if (and (not (eq x AB-block)) (relative-to-same x) (typep x type-block))
                (progn
                    (if diff-min-sim
                        (progn
                            (setf   (diff-min-sim x) (- (diff-min-sim x) diff-min-sim))
                            (setf   (min-simultaneous-notes x) (- (min-simultaneous-notes parent) (diff-min-sim x)))
                            (change-subblocks-values x 
                                  :min-simultaneous-notes (min-simultaneous-notes x))
                        )
                    )
                    (if diff-max-sim
                        (progn
                            (setf   (diff-max-sim x) (- (diff-max-sim x) diff-max-sim))
                            (setf   (max-simultaneous-notes x) (- (max-simultaneous-notes parent) (diff-max-sim x)))
                            (change-subblocks-values x 
                                  :max-simultaneous-notes (max-simultaneous-notes x))
                        )
                    )
                    (if diff-min-length
                        (progn
                            (print diff-min-length)
                            (setf   (diff-min-length x) (- (diff-min-length x) diff-min-length))
                            (setf   (min-note-length x) (floor (expt 2 (- (log (min-note-length parent) 2) (diff-min-length x)))))
                            (change-subblocks-values x 
                                    :min-note-length-flag (min-note-length-flag x)
                                    :min-note-length (min-note-length x))
                        )
                    )
                    (if diff-max-length
                        (progn
                            (setf   (diff-max-length x) (- (diff-max-length x) diff-max-length))
                            (setf   (max-note-length x) (floor (expt 2 (- (log (max-note-length parent) 2) (diff-max-length x)))))
                            (change-subblocks-values x 
                                    :max-note-length-flag (max-note-length-flag x)
                                    :max-note-length (max-note-length x))
                        )
                    )
                    (if diff-key-selection
                        (progn
                            (setf   (diff-key-selection x) (- (diff-key-selection x) diff-key-selection))
                            (setf   (key-selection x) (- (key-selection parent) (diff-key-selection x)))
                            (change-subblocks-values x 
                                  :key-selection (key-selection x))
                        )
                    )
                    (if diff-mode-selection
                        (progn
                            (setf   (diff-mode-selection x) (- (diff-mode-selection x) diff-mode-selection))
                            (setf   (mode-selection x) (- (mode-selection parent) (diff-mode-selection x)))
                            (change-subblocks-values x 
                                  :mode-selection (mode-selection x))
                        )
                    )
                    (if diff-chord-key
                        (progn
                            (setf   (diff-chord-key x) (- (diff-chord-key x) diff-chord-key))
                            (setf   (chord-key x) (- (chord-key parent) (diff-chord-key x)))
                            (change-subblocks-values x 
                                  :chord-key (chord-key x))
                        )
                    )
                    (if diff-chord-quality
                        (progn
                            (setf   (diff-chord-quality x) (- (diff-chord-quality x) diff-chord-quality))
                            (setf   (chord-quality x) (- (chord-quality parent) (diff-chord-quality x)))
                            (change-subblocks-values x 
                                  :chord-quality (chord-quality x))
                        )
                    )
                    (if diff-min-pitch
                        (progn
                            (setf   (diff-min-pitch x) (- (diff-min-pitch x) diff-min-pitch))
                            (setf   (min-pitch x) (- (min-pitch parent) (diff-min-pitch x)))
                            (change-subblocks-values x 
                                    :min-pitch-flag (min-pitch-flag x)
                                    :min-pitch (min-pitch x))
                        )
                    )
                    (if diff-max-pitch
                        (progn
                            (setf   (diff-max-pitch x) (- (diff-max-pitch x) diff-max-pitch))
                            (setf   (max-pitch x) (- (max-pitch parent) (diff-max-pitch x)))
                            (change-subblocks-values x 
                                    :max-pitch-flag (max-pitch-flag x)
                                    :max-pitch (max-pitch x))
                        )
                    )
                )
            )
        )
    )
)