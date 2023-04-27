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
                                                chord-quality)
    (let (block-list)
    
    ;; Setup the sub-block list for the loop
    (cond 
        ((typep rock-block 'mldz::rock) (setq block-list (block-list rock-block)))
        ((or (typep rock-block 'mldz::a) (typep rock-block 'mldz::b)) 
            (setq block-list (list   (s-block rock-block)
                                (r-block rock-block) 
                                (d-block rock-block)
                                (c-block rock-block)))
        )
        ((or (typep rock-block 'mldz::s) (typep rock-block 'mldz::r)
            (typep rock-block 'mldz::d) (typep rock-block 'mldz::c)) 
            (setq block-list (list (accomp rock-block)))
        )
    )

    ;; Update the diff parameter for this block
    (if (not (typep rock-block 'mldz::rock))
        (progn

            ;; Pitch constraints
            (if (and chord-key (chord-key (parent rock-block)))
                (setf   (diff-chord-key rock-block) 
                        (-  (name-to-note-value (chord-key (parent rock-block))) 
                            (name-to-note-value chord-key)))
            )
            (if (and (or min-pitch-flag min-pitch) (min-pitch (parent rock-block)))
                (setf   (diff-min-pitch rock-block) 
                        (- (min-pitch (parent rock-block)) 
                            min-pitch))
            )
            (if (and (or max-pitch-flag max-pitch) (max-pitch (parent rock-block)))
                (setf   (diff-max-pitch rock-block) 
                        (- (max-pitch (parent rock-block)) 
                            max-pitch))
                
            )

            ;;Other constraints
            (if (not (typep rock-block 'mldz::accompaniment))
                (progn 
                    (if (and (or min-note-length-flag min-note-length) (min-note-length (parent rock-block)))
                        (setf   (diff-min-length rock-block) 
                                (-  (log (min-note-length (parent rock-block)) 2) 
                                    (log min-note-length 2)))
                        
                    )
                    (if (and (or max-note-length-flag max-note-length) (max-note-length (parent rock-block)))
                        (setf   (diff-max-length rock-block) 
                                (-  (log (max-note-length (parent rock-block)) 2) 
                                    (log max-note-length 2)))
                    )
                    (if (and min-simultaneous-notes (min-simultaneous-notes (parent rock-block)))
                        (setf   (diff-min-sim rock-block) 
                                (- (min-simultaneous-notes (parent rock-block)) 
                                    min-simultaneous-notes))
                    )
                    (if (and max-simultaneous-notes (max-simultaneous-notes (parent rock-block)) )
                        (setf   (diff-max-sim rock-block) 
                                (- (max-simultaneous-notes (parent rock-block)) 
                                    max-simultaneous-notes))
                    )
                )
            )

        )
    )

    ;; Loop on sub-blocks to update their values
    (loop :for x in block-list do
        (setf (parent x) rock-block)
        (if bar-length 
            (progn
                (setq n-bars (/ bar-length (list-length block-list)))
                (setf (bar-length x) n-bars)
            )
        )

        ;;Pitch constraints
        (if chord-key
            (cond 
                ((relative-to-parent x)
                    (setf (chord-key x) (note-value-to-name (- (name-to-note-value chord-key) (diff-chord-key x))))
                )
            )
        )
        (if min-pitch
            (cond 
                ((relative-to-parent x)
                    (setf   (min-pitch-flag x) min-pitch-flag 
                            (min-pitch x) (- min-pitch (diff-min-pitch x)))
                )
            )
            
        )
        (if max-pitch
            (cond 
                ((relative-to-parent x)
                    (setf   (max-pitch-flag x) max-pitch-flag 
                            (max-pitch x) (- max-pitch (diff-max-pitch x)))
                )
            )
        )
        (if chord-quality
            (setf (chord-quality x) chord-quality)
        )

        ;; Other constraints
        (if (not (typep x 'mldz::accompaniment))
            (progn 
                (if min-note-length
                    (cond 
                        ((relative-to-parent x)
                        (progn
                            (setf   (min-note-length-flag x) min-note-length-flag 
                                    (min-note-length x) (floor (expt 2 (- (log min-note-length 2) (diff-min-length x)))))
                        )
                        )
                    )
                )
                (if max-note-length
                    (cond 
                        ((relative-to-parent x)
                            (setf   (max-note-length-flag x) max-note-length-flag 
                                    (max-note-length x) (floor (expt 2 (- (log max-note-length 2) (diff-max-length x))))))
                    )
                )
                (if min-simultaneous-notes
                    (cond 
                        ((relative-to-parent x)
                            (setf (min-simultaneous-notes x) (- min-simultaneous-notes (diff-min-sim x)))
                        )
                    )
                )
                (if max-simultaneous-notes
                    (cond 
                        ((relative-to-parent x)
                            (setf (max-simultaneous-notes x) (- max-simultaneous-notes (diff-max-sim x)))
                        )
                    )
                )
            )
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
                                    :chord-quality (chord-quality x)
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
    (if (or (typep (parent rock-block) 'mldz::a) (typep (parent rock-block) 'mldz::b))
        (setf (bar-length (parent rock-block)) (bar-length-sum-AB (parent rock-block)))
        (setf (bar-length (parent rock-block)) (bar-length-sum-rock (parent rock-block)))
    )
    ;; (make-my-interface (parent rock-block))
    (if (not (typep (parent rock-block) 'mldz::rock))
        (set-bar-length-up (parent rock-block))
    )
)

(defun propagate-bar-length-srdc (rock-block)
    (let ((parent (parent rock-block)) (nbars (bar-length rock-block)))
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
                                    diff-chord-key
                                    diff-chord-quality
                                    diff-min-pitch
                                    diff-max-pitch)
    (let (
        (parent (parent AB-block))
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
                    (if diff-chord-key
                        (progn
                            (setf   (diff-chord-key x) (- (diff-chord-key x) diff-chord-key))
                            (setf   (chord-key x) (note-value-to-name (- (name-to-note-value (chord-key parent)) (diff-chord-key x))))
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