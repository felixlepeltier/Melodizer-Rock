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
                                    :min-simultaneous-notes min-simultaneous-notes
                                    :max-simultaneous-notes max-simultaneous-notes
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

(defun ceil-to-exp (val)
    (cond
        ((<= val 1) 1)
        ((<= val 2) 2)
        ((<= val 4) 4)
        ((<= val 8) 8)
        ((<= val 16) 16)
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


(defun count-A-block-list (block-list)
    (let ((count 0))
        (dolist (n block-list)
            (if (typep n 'mldz::a)
                (setq count (+ count 1))
            )
        )
        count
    )
)

(defun count-B-block-list (block-list)
    (let ((count 0))
        (dolist (n block-list)
            (if (typep n 'mldz::b)
                (setq count (+ count 1))
            )
        )
        count
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

; Create push and pull list from a voice object
(defun create-push-pull-int (input-chords quant)
    (let (temp
         (next 0)
         (push (list))
         (pull (list '-1))
        ;; (pull (list))
         (playing (list))
         (tree (om::tree input-chords))
         (pitch (to-pitch-list (om::chords input-chords))))
         (setq tree (second tree))
         (loop :for i :from 0 :below (length tree) :by 1 :do
            (setq temp (read-tree-int (make-list quant :initial-element -1) (make-list quant :initial-element -1) (make-list quant :initial-element -1) (second (nth i tree)) pitch 0 quant next))
            (setq push (append push (first temp)))
            (setq pull (append pull (second temp)))
            (setq playing (append playing (third temp)))
            (setf next (fourth temp))
         )
         (list push pull playing))
)

;; ((4 4) (1 1 1 1))
; <tree> is the rhythm tree to read
; <pitch> is the ordered list of pitch (each element of push is represented by a list with the pitch of notes played on this quant)
; <pos> is the next position in push to add values
; <length> is the current duration of a note to add
; <next> is the index in pitch of the next notes we will add
;recursive function to read a rhythm tree and create push and pull
(defun read-tree-int (push pull playing tree pitch pos length next)
    (print "in read-tree-int")
    (progn
        (setf length (/ length (ceil-to-exp (length tree))))
        (loop :for i :from 0 :below (length tree) :by 1 :do
            (if (typep (nth i tree) 'list)
                (let (temp)
                    (setq temp (read-tree-int push pull playing (second (nth i tree)) pitch pos length next))
                    (setq push (first temp))
                    (setq pull (second temp))
                    (setq playing (third temp))
                    (setf next (fourth temp))
                    (setf pos (fifth temp))
                )
                (progn
                    (setf (nth pos push) (first (nth next pitch)))
                    (loop :for j :from pos :below (+ pos (* length (nth i tree))) :by 1 :do
                        (setf (nth j playing) (first (nth next pitch)))
                    )
                    (setf pos (+ pos (* length (nth i tree))))
                    (setf (nth (- pos 1) pull) (first (nth next pitch)))
                    (setf next (+ next 1))
                )
            )
        )
        (list push pull playing next pos)
    )
)

; Getting a list of chords and a rhythm tree from the playing list of intvar
(defun build-voice-int (sol push pull playing bars quant tempo)
    (let ((p-push (list))
          (p-pull (list))
          (p-playing (list))
          (chords (list))
          (tree (list))
          (ties (list))
          (prev 0)
          )
    (setq p-push (nconc p-push (mapcar (lambda (n) (* 100 (gil::g-values sol n))) push)))
    ;; (print p-push)
    ;; (loop :for i :below (length pull) do (print (gil::g-values sol (nth i pull))))
    (setq p-pull (nconc p-pull (mapcar (lambda (n) (* 100 (gil::g-values sol n))) pull)))
    (setq p-playing (nconc p-playing (mapcar (lambda (n) (* 100 (gil::g-values sol n))) playing)))
    
    (setq count 1)
    ;; (setq rest 0)
    (loop :for b :from 0 :below bars :by 1 :do
        (if (< (nth (* b quant) p-playing) 0)
            (setq rest 1)
            (setq rest 0)
        )
        (setq rhythm (list))
        (loop :for q :from 0 :below quant :by 1 :do
            (setq i (+ (* b quant) q))
            (cond
                ((>= (nth i p-push) 0)
                     ; if rhythm impulse
                     (progn
                        (setq duration 0)
                        (setq j (+ i 1))
                        (loop
                            (if (>= j (length p-pull))
                                (setq duration (* (floor 60000 (* tempo quant)) (- j i)))
                                (return)
                            )
                            (if (>= (nth j p-pull) 0)
                                (if (= (nth j p-pull) (nth i p-push))
                                    (progn
                                        (setq duration (* (floor 60000 (* tempo quant)) (- j i)))
                                        (return)
                                    )
                                )
                            )
                            (incf j)
                        )
                        (setq chord (make-instance 'chord :LMidic (list (nth i p-push)) :Ldur (list duration)))
                        (setq chords (nconc chords (list chord)))
                        (cond
                            ((= rest 1)
                                (progn
                                    (setq rhythm (nconc rhythm (list (* -1 count))))
                                    (setq rest 0)))
                            ((/= q 0)
                                (setq rhythm (nconc rhythm (list count))))
                        )
                        (setq count 1))
                )
                ((and (< (nth i p-playing) 0) (= rest 0))
                    (setq rest 1)
                    (if (> count 0)
                        (setq rhythm (nconc rhythm (list count)))
                    )
                    (setq count 1)
                )
                ; else
                (t (setq count (+ count 1)))
            )
        )
        (if (= rest 1)
            (setq rhythm (nconc rhythm (list (* -1 count))))
            (setq rhythm (nconc rhythm (list count)))
        )
        (setq count 0)
        (setq rhythm (list '(4 4) rhythm))

        (setq tree (nconc tree (list rhythm)))
    )
    (setq tree (list '? tree))
    (list chords tree)
    )
)

; returns the list of intervals defining a given mode
(defun get-scale-chord (mode)
    (cond
        ((string-equal mode "Major")
            (list 2 2 1 2 2 2 1)
        )
        ((string-equal mode "Minor")
            (list 2 1 2 2 1 2 2)
        )
        ((string-equal mode "Diminished")
            (list 2 1 2 1 2 1 2)
        )
        ((string-equal mode "Augmented")
            (list 3 1 3 1 3 1)
        )
    )
)