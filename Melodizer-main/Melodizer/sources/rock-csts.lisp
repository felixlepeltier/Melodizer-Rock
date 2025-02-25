(in-package :mldz)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Link arrays of music representation ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Post the constraints to link the three arrays of the representation when using SetVar
(defun link-push-pull-playing-set (sp push pull playing max-pitch max-simultaneous-notes)
    ;initial constraint on pull, push, playing and durations
    (gil::g-empty sp (first pull)) ; pull[0] == empty
    (gil::g-rel sp (first push) gil::SRT_EQ (first playing)) ; push[0] == playing [0]

    ;connect push, pull and playing
    (loop :for j :from 1 :below (length push) :do ;for each interval
        (let (temp z c)
            (setq temp (gil::add-set-var sp 0 max-pitch 0 max-simultaneous-notes)); temporary variables
            (gil::g-op sp (nth (- j 1) playing) gil::SOT_MINUS (nth j pull) temp); temp[0] = playing[j-1] - pull[j]
            (gil::g-op sp temp gil::SOT_UNION (nth j push) (nth j playing)); playing[j] == playing[j-1] - pull[j] + push[j] Playing note
            (gil::g-rel sp (nth j pull) gil::SRT_SUB (nth (- j 1) playing)) ; pull[j] <= playing[j-1] cannot pull a note not playing
            (gil::g-set-op sp (nth (- j 1) playing) gil::SOT_MINUS (nth j pull) gil::SRT_DISJ (nth j push)); push[j] || playing[j-1] - pull[j] Cannot push a note still playing
        )
    )
)

;; Post the constraints to link the three arrays of the representation when using IntVar
(defun link-push-pull-playing-int (sp push pull playing max-pitch)
    ;initial constraint on pull, push, playing and durations
    (gil::g-rel sp (first pull) gil::IRT_EQ -1) ; pull[0] == empty
    (gil::g-rel sp (first push) gil::IRT_EQ (first playing)) ; push[0] == playing [0]

    (loop :for j :from 16 :below (length push) :by 16 :do
        (gil::g-rel sp (nth j pull) gil::IRT_EQ (nth (- j 1) playing))
    )

    ;connect push, pull and playing
    (loop :for j :from 1 :below (length push) :do ;for each interval
        (let (
            playing-j-playing-j-one
            push-j-pull-j
            push-j-playing-j
            pull-j-playing-j-one
            pull-j-one
            push-j-one
            push-j-nq-one
            playing-j-one
            )
            (setq 
                playing-j-playing-j-one (gil::add-bool-var-expr sp (nth j playing) gil::IRT_EQ (nth (- j 1) playing))
                push-j-pull-j (gil::add-bool-var-expr sp (nth j push) gil::IRT_EQ (nth j pull))
                push-j-playing-j (gil::add-bool-var-expr sp (nth j push) gil::IRT_EQ (nth j playing))
                pull-j-playing-j-one (gil::add-bool-var-expr sp (nth j pull) gil::IRT_EQ (nth (- j 1) playing))
                pull-j-one (gil::add-bool-var-expr sp (nth j pull) gil::IRT_EQ -1)
                push-j-one (gil::add-bool-var-expr sp (nth j push) gil::IRT_EQ -1)
                push-j-nq-one (gil::add-bool-var-expr sp (nth j push) gil::IRT_NQ -1)
                playing-j-one (gil::add-bool-var-expr sp (nth j playing) gil::IRT_EQ -1)
            )

            ;; playing[j] can only be equal to the preceding played note or a new pushed note
            ;; playing[j] = playing[j-1] || playing[j] = push[j]
            (gil::g-op sp playing-j-playing-j-one gil::BOT_OR push-j-playing-j 1)
            ;; push[j] can only equal the current note playing or -1
            ;; push[j] = playing[j] || push[j] = -1
            (gil::g-op sp push-j-playing-j gil::BOT_OR push-j-one 1)
            ;; A note can be pulled only if it was previously playing
            ;; pull[j] = playing[j-1] || pull[j] = -1
            (gil::g-op sp pull-j-playing-j-one gil::BOT_OR pull-j-one 1)
            ;; A note can be pushed only if the previous playing note was pulled
            ;; push[j] /= -1 => pull[j] = playing[j-1]
            (gil::g-op sp push-j-nq-one gil::BOT_IMP pull-j-playing-j-one 1)
            ;; No note playing implies no note pushed and previous note pulled
            ;; playing[j] = -1 => push[j] = -1 && pull[j] = playing[j-1]
            (gil::g-op sp playing-j-one gil::BOT_IMP push-j-one 1)
            (gil::g-op sp playing-j-one gil::BOT_IMP pull-j-playing-j-one 1)
            ;; Same note playing implies the note to either have been pushed and pulled
            ;; at the same time, or neither pushed or pulled
            ;; push[j] = pull[j] <=> playing[j] = playing[j-1]
            (gil::g-op sp playing-j-playing-j-one gil::BOT_IMP push-j-pull-j 1)
            (gil::g-op sp push-j-pull-j gil::BOT_IMP playing-j-playing-j-one 1)
        )
    )
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Constrain Blocks and their sub-blocks ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Call the right function to constrain the block by their type
(defun constrain-srdc-from-parent (srdc-parent push pull playing push-acc pull-acc playing-acc push-A0 push-B0 quant max-pitch sp)
    (if (typep srdc-parent 'mldz::a)
        ;; The block is of type A, constrain it as such
        (constrain-srdc-from-A srdc-parent push pull playing push-acc pull-acc playing-acc push-A0 quant max-pitch sp)
        ;; The block is of type B, constrain it as such
        (constrain-srdc-from-B srdc-parent push pull playing push-acc pull-acc playing-acc push-B0 quant max-pitch sp)
    )
)


;; Split the three arrays for the sub-blocks then call the block-specific constraints
(defun constrain-srdc-from-AB (A-block push pull playing push-acc pull-acc playing-acc post-constraints quant max-pitch sp)
    (print "constrain-srdc-from-AB")
    
    (if (> (bar-length (s-block A-block)) 0)
        ;; bars*quant elements in each subblock and starts at startidx
        ;; for the sub arrays of push pull playing
        (let ((bars (bar-length (s-block A-block)))
            (s-block (s-block A-block))
            (r-block (r-block A-block))
            (d-block (d-block A-block))
            (c-block (c-block A-block))
            notes-in-subblock 
            startidx-s startidx-r startidx-d startidx-c
            temp-push-s temp-pull-s temp-playing-s
            temp-push-s-acc temp-pull-s-acc temp-playing-s-acc
            temp-push-r temp-pull-r temp-playing-r 
            temp-push-r-acc temp-pull-r-acc temp-playing-r-acc 
            temp-push-d temp-pull-d temp-playing-d
            temp-push-d-acc temp-pull-d-acc temp-playing-d-acc 
            temp-push-c temp-pull-c temp-playing-c
            temp-push-c-acc temp-pull-c-acc temp-playing-c-acc
            )


            ;; notes in each sub block (s/r/d/c)
            (setq notes-in-subblock (* bars quant))
            ;; sectioning the array into the respective parts for s r d c

            ;; access push pull playing arrays for the section related to s
            ;; (sublst x y z) creates a list based on list x from index y and of z sequential elements
            (setq startidx-s 0)
            (setq temp-push-s (sublst push startidx-s notes-in-subblock))
            (setq temp-pull-s (sublst pull startidx-s notes-in-subblock))
            (setq temp-playing-s (sublst playing startidx-s notes-in-subblock))
            (setq temp-push-s-acc (sublst push-acc startidx-s notes-in-subblock))
            (setq temp-pull-s-acc (sublst pull-acc startidx-s notes-in-subblock))
            (setq temp-playing-s-acc (sublst playing-acc startidx-s notes-in-subblock))

            ;; access push pull playing arrays for the section related to r
            (setq startidx-r notes-in-subblock)
            (setq temp-push-r (sublst push startidx-r notes-in-subblock))
            (setq temp-pull-r (sublst pull startidx-r notes-in-subblock))
            (setq temp-playing-r (sublst playing startidx-r notes-in-subblock))
            (setq temp-push-r-acc (sublst push-acc startidx-r notes-in-subblock))
            (setq temp-pull-r-acc (sublst pull-acc startidx-r notes-in-subblock))
            (setq temp-playing-r-acc (sublst playing-acc startidx-r notes-in-subblock))

            ;; access push pull playing arrays for the section related to d
            (setq startidx-d (+ startidx-r notes-in-subblock))
            (setq temp-push-d (sublst push startidx-d notes-in-subblock))
            (setq temp-pull-d (sublst pull startidx-d notes-in-subblock))
            (setq temp-playing-d (sublst playing startidx-d notes-in-subblock))
            (setq temp-push-d-acc (sublst push-acc startidx-d notes-in-subblock))
            (setq temp-pull-d-acc (sublst pull-acc startidx-d notes-in-subblock))
            (setq temp-playing-d-acc (sublst playing-acc startidx-d notes-in-subblock))
            (gil::g-rel sp (nth 0 temp-pull-d) gil::IRT_EQ (nth (- startidx-d 1) playing)) ; pull[0]=playing[previous]

            ;; access push pull playing arrays for the section related to c
            (setq startidx-c (+ startidx-d notes-in-subblock))
            (setq temp-push-c (sublst push startidx-c notes-in-subblock))
            (setq temp-pull-c (sublst pull startidx-c notes-in-subblock))
            (setq temp-playing-c (sublst playing startidx-c notes-in-subblock))
            (setq temp-push-c-acc (sublst push-acc startidx-c notes-in-subblock))
            (setq temp-pull-c-acc (sublst pull-acc startidx-c notes-in-subblock))
            (setq temp-playing-c-acc (sublst playing-acc startidx-c notes-in-subblock))
            (gil::g-rel sp (nth startidx-c pull) gil::IRT_EQ (nth (- startidx-c 1) playing)) ; pull[0]=playing[previous]

            ;; set constraints on these arrays from the values saved in the slots of s-block 
            ;; s
            (print "constraining s")
            (constrain-s sp s-block A-block temp-push-s temp-pull-s temp-playing-s
                                            temp-push-s-acc temp-pull-s-acc temp-playing-s-acc 
                                            max-pitch post-constraints)

            ;; r
            (print "constraining r")
            (constrain-r sp r-block A-block temp-push-r temp-pull-r temp-playing-r  
                                            temp-push-r-acc temp-pull-r-acc temp-playing-r-acc
                                            temp-push-s temp-pull-s temp-playing-s
                                            max-pitch post-constraints)
        
            ;; d
            (print "constraining d")
            (constrain-d sp d-block A-block temp-push-d temp-pull-d temp-playing-d
                                            temp-push-d-acc temp-pull-d-acc temp-playing-d-acc
                                            temp-push-s temp-pull-s temp-playing-s
                                            max-pitch post-constraints)

            ;; c
            (print "constraining c")
            (constrain-c sp c-block A-block temp-push-c temp-pull-c temp-playing-c
                                            temp-push-c-acc temp-pull-c-acc temp-playing-c-acc
                                            max-pitch post-constraints)

        )
    )

)

;; Constrain the A blocks with the resemblance if they are not the first A
(defun constrain-srdc-from-A (A-block push pull playing push-acc pull-acc playing-acc push-A0 quant max-pitch sp)
    (print "constrain-srdc-from-A")
    (let ((post-constraints t) (sim (similarity-percent-A0 A-block)))

    ;; If the block is not the first one of its type, the resemblance must be set with the first
    (if (not (= (block-position-A A-block) 0))
        (let (temp-push)
            (setq temp-push (transpose-chords-key sp (chord-key (nth (idx-first-a (parent A-block)) (block-list (parent A-block)))) 
                                                (chord-quality (nth (idx-first-a (parent A-block)) (block-list (parent A-block))))
                                                (chord-key A-block) (chord-quality A-block) push-A0))
            (cst-common-vars sp temp-push push sim)
            ;; if it has 100% resemblance with the first A, posting constraints on melody might create conflicts
            (if (= sim 100)
                (setq post-constraints nil)
                (setq post-constraints t)
            )
        )
    )
    
    ;; A and B behave the same way, the only distinction is done 
    ;; with the resemblance beween blocks of the same type
    ;; so the same function can be called for the sub-blocks
    (constrain-srdc-from-AB A-block push pull playing push-acc pull-acc playing-acc post-constraints quant max-pitch sp)
    )
)

;; Constrain the B blocks with the resemblance if they are not the first B
(defun constrain-srdc-from-B (B-block push pull playing push-acc pull-acc playing-acc push-B0 quant max-pitch sp)
    (print "constrain-srdc-from-B")
    (let ((post-constraints t) (sim (similarity-percent-B0 B-block)))
    
    (if (not (= (block-position-B B-block) 0))
        (let (temp-push)
            (setq temp-push (transpose-chords-key sp (chord-key (nth (idx-first-b (parent B-block)) (block-list (parent B-block)))) 
                                                (chord-quality (nth (idx-first-b (parent B-block)) (block-list (parent B-block))))
                                                (chord-key B-block) (chord-quality B-block) push-B0))
            (cst-common-vars sp temp-push push sim)
            ;; if it has 100% resemblance with the first A, posting constraints on melody might create conflicts
            (if (= sim 100)
                (setq post-constraints nil)
                (setq post-constraints t)
            )
        )
    )

    ;; A and B behave the same way, the only distinction is done 
    ;; with the resemblance beween blocks of the same type
    ;; so the same function can be called for the sub-blocks
    (constrain-srdc-from-AB B-block push pull playing push-acc pull-acc playing-acc post-constraints quant max-pitch sp)
    )
)

;; for now these constrain-srdc functions take the parent block as argument in case it comes in handy 
;; when we implement more constraints which could be specified through slots of the parent block
(defun constrain-s (sp s-block s-parent push pull playing push-acc pull-acc playing-acc max-pitch post-constraints)
    
    ;; if  (/= melody-source nil) and (block-position-A == 0)
    (let ((melody-A (melody-source-A (parent s-parent)))
        (melody-B (melody-source-B (parent s-parent)))
        (first-A (= (block-position-A s-parent) 0))
        (first-B (= (block-position-B s-parent) 0))
        set-A set-B
        )
        (setq set-A (and first-A melody-A))
        (setq set-B (and first-B melody-B))

        (if (or set-A set-B)
            ;; if in a block that needs to have it's melody set to a source
            (if set-A
                ;; set-A
                (let (push-source pull-source playing-source ppp-source)
                    (setq ppp-source (create-push-pull-int (melody-source-A (parent s-parent)) 16))

                    (setq push-source (first ppp-source))
                    (setq pull-source (second ppp-source))
                    (setq playing-source (third ppp-source))

                    (loop :for i :from 0 :below (length push-source) :by 1 :do
                        (gil::g-rel sp (nth i push) gil::IRT_EQ (nth i push-source))
                    )
                    (loop :for i :from 1 :below (- (length pull-source) 1) :by 1 :do
                        (gil::g-rel sp (nth i pull) gil::IRT_EQ (nth i pull-source))
                    )
                    (loop :for i :from 0 :below (length playing-source) :by 1 :do
                        (gil::g-rel sp (nth i playing) gil::IRT_EQ (nth i playing-source))
                    )

                    (print "First A block's s has been set to the source melody")
                    (if (< (length push-source) (length push))
                        (post-rock-constraints sp s-block   (sublst push (length push-source) (- (length push) (length push-source)))
                                                            (sublst pull (length push-source) (- (length push) (length push-source)))
                                                            (sublst playing (length push-source) (- (length push) (length push-source)))
                                                            nil t)
                    )
                )
                ;; set-B
                (let (push-source pull-source playing-source ppp-source)
                    (setq ppp-source (create-push-pull-int (melody-source-B (parent s-parent)) 16))
                    
                    (setq push-source (first ppp-source))
                    (setq pull-source (second ppp-source))
                    (setq playing-source (third ppp-source))

                    (loop :for i :from 0 :below (length push-source) :by 1 :do
                        (gil::g-rel sp (nth i push) gil::IRT_EQ (nth i push-source))
                    )
                    (loop :for i :from 1 :below (- (length pull-source) 1) :by 1 :do
                        (gil::g-rel sp (nth i pull) gil::IRT_EQ (nth i pull-source))
                    )
                    (loop :for i :from 0 :below (length playing-source) :by 1 :do
                        (gil::g-rel sp (nth i playing) gil::IRT_EQ (nth i playing-source))
                    )
                    (if (< (length push-source) (length push))
                        (post-rock-constraints sp s-block   (sublst push (length push-source) (- (length push) (length push-source)))
                                                            (sublst pull (length push-source) (- (length push) (length push-source)))
                                                            (sublst playing (length push-source) (- (length push) (length push-source)))
                                                            nil t)
                    )
                    (print "First B block's s has been set to the source melody")
                )
            )
            ;; neither set-A nor set-B =>
            ;; don't need to set a source melody, constrain as it should normally do
            (post-rock-constraints sp s-block push pull playing nil post-constraints)
        )
        ;; ;; accompaniment should always be constrained
        (post-rock-constraints sp (accomp s-block) push-acc pull-acc playing-acc nil t)
    )

)


;; Constrain the r block based on its resemblance with the s-block
(defun constrain-r (sp r-block r-parent push pull playing push-acc pull-acc playing-acc
                                        push-s pull-s playing-s max-pitch post-constraints)

    (gil::g-rel sp (first pull) gil::IRT_EQ (nth (- (length playing-s) 1) playing-s)) ; pull[0]=playing-s[quant-1]

    ;; post optional constraints defined in the rock csp
    ;; dont constrain if source melody is given or the similarity with the s block is 100%
    (let (melody)
        (if (typep r-parent 'mldz::a)
            (setq melody (melody-source-A (parent r-parent)))
            (setq melody (melody-source-B (parent r-parent)))
        )
        (post-rock-constraints sp r-block push pull playing nil (and post-constraints (or (not melody) (< (similarity-percent-s r-block) 100))))
    )

    
    (post-rock-constraints sp (accomp r-block) push-acc pull-acc playing-acc nil t)

    ;; constrain r such that it has a similarity of (similarity-percent-s r-block) with notes played in s-block
    ;; transposed the number of semitones asked of the r-block
    (let ((sim (similarity-percent-s r-block)) 
            temp-push  temp-playing      
        )
        (setq temp-push (transpose-chords-semitones sp (chord-key (s-block r-parent)) (chord-quality (s-block r-parent))
                                        (semitones r-block) push-s))  
        (cst-common-vars sp temp-push push sim)
    )
)

; Constrain the d-block based on its resemblance with the s-bloc
(defun constrain-d (sp d-block d-parent push pull playing push-acc pull-acc playing-acc 
                                        push-s pull-s playing-s max-pitch post-constraints)
    (post-rock-constraints sp d-block push pull playing nil post-constraints)
    (post-rock-constraints sp (accomp d-block) push-acc pull-acc playing-acc nil t)

     ;; constrain d such that it has a difference of (difference-percent-s d-block) with notes played in s-block
    ;; transposed the number of semitones asked of the d-block
    (let ((diff (difference-percent-s d-block)) 
            temp-push  temp-playing      
        )
        (setq temp-push (transpose-chords-semitones sp (chord-key (s-block d-parent)) (chord-quality (s-block d-parent))
                                        (semitones d-block) push-s))
        
        (cst-common-vars sp temp-push push (- 100 diff))
    )
)

;; constrain c such that is respects the cadence specific rules
(defun constrain-c (sp c-block c-parent push pull playing push-acc pull-acc playing-acc max-pitch post-constraints)

    (let ((block-list-len (length (block-list (parent c-parent)))) ;; how many blocks are in the global structure
        (position (block-position c-parent)) ;; position of the current block in the global structure (start index is 0)
        (c-type (cadence-type c-block))
        (key (chord-key c-block))
        (quality (chord-quality c-block))
        (chord-midi-value (name-to-note-value (chord-key c-block)))
        (triad-to-play (list)) ;; intervals depending on quality
        (chords-to-play (list)) ;; root key(s) on which the triad(s) is(are) played
        (notes-to-play (list)) ;; notes to be pushed, list of lists
        (mnl (min-note-length (accomp c-block)))
        )
        (cond ((string= quality "Major") (setq triad-to-play (list 0 4 7)))
            ((string= quality "Minor") (setq triad-to-play (list 0 3 7)))
            ((string= quality "Augmented") (setq triad-to-play (list 0 4 8)))
            ((string= quality "Diminished") (setq triad-to-play (list 0 3 6)))
        )
        (cond 
            ((string= c-type "Default") 
                (print "cadence-type")
                (print "Default")
                ;; TODO: Set a default type of cadence depending on for example, the position of a block within the structure
                ;; (more conclusive cadence towards the end etc.)
            )
            ((string= c-type "None") 
                (print "cadence-type")
                (print "No cadence")
                ;; TODO: Check if None functions properly
            )
            ((string= c-type "Perfect") 
                (print "cadence-type")
                (print "Perfect")

                ;; Perfect V -> I
                (setq chords-to-play (list 7 0))
                (setq notes-to-play (append notes-to-play (list (+ (+ chord-midi-value (nth 0 chords-to-play)) (nth 0 triad-to-play)) (+ (+ chord-midi-value (nth 0 chords-to-play)) (nth 1 triad-to-play)) (+ (+ chord-midi-value (nth 0 chords-to-play)) (nth 2 triad-to-play)))))

                (setq notes-to-play (append (list notes-to-play) (list (list (+ (+ chord-midi-value (nth 1 chords-to-play)) (nth 0 triad-to-play)) (+ (+ chord-midi-value (nth 1 chords-to-play)) (nth 1 triad-to-play)) (+ (+ chord-midi-value (nth 1 chords-to-play)) (nth 2 triad-to-play))))))

                (gil::g-rel sp (nth 0 push-acc) gil::SRT_EQ (nth 0 notes-to-play))
                (gil::g-rel sp (nth (* (/ mnl 2) (bar-length (accomp c-block))) push-acc) gil::SRT_EQ (nth 1 notes-to-play))
            )
            ((string= c-type "Plagal") 
                (print "cadence-type")
                (print "Plagal")

                ;; Plagal IV -> I
                (setq chords-to-play (list 5 0))
                (setq notes-to-play (append notes-to-play (list (+ (+ chord-midi-value (nth 0 chords-to-play)) (nth 0 triad-to-play)) (+ (+ chord-midi-value (nth 0 chords-to-play)) (nth 1 triad-to-play)) (+ (+ chord-midi-value (nth 0 chords-to-play)) (nth 2 triad-to-play)))))

                (setq notes-to-play (append (list notes-to-play) (list (list (+ (+ chord-midi-value (nth 1 chords-to-play)) (nth 0 triad-to-play)) (+ (+ chord-midi-value (nth 1 chords-to-play)) (nth 1 triad-to-play)) (+ (+ chord-midi-value (nth 1 chords-to-play)) (nth 2 triad-to-play))))))

                (gil::g-rel sp (nth 0 push-acc) gil::SRT_EQ (nth 0 notes-to-play))
                (gil::g-rel sp (nth (* (/ mnl 2) (bar-length (accomp c-block))) push-acc) gil::SRT_EQ (nth 1 notes-to-play))
            )
            ((string= c-type "Semi") 
                (print "cadence-type")
                (print "Semi")

                ;; Demi I -> V
                (setq chords-to-play (list 0 7))
                (setq notes-to-play (append notes-to-play (list (+ (+ chord-midi-value (nth 0 chords-to-play)) (nth 0 triad-to-play)) (+ (+ chord-midi-value (nth 0 chords-to-play)) (nth 1 triad-to-play)) (+ (+ chord-midi-value (nth 0 chords-to-play)) (nth 2 triad-to-play)))))

                (setq notes-to-play (append (list notes-to-play) (list (list (+ (+ chord-midi-value (nth 1 chords-to-play)) (nth 0 triad-to-play)) (+ (+ chord-midi-value (nth 1 chords-to-play)) (nth 1 triad-to-play)) (+ (+ chord-midi-value (nth 1 chords-to-play)) (nth 2 triad-to-play))))))

                (gil::g-rel sp (nth 0 push-acc) gil::SRT_EQ (nth 0 notes-to-play))
                (gil::g-rel sp (nth (* (/ mnl 2) (bar-length (accomp c-block))) push-acc) gil::SRT_EQ (nth 1 notes-to-play))   
            )
            ((string= c-type "Deceptive") 
                (print "cadence-type")
                (print "Deceptive")
                ;; Deceptive V -> VI || V -> III
            )
        )
    )

    (let ((bar-len (bar-length c-block))
        (quant 16)
        (chord-midi-value (name-to-note-value (chord-key c-block)))
        notes
        final-idx
        )
        (setq notes (octaves-of-note chord-midi-value))
        (setq final-idx (- (* bar-len quant) 1))
        (gil::g-dom sp (nth final-idx playing) notes)  
    )
    (post-rock-constraints sp c-block push pull playing t post-constraints)

    (post-rock-constraints sp (accomp c-block) push-acc pull-acc playing-acc t t)
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; LIMITING NOTE TO THE SCALE ;;
;; OR THE CHORDS              ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Constraints on polyphonic voices
(defun chord-key-cst (sp playing rock)
    (let ((key (chord-key rock))
        (quality (chord-quality rock))
        (chord-midi-value (name-to-note-value (chord-key rock)))
        (triad-to-play (list)) ;; intervals depending on quality
        (notes-to-play (list))
        )
        (cond ((string= quality "Major") (setq triad-to-play (list 0 4 3)))
            ((string= quality "Minor") (setq triad-to-play (list 0 3 4)))
            ((string= quality "Diminished") (setq triad-to-play (list 0 3 3)))
            ((string= quality "Augmented") (setq triad-to-play (list 0 4 4)))
        )
        (setq notes-to-play (build-chordset triad-to-play (- chord-midi-value 60)))
        (loop :for i :from 0 :below (length playing) :do
            (let ((bool-array (gil::add-bool-var-array sp (length notes-to-play) 0 1)));;Array to state that one triad is played
                (loop :for j :from 0 :below (length notes-to-play) :do
                    (gil::g-rel-reify sp (nth i playing) gil::SRT_EQ (nth j notes-to-play) (nth j bool-array) gil::RM_IMP)
                )
                ;; Exactly one triad can be played at each time
                (gil::g-rel sp gil::BOT_OR bool-array 1)
            )
        )
    )
)

;; Constraints on monophonic voices
(defun chord-key-cst-int (sp push playing rock)
    (let (
        (chord (get-scale-chord (chord-quality rock)))
        (offset (- (name-to-note-value (chord-key rock)) 60))
        chordset
        )
        (setq chordset (build-scaleset chord offset))
        (loop :for i :from 0 :below (length playing) :by 1 :do
            (let (bool-array bool-temp)
                (setq bool-array (gil::add-bool-var-array sp (+ (length chordset) 1) 0 1))
                (loop :for n :from 0 :below (length chordset) :by 1 :do
                    (let (bool)
                        (setq bool (gil::add-bool-var-expr sp (nth i playing) gil::IRT_EQ (nth n chordset)))
                        (gil::g-rel sp bool gil::IRT_EQ (nth n bool-array))
                    )
                )
                (setq bool-temp (gil::add-bool-var-expr sp (nth i playing) gil::IRT_EQ -1))
                (gil::g-rel sp bool-temp gil::IRT_EQ (nth (length chordset) bool-array))
                (gil::g-rel sp gil::BOT_OR bool-array 1)
            )
        )
    )
)

;;;;;;;;;;;;;;;;;;;;;;;;
; LIMITING PITCH RANGE ;
;;;;;;;;;;;;;;;;;;;;;;;;

(defun pitch-range (sp push min-pitch max-pitch)
    (loop :for j :below (length push) :by 1 :do
        (if (typep (nth j push) 'gil::int-var)
            ;; Constraints on monophonic voices
            (progn 
                (let (bool-temp bool-one bool-min bool-max)
                    (setq bool-one (gil::add-bool-var-expr sp (nth j push) gil::IRT_EQ -1))
                    (setq bool-min (gil::add-bool-var-expr sp (nth j push) gil::IRT_GQ min-pitch))
                    (setq bool-max (gil::add-bool-var-expr sp (nth j push) gil::IRT_LQ max-pitch))
                    (setq bool-temp (gil::add-bool-var sp 0 1))
                    (gil::g-op sp bool-min gil::BOT_AND bool-max bool-temp)
                    (gil::g-op sp bool-temp gil::BOT_OR bool-one 1)
                )
            )
            ;; Constraints on polyphonic voices
            (gil::g-dom-ints sp (nth j push) gil::SRT_SUB min-pitch max-pitch)
        )
    )
)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; LIMITING MINIMUM NOTE LENGTH ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun note-min-length-rock (sp push pull playing min-length)
    (loop :for j :from 0 :below (length push) :by 1 :do
        (loop :for k :from 1 :below min-length :by 1 :while (< (+ j k) (length pull)) :do
            (if (typep (nth j push) 'gil::int-var)
                ;; Constraints on monophonic voices
                (let (bool-temp bool2 bool3 bool4 bool5 bool6)
                    (setq bool-temp (gil::add-bool-var-expr sp (nth j push) gil::IRT_NQ -1))
                    (setq bool3 (gil::add-bool-var-expr sp (nth (+ j k) pull) gil::IRT_EQ -1))
                    (gil::g-op sp bool-temp gil::BOT_IMP bool3 1)

                    ;; Limiting silence minimum length
                    (if (> j 0)
                        (progn
                            (setq bool2 (gil::add-bool-var-expr sp (nth j playing) gil::IRT_EQ -1))
                            (setq bool5 (gil::add-bool-var-expr sp (nth (- j 1) playing) gil::IRT_NQ -1))
                            (setq bool4 (gil::add-bool-var-expr sp (nth (+ j k) playing) gil::IRT_EQ -1))
                            (setq bool6 (gil::add-bool-var sp 0 1))
                            (gil::g-op sp bool5 gil::BOT_AND bool2 bool6)
                            (gil::g-op sp bool6 gil::BOT_IMP bool4 1)
                        )
                        (progn
                            (setq bool2 (gil::add-bool-var-expr sp (nth j playing) gil::IRT_EQ -1))
                            (setq bool4 (gil::add-bool-var-expr sp (nth (+ j k) playing) gil::IRT_EQ -1))
                            (gil::g-op sp bool2 gil::BOT_IMP bool4 1)
                        )
                    )
                    
                )
                ;; Constraints on polyphonic voices
                (gil::g-rel sp (nth (+ j k) pull) gil::SRT_DISJ (nth j push))
            )
        )
    )
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; LIMITING MAXIMUM NOTE LENGTH ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun note-max-length-rock (sp push pull max-length)
    (setq l max-length)
    (if (typep (nth 0 push) 'gil::int-var)
        ;; Constraints on monophonic voices
        (loop :for j :from 0 :below (- (length push) l) :by 1 :do
            (let (  (count (gil::add-int-var sp 0 l))
                    (int-array (gil::add-int-var-array sp l 0 l)))
                (loop :for k :from 0 :below l :by 1 :do
                    (setf (nth k int-array) (gil::add-int-var-expr sp (nth j push) gil::IOP_SUB (nth (+ 1 (+ j k)) pull)))
                )
                (gil::g-count sp int-array 0 gil::IRT_EQ count)
                (gil::g-rel sp count gil::IRT_GQ 1)
            )
        )
        ;; Constraints on polyphonic voices
        (loop :for j :from 0 :below (- (length push) l) :by 1 :do
            (let ((l-pull (gil::add-set-var-array sp l 0 127 0 127))
                (l-pull-union (gil::add-set-var sp 0 127 0 127)))
                (loop :for k :from 0 :below l :by 1 :do
                    (gil::g-rel sp (nth k l-pull) gil::SRT_EQ (nth (+ 1 (+ j k)) pull))
                )
                (gil::g-setunion sp l-pull-union l-pull)
                (gil::g-rel sp (nth j push) gil::SRT_SUB l-pull-union)
            )
        )
    )
    
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; LIMITING THE NUMBER OF COMMON NOTES ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun cst-common-vars (sp vars1 vars2 sim)
    (let (count-vars int-array n-vars perc)
        (setq perc (/ sim 100))
        (setq n-vars (ceiling (* (length vars1) perc)))
        
        (setq count (gil::add-int-var sp 0 (length vars1)))
        (setq int-array (gil::add-int-var-array sp (length vars1) -127 127))

        (loop :for i :from 0 :below (min (length vars1) (length vars2)) do
            (setf (nth i int-array) (gil::add-int-var-expr sp (nth i vars1) gil::IOP_SUB (nth i vars2)))
        )

        (gil::g-count sp int-array 0 gil::IRT_EQ count)
        (gil::g-rel sp count gil::IRT_GQ n-vars)
    )
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; LIMITING THE NUMBER OF DIFFERENT NOTES ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun cst-different-vars (sp vars1 vars2 diff)
    (let (count-vars int-array n-vars perc)
        (setq perc (/ diff 100))
        (setq n-vars (ceiling (* (length vars1) perc)))
        
        (setq count (gil::add-int-var sp 0 (length vars1)))
        (setq int-array (gil::add-int-var-array sp (length vars1) -127 127))

        (loop :for i :from 0 :below (min (length vars1) (length vars2)) do
            (setf (nth i int-array) (gil::add-int-var-expr sp (nth i vars1) gil::IOP_SUB (nth i vars2)))
        )

        (gil::g-count sp int-array 0 gil::IRT_NQ count)
        (gil::g-rel sp count gil::IRT_GQ n-vars)
    )
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; LIMITING THE INTERVALS BETWEEN NOTES ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun limit-intervals-cst (sp playing)
    (let ((max-interval 7))
        (loop :for i :from 1 :below (length playing) :do
            (limit-one-interval-cst sp (nth i playing) (nth (- i 1) playing) max-interval)
        )
    )
)

(defun limit-one-interval-cst (sp playing-i playing-i-one max-interval)
    (let (bool-interval-max interval interval-abs bool-pi bool-pi-one bool)
            (setq bool-pi (gil::add-bool-var-expr sp playing-i gil::IRT_EQ -1))
            (setq bool-pi-one (gil::add-bool-var-expr sp playing-i-one gil::IRT_EQ -1))

            ;; Define the interval between the two notes
            ;; interval = |playing[i] - playing[i-1]|
            (setq interval (gil::add-int-var-expr sp playing-i gil::IOP_SUB playing-i-one))
            (setq interval-abs (gil::add-int-var sp 0 127))
            (gil::g-abs sp interval interval-abs)

            ;; The maximum interval
            ;; interval <= 7 (perfect fifth)
            (setq bool-interval-max (gil::add-bool-var-expr sp interval-abs gil::IRT_LQ max-interval))

            ;; playing[i] = -1 OR |interval| <= max-interval
            (setq bool (gil::add-bool-var sp 0 1))
            (gil::g-op sp bool-pi gil::BOT_OR bool-pi-one bool)
            (gil::g-op sp bool gil::BOT_OR bool-interval-max 1)
        )
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; TRANSPOSING AN ARRAY OF VARIABLE ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun transpose-chords-key (sp chord1 quality1 chord2 quality2 push)
    (let (
        (notes (build-scaleset (get-scale-chord quality1)
                    (- (name-to-note-value chord1) 60)))
        (new-notes (build-scaleset (get-scale-chord quality2)
                    (- (name-to-note-value chord2) 60)))
        temp-push        
        )
        (setq notes (append '(-1) notes))
        (setq new-notes (append '(-1) new-notes))
        (setq temp-push (gil::add-int-var-array sp (length push) -1 127))
        (loop :for i :from 0 :below (length push) :do
            (let ((bool-array (gil::add-bool-var-array sp (length notes) 0 1)) bool-temp bool-tot difference)
                (loop :for n :from 0 :below (min (length notes) (length new-notes)) :do
                    (let (bool1 bool2)
                        ;; If the note belongs to the chord, force the new note to belong to the new chord
                        (setq bool1 (gil::add-bool-var-expr sp (nth i push) gil::IRT_EQ (nth n notes)))
                        (setq bool2 (gil::add-bool-var-expr sp (nth i temp-push) gil::IRT_EQ (nth n new-notes)))
                        (gil::g-op sp bool1 gil::BOT_IMP bool2 1)
                    )
                )
            )
        )
        temp-push
    )
)


(defun transpose-chords-semitones (sp chord1 quality1 semitones push)
    (let (
        (notes (build-scaleset (get-scale-chord quality1)  ;if - mode selectionné
                    (- (name-to-note-value chord1) 60)))
        temp-push  new-notes      
        )
        (setq new-notes (loop :for i :from 0 :below (length notes) :collect (+ (nth i notes) semitones)))
        (setq notes (append '(-1) notes))
        (setq new-notes (append '(-1) new-notes))
        (setq temp-push (gil::add-int-var-array sp (length push) -1 127))
        (loop :for i :from 0 :below (length push) :do
            (let ((bool-array (gil::add-bool-var-array sp (length notes) 0 1)) bool-temp bool-tot difference)
                (loop :for n :from 0 :below (min (length notes) (length new-notes)) :do
                    (let (bool1 bool2)
                        ;; If the note belongs to the chord, force the new note to belong to the new chord
                        (setq bool1 (gil::add-bool-var-expr sp (nth i push) gil::IRT_EQ (nth n notes)))
                        (setq bool2 (gil::add-bool-var-expr sp (nth i temp-push) gil::IRT_EQ (nth n new-notes)))
                        (gil::g-op sp bool1 gil::BOT_IMP bool2 1)
                    )
                )
            )
        )
        temp-push
    )
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; MINIMISE THE INTERVAL BETWEEN TWO NOTES ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun minimise-interval (sp playing-i-one playing-i chord quality)
    (let (interval interval-abs scalset interval-array)
        (setq interval (gil::add-int-var-expr sp playing-i gil::IOP_SUB playing-i-one))
        (setq interval-abs (gil::add-int-var sp 0 127))
        (gil::g-abs sp interval interval-abs)

        (setq scaleset (append '(-1) (build-scaleset (get-scale-chord quality)  ;if - mode selectionné
                    (- (name-to-note-value chord) 60))))
        (setq interval-array (gil::add-int-var-array sp (length scaleset) -1 127))
        (loop :for i :from 0 :below (length scaleset) :do
            (let (interval-temp (note (nth i scaleset)))
                (setq interval-temp (gil::add-int-var-expr sp playing-i-one gil::IOP_SUB note))
                (gil::g-abs sp interval-temp (nth i interval-array))
            )
        )   
        (gil::g-lmin sp interval-abs interval-array)     
    )
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; LIMIT THE OVERALL INTERVAL BETWEEN THE NOTES ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun limit-song-interval (sp playing max-interval)
    (let ((max-note (gil::add-int-var sp 0 127))
        (min-note (gil::add-int-var sp 0 127))
        (playing-notes (gil::add-int-var-array sp (length playing) 0 127))
        (average-note (gil::add-int-var sp 0 127))
        interval
        )
        (gil::g-lmax sp max-note playing)
        (gil::g-rel sp average-note gil::IRT_EQ 63)

        (loop :for i :from 0 :below (length playing) :do
            (let (bool)
                (setq bool (gil::add-bool-var-expr sp (nth i playing) gil::IRT_EQ -1))
                (gil::g-ite sp bool average-note (nth i playing) (nth i playing-notes))
            )
        )
        (gil::g-lmin sp min-note playing-notes)
        (setq interval (gil::add-int-var-expr sp max-note gil::IOP_SUB min-note))
        (gil::g-rel sp interval gil::IRT_LQ max-interval)    
    )
)
