(in-package :mldz)

;;;;;;;;;;;;;;;;;
; NEW-MELODIZER ;
;;;;;;;;;;;;;;;;;

; <rock-csp> list of the child block objects
; <percent-diff> percentage of difference wanted for the solutions
; This function creates the CSP by creating the space and the variables, posting the constraints and the branching, specifying
; the search options and creating the search engine.
(defmethod rock-solver (rock-csp percent-diff branching)
    (let ((sp (gil::new-space)); create the space;
        push pull playing  
        dfs tstop sopts scaleset pitch temp push-card
        pos

        (max-pitch 127)
        (bars (bar-length rock-csp))
        (quant 16)
        (min-length 1) ;minimum length of a note with associated constraint
        (chord-rhythm 2) ;a chord is played every [chord-rhythm] quant
        (chord-min-length 2)) ; minimum length of a chord with associated constraint
        (print "csp")
        (print rock-csp)
        (print "block-list of the csp")
        (print (block-list rock-csp))

        ;Setting constraint for this block and child blocks
        (setq temp (get-sub-rock-values sp rock-csp))
        (setq push (nth 0 temp))
        (setq pull (nth 1 temp))
        (setq playing (nth 2 temp))
        ;(setq push-card (nth 5 temp))

        (gil::g-specify-sol-variables sp push)

        (gil::g-branch sp push gil::SET_VAR_SIZE_MIN gil::SET_VAL_RND_INC)
        ;; (gil::g-branch sp pull gil::SET_VAR_SIZE_MIN gil::SET_VAL_RND_INC)

        ;time stop
        (setq tstop (gil::t-stop)); create the time stop object
        (gil::time-stop-init tstop 500); initialize it (time is expressed in ms)

        ;search options
        (setq sopts (gil::search-opts)); create the search options object
        (gil::init-search-opts sopts); initialize it
        (gil::set-n-threads sopts 1); set the number of threads to be used during the search (default is 1, 0 means as many as available)
        (gil::set-time-stop sopts tstop); set the timestop object to stop the search if it takes too long

        ; search engine
        (setq se (gil::search-engine sp (gil::opts sopts) gil::DFS))

        (print "new-melodizer basic CSP constructed")
        ; return
        (list se push pull playing tstop sopts bars quant sp)
    )
)

;recursive function to set the constraint on all the blocks in the tree structure
; TODO : adapt function for A A B A and launch functions for s r d c
(defun get-sub-rock-values (sp rock-csp)
    (print "At the start of get-sub-rock-values (sp rock-csp)")

    ; for block child of rock-csp
    ; (pull supersets de get-sub-block-values(block) )
    ; constraints
    ; return pull push playing
    (let (pull push playing block-list positions
        sub-push sub-pull push-card 

         (bars (bar-length rock-csp))
         (quant 16)
         (max-pitch 127)
         )
        (print "get subblocks")

        ;; initialize the variables
        (setq push (gil::add-set-var-array sp (+ (* bars quant) 1) 0 max-pitch 0 1))
        (setq pull (gil::add-set-var-array sp (+ (* bars quant) 1) 0 max-pitch 0 1))
        (setq playing (gil::add-set-var-array sp (+ (* bars quant) 1) 0 max-pitch 0 1))
        (setq push-card (gil::add-int-var-array sp (+ (* bars quant) 1) 0 127))

        ;; connects push pull and playing with constraints
        (link-push-pull-playing sp push pull playing max-pitch)
        (link-push-push-card sp push push-card)
        
        

        ;; set constraints on push pull and playing from all blocks in the structure
        (setq block-list (block-list rock-csp))
        
        ;; iterate over all blocks A and B in block-list
        (loop :for i :from 0 :below (length block-list) :by 1 :do
            ;; for every A/B block, post constraints from s,r,d,c
            ;; cut the push pull playing array into (length block-list) parts and feed the adequate part
            ;; to (constrain-ppp-from-srdc)
            (let (temp-push temp-pull temp-playing temp-push-card srdc-parent startidx notes-per-block)
                (setq notes-per-block (/(* bars quant) (length block-list)))
                (setq startidx (* i notes-per-block))
                (setq srdc-parent (nth i block-list))
                (setq temp-push (sublst push startidx notes-per-block))
                (setq temp-pull (sublst pull startidx notes-per-block))
                (setq temp-playing (sublst playing startidx notes-per-block))
                (setq temp-push-card (sublst push-card startidx notes-per-block))
                (constrain-srdc-from-parent srdc-parent temp-push temp-pull temp-playing temp-push-card quant sp)
            )
        )


        (print "At the end of get-sub-rock-values (sp rock-csp)")
        ;; return
        (list push pull playing)
    )
)

;posts the optional constraints specified in the list
; TODO CHANGE LATER SO THE FUNCTION CAN BE CALLED FROM THE STRING IN THE LIST AND NOT WITH A SERIES OF IF STATEMENTS
(defun post-optional-rock-constraints (sp rock push pull playing push-card); sub-push sub-pull)

    (if (min-pushed-notes rock)
        (min-pushed-notes-cst sp push-card (min-pushed-notes rock))
    )

    (if (max-pushed-notes rock)
        (gil::g-card sp push 0 (max-pushed-notes rock))
    )

    ; Time constraints
    (if (min-note-length-flag rock)
        (note-min-length-rock sp push pull (min-note-length rock))
    )

    (if (max-note-length-flag rock)
        (note-max-length-rock sp push pull (max-note-length rock))
    )

    (if (quantification rock)
        (set-quantification sp push pull (quantification rock))
    )

    ; Pitch constraints
    ; following a scale
    (if (key-selection rock)
        (key-selection-cst sp push (key-selection rock) (mode-selection rock))
    )

    (pitch-range sp push (min-pitch rock) (max-pitch rock))
)

;;;;;;;;;;;;;;;
; SEARCH-NEXT ;
;;;;;;;;;;;;;;;

; <l> is a list containing the search engine for the problem and the variables
; <rock-object> is a rock object
; this function finds the next solution of the CSP using the search engine given as an argument
(defmethod new-rock-next (l rock-object)
    (let ((se (first l))
         (push (second l))
         (pull (third l))
         (playing (fourth l))
         (tstop (fifth l))
         (sopts (sixth l))
         (bars (seventh l))
         (quant (eighth l))
         (sp (ninth l))
         (check t); for the while loop
         sol score (p-push (list)) (p-pull (list)) (p-playing (list)))

         (print "in search basic")

        (om::while check :do
            (gil::time-stop-reset tstop);reset the tstop timer before launching the search
            (setq sol (gil::search-next se)); search the next solution
            (if (null sol)
                (stopped-or-ended (gil::stopped se) (stop-search rock-object) tstop); check if there are solutions left and if the user wishes to continue searching
                (setf check nil); we have found a solution so break the loop
            )
        )
        ;; (print (percent-diff rock-object))
        ;; (set-percent-diff sp (percent-diff rock-object) sol push pull playing)

        ;; (setq se (gil::search-engine sp (gil::opts sopts) gil::BAB))

         ;créer score qui retourne la liste de pitch et la rhythm tree
        (setq score-voice (build-voice sol push pull bars quant (tempo rock-object)))

        (list 
        (make-instance 'om::voice
            :chords (first score-voice)
            :tree (second score-voice)
        )
        se push pull playing tstop sopts bars quant sp)

        ;; (setq score-chord-seq (build-chord-seq sol push pull bars quant (tempo rock-object)))

        ;; (make-instance 'chord-seq
        ;;     :LMidic (first score-chord-seq)
        ;;     :LOnset (second score-chord-seq)
        ;;     :Ldur (third score-chord-seq)
        ;; )
    )
)

; determines if the search has been stopped by the solver because there are no more solutions or if the user has stopped the search
(defun stopped-or-ended (stopped-se stop-user tstop)
    (if (= stopped-se 0); if the search has not been stopped by the TimeStop object, there is no more solutions
        (error "There are no more solutions.")
    )
    ;otherwise, check if the user wants to keep searching or not
    (if stop-user
        (error "The search has been stopped. Press next to continue the search.")
    )
)
