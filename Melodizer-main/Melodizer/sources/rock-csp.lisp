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
        dfs tstop sopts scaleset pitch temp
        pos

        (max-pitch 127)
        (bars (bar-length rock-csp))
        (quant 16)
        (min-length 1) ;minimum length of a note with associated constraint
        (chord-rhythm 2) ;a chord is played every [chord-rhythm] quant
        (chord-min-length 2)) ; minimum length of a chord with associated constraint

        ;Setting constraint for this block and child blocks
        (setq temp (get-sub-rock-values sp rock-csp))
        (setq push (nth 0 temp))
        (setq pull (nth 1 temp))
        (setq playing (nth 2 temp))
        (setq push-acc (nth 3 temp))
        (setq pull-acc (nth 4 temp))
        (setq playing-acc (nth 5 temp))

        ;; for BAB
        ;; (gil::g-branch sp (append push pull playing) gil::INT_VAR_SIZE_MIN gil::INT_VAL_RND)
        (gil::g-branch sp push gil::INT_VAR_SIZE_MIN gil::INT_VAL_RND)
        (gil::g-branch sp pull gil::INT_VAR_SIZE_MIN gil::INT_VAL_RND)
        (gil::g-branch sp playing gil::INT_VAR_SIZE_MIN gil::INT_VAL_RND)
        (gil::g-branch sp push-acc gil::SET_VAR_SIZE_MIN gil::SET_VAL_RND_INC)

        (gil::g-specify-sol-variables sp push)
        (gil::g-specify-percent-diff sp percent-diff)

        ;time stop
        (setq tstop (gil::t-stop)); create the time stop object
        (gil::time-stop-init tstop 500); initialize it (time is expressed in ms)

        ;search options
        (setq sopts (gil::search-opts)); create the search options object
        (gil::init-search-opts sopts); initialize it
        (gil::set-n-threads sopts 1); set the number of threads to be used during the search (default is 1, 0 means as many as available)
        (gil::set-time-stop sopts tstop); set the timestop object to stop the search if it takes too long

        ; search engine
        ;; (setq se (gil::search-engine sp (gil::opts sopts) gil::DFS))
        (setq se (gil::search-engine sp (gil::opts sopts) gil::BAB))

        (print "new-melodizer basic CSP constructed")

        ; return
        (list se push pull playing push-acc pull-acc playing-acc tstop sopts bars quant sp)
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
    (let (pull push playing pull-acc push-acc playing-acc block-list positions
        sub-push sub-pull pitches-notes lengths-notes

         (bars (bar-length rock-csp))
         (quant 16)
         (max-pitch 127)
         (max-simultaneous-notes 10)
         (min-simultaneous-notes 0)
         (no-note -1)
         nb-notes
         )
        (print "get subblocks")
        (setq nb-notes (+ (* bars quant) 1))

        ;; initialize the variables
        (setq push (gil::add-int-var-array sp nb-notes no-note max-pitch))
        (setq pull (gil::add-int-var-array sp nb-notes no-note max-pitch))
        (setq playing (gil::add-int-var-array sp nb-notes no-note max-pitch))

        (setq push-acc (gil::add-set-var-array sp nb-notes 0 max-pitch 0 max-simultaneous-notes))
        (setq pull-acc (gil::add-set-var-array sp nb-notes 0 max-pitch 0 max-simultaneous-notes))
        (setq playing-acc (gil::add-set-var-array sp nb-notes 0 max-pitch min-simultaneous-notes max-simultaneous-notes))
        
        ;; connects push pull and playing with constraints
        (link-push-pull-playing-int sp push pull playing max-pitch)
        (limit-intervals-cst sp playing)
        (link-push-pull-playing-set sp push-acc pull-acc playing-acc max-pitch max-simultaneous-notes)
        
        

        ;; set constraints on push pull and playing from all blocks in the structure
        (setq block-list (block-list rock-csp))
        
        ;; iterate over all blocks A and B in block-list
        (loop :for i :from 0 :below (length block-list) :by 1 :do
            ;; for every A/B block, post constraints from s,r,d,c
            ;; cut the push pull playing array into (length block-list) parts and feed the adequate part
            ;; to (constrain-ppp-from-srdc)
            (let (temp-push temp-pull temp-playing temp-push-acc temp-pull-acc temp-playing-acc
                  srdc-parent startidx notes-per-block)
                (setq srdc-parent (nth i block-list))
                (setq notes-per-block (* (bar-length srdc-parent) quant))
                (setq startidx (* i notes-per-block))
                (setq temp-push (sublst push startidx notes-per-block))
                (setq temp-pull (sublst pull startidx notes-per-block))
                (setq temp-playing (sublst playing startidx notes-per-block))
                (setq temp-push-acc (sublst push-acc startidx notes-per-block))
                (setq temp-pull-acc (sublst pull-acc startidx notes-per-block))
                (setq temp-playing-acc (sublst playing-acc startidx notes-per-block))
                (constrain-srdc-from-parent srdc-parent temp-push temp-pull temp-playing 
                                            temp-push-acc temp-pull-acc temp-playing-acc quant max-pitch max-simultaneous-notes sp)
            )
        )

        ;; (post-optional-rock-constraints sp rock-csp push pull playing)
        (print "At the end of get-sub-rock-values (sp rock-csp)")
        ;; return
        (list push pull playing push-acc pull-acc playing-acc)
    )
)

;posts the optional constraints specified in the list
; TODO CHANGE LATER SO THE FUNCTION CAN BE CALLED FROM THE STRING IN THE LIST AND NOT WITH A SERIES OF IF STATEMENTS
(defun post-optional-rock-constraints (sp rock push pull playing); sub-push sub-pull)
    (print "optional constraints")
    (print (min-simultaneous-notes rock))
    (if (and (min-simultaneous-notes rock) (typep (nth 0 push) 'gil::set-var))
        (gil::g-card sp playing (min-simultaneous-notes rock) (max-simultaneous-notes rock))
    )

    (if (and (max-simultaneous-notes rock) (typep (nth 0 push) 'gil::set-var))
        (gil::g-card sp playing (min-simultaneous-notes rock) (max-simultaneous-notes rock))
    )

    ; Time constraints
    (if (min-note-length-flag rock)
        (note-min-length-rock sp push pull (min-note-length rock))
    )

    (if (max-note-length-flag rock)
        (note-max-length-rock sp push pull (max-note-length rock))
    )

    ; Pitch constraints

    (if (chord-key rock)
        (if (typep (nth 0 push) 'gil::set-var)
            (chord-key-cst sp push rock)
            ;; (chord-key-cst-int sp push rock)
        )
        
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
    (let ((se (nth 0 l))
         (push (nth 1 l))
         (pull (nth 2 l))
         (playing (nth 3 l))
         (push-acc (nth 4 l))
         (pull-acc (nth 5 l))
         (playing-acc (nth 6 l))
         (tstop (nth 7 l))
         (sopts (nth 8 l))
         (bars (nth 9 l))
         (quant (nth 10 l))
         (sp (nth 11 l))
         (check t); for the while loop
         sol score-voice score-acc)

         (print "in search basic")
         

        (om::while check :do
            (gil::time-stop-reset tstop);reset the tstop timer before launching the search
            (setq sol (gil::search-next se)); search the next solution
            (if (null sol)
                (stopped-or-ended (gil::stopped se) (stop-search rock-object) tstop); check if there are solutions left and if the user wishes to continue searching
                (setf check nil); we have found a solution so break the loop
            )
        )

        ;créer score qui retourne la liste de pitch et la rhythm tree
        (print "building scores")
        ;; (print (gil::g-values sol push))
        ;; (loop :for i :below (length pull) do (print (gil::g-values sol (nth i pull))))
        ;; (print (gil::g-values sol pull))
        (print (gil::g-values sol playing))
        (setq score-voice (build-voice-int sol push pull bars quant (tempo rock-object)))
        (print score-voice)
        (setq score-acc (build-voice sol push-acc pull-acc bars quant (tempo rock-object)))
        
        (list 
            (make-instance 'om::poly
                :voices (list 
                            (make-instance 'om::voice
                                :chords (first score-voice)
                                :tree (second score-voice)
                                :tempo (tempo rock-object)
                            )
                            (make-instance 'om::voice
                                :chords (first score-acc)
                                :tree (second score-acc)
                                :tempo (tempo rock-object)
                            )
                        )
            )

            se push pull playing push-acc pull-acc playing-acc tstop sopts bars quant sp)

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
