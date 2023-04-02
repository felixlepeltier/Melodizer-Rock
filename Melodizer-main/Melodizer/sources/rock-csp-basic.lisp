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

        ;Setting constraint for this block and child blocks
        (setq temp (get-sub-rock-values sp rock-csp))
        (setq push (nth 0 temp))
        (setq pull (nth 1 temp))
        (setq playing (nth 2 temp))
        ;(setq push-card (nth 5 temp))
        (print (length push))
        (gil::g-specify-sol-variables sp playing)
        (gil::g-specify-percent-diff sp percent-diff)
        (print percent-diff)

        ;; (gil::g-branch sp push gil::SET_VAR_SIZE_MIN gil::SET_VAL_RND_INC)
        ;; (gil::g-branch sp pull gil::SET_VAR_SIZE_MIN gil::SET_VAL_RND_INC)
        (gil::g-branch sp playing gil::SET_VAR_SIZE_MIN gil::SET_VAL_RND_INC)

        ;time stop
        (setq tstop (gil::t-stop)); create the time stop object
        (gil::time-stop-init tstop 500); initialize it (time is expressed in ms)

        ;search options
        (setq sopts (gil::search-opts)); create the search options object
        (gil::init-search-opts sopts); initialize it
        (gil::set-n-threads sopts 1); set the number of threads to be used during the search (default is 1, 0 means as many as available)
        (gil::set-time-stop sopts tstop); set the timestop object to stop the search if it takes too long

        ; search engine
        (setq se (gil::search-engine sp (gil::opts sopts) gil::BAB))

        (print "new-melodizer basic CSP constructed")
        ; return
        (list se push pull playing tstop sopts bars quant)
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

        ;initialize the variables
        (setq push (gil::add-set-var-array sp (+ (* bars quant) 1) 0 max-pitch 0 1))
        (setq pull (gil::add-set-var-array sp (+ (* bars quant) 1) 0 max-pitch 0 1))
        (setq playing (gil::add-set-var-array sp (+ (* bars quant) 1) 0 max-pitch 0 1))


        ;initial constraint on pull, push, playing and durations
        (gil::g-empty sp (first pull)) ; pull[0] == empty
        ;;-------------------------------------------
        ;; les 3 arrays on une variable de plus pour eviter d'imposer un silence en derniere note
        ;; mais les deux contraintes interdisent un push et playing en dernier lieu rendent 100%
        ;; de diff impossible pour trouver une seconde solution
        ;;-------------------------------------------
        ;; (gil::g-empty sp (car (last push)))  ; push[bars*quant] == empty
        ;; (gil::g-empty sp (car (last playing)))  ; playing[bars*quant] == empty
        (gil::g-rel sp (first push) gil::SRT_EQ (first playing)) ; push[0] == playing [0]

        ;compute quardinality of pushed notes
        ;; (setq push-card (gil::add-int-var-array sp (+ (* bars quant) 1) 0 127))
        ;; (loop :for i :from 0 :below (+ (* bars quant) 1) :by 1 :do
        ;;     (gil::g-card-var sp (nth i push) (nth i push-card))
        ;; )


        ;connect push, pull and playing
        (loop :for j :from 1 :below (+ (* bars quant) 1) :do ;for each interval
            (let (temp z c)
                (setq temp (gil::add-set-var sp 0 max-pitch 0 1)); temporary variables
                (gil::g-op sp (nth (- j 1) playing) gil::SOT_MINUS (nth j pull) temp); temp[0] = playing[j-1] - pull[j]
                (gil::g-op sp temp gil::SOT_UNION (nth j push) (nth j playing)); playing[j] == playing[j-1] - pull[j] + push[j] Playing note
                (gil::g-rel sp (nth j pull) gil::SRT_SUB (nth (- j 1) playing)) ; pull[j] <= playing[j-1] cannot pull a note not playing
                (gil::g-set-op sp (nth (- j 1) playing) gil::SOT_MINUS (nth j pull) gil::SRT_DISJ (nth j push)); push[j] || playing[j-1] - pull[j] Cannot push a note still playing
            )
        )
        

        (print "At the end of get-sub-rock-values (sp rock-csp)")
        ;constraints
        ;(post-optional-rock-constraints sp rock-csp push pull playing push-card sub-push sub-pull)
        ;(pitch-range sp push (min-pitch rock-csp) (max-pitch rock-csp))
        (list push pull playing)
    )
)

;posts the optional constraints specified in the list
; TODO CHANGE LATER SO THE FUNCTION CAN BE CALLED FROM THE STRING IN THE LIST AND NOT WITH A SERIES OF IF STATEMENTS
(defun post-optional-rock-constraints (sp rock push pull playing push-card sub-push sub-pull)

    (if (min-pushed-notes rock)
        (loop :for i :from 0 :below (length push-card) :by 1 :do
            (setq b1 (gil::add-bool-var sp 0 1))
            (gil::g-rel-reify sp (nth i push-card) gil::IRT_EQ 0 b1)
            (setq b2 (gil::add-bool-var sp 0 1))
            (gil::g-rel-reify sp (nth i push-card) gil::IRT_GQ (min-pushed-notes rock) b2)
            (gil::g-rel sp b1 gil::BOT_OR b2)
        )
    )

    (if (max-pushed-notes rock)
        (gil::g-card sp push 0 (max-pushed-notes rock))
    )


    ;; ; Time constraints
    (if (min-note-length-flag rock)
        (note-min-length sp push pull (min-note-length rock))
    )

    (if (max-note-length-flag rock)
        (note-max-length sp push pull (max-note-length rock))
    )

    (if (quantification rock)
        (set-quantification sp push pull (quantification rock))
    )

    ; Pitch constraints
    ; following a scale
    (if (key-selection rock)
        (if (mode-selection rock)
            (let (scaleset
                  (bool (gil::add-bool-var sp 0 1)) ; créer le booleen pour la reify
                  (scale (get-scale (mode-selection rock)))  ;if - mode selectionné
                  (offset (- (name-to-note-value (key-selection rock)) 60)))
                 (setq scaleset (build-scaleset scale offset))
                 (gil::g-rel sp bool gil::SRT_EQ 1) ;forcer le reify a true dans ce cas
                 (scale-follow-reify sp push scaleset bool))
            (let (scaleset
                  (bool (gil::add-bool-var sp 0 1)) ; créer le booleen pour la reify
                  (scale (get-scale "ionian (major)"))  ;else - pas de mode selectionné => major natural
                  (offset (- (name-to-note-value (key-selection rock)) 60)))
                 (gil::g-rel sp bool gil::SRT_EQ 1) ;forcer le reify a true dans ce cas
                 (setq scaleset (build-scaleset scale offset))
                 (scale-follow-reify sp push scaleset bool))
        )
        (if (mode-selection rock)
            (let ((bool-array (gil::add-bool-var-array sp 12 0 1))) ; créer le booleen pour la reify
                (loop :for key :from 0 :below 12 :by 1 :do
                    (setq scale (get-scale (mode-selection rock)))
                    (setq scaleset (build-scaleset scale key))
                    (scale-follow-reify sp push scaleset (nth key bool-array))
                )
                (gil::g-rel sp gil::BOT_OR bool-array 1)
            )
        )
    )
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

        (setq p-push (nconc p-push (mapcar (lambda (n) (gil::g-values sol n)) push)))
        (print p-push)
        (setq p-pull (nconc p-pull (mapcar (lambda (n) (gil::g-values sol n)) pull)))
        (print p-pull)
        (setq p-playing (nconc p-playing (mapcar (lambda (n) (gil::g-values sol n)) playing)))
        (print p-playing)
         ;créer score qui retourne la liste de pitch et la rhythm tree
        (setq score-voice (build-voice sol push pull bars quant (tempo rock-object)))

        (make-instance 'om::voice
            :chords (first score-voice)
            :tree (second score-voice)
        )

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
