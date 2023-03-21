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
        push pull playing pushMap pullMap dfs tstop sopts scaleset pitch temp push-card q-push
        pos

        (max-pitch 127)
        (bars (bar-length rock-csp))
        (quant 192)
        (min-length 1) ;minimum length of a note with associated constraint
        (chord-rhythm 2) ;a chord is played every [chord-rhythm] quant
        (chord-min-length 2)) ; minimum length of a chord with associated constraint
        (print "csp")
        (print rock-csp)

        (setq push-list (list))
        (setq pull-list (list))
        (setq playing-list (list))
        (setq debug (list))
        (setq debug2 (list))

        ;Setting constraint for this block and child blocks
        (setq temp (get-sub-rock-values sp rock-csp))
        (setq push (nth 0 temp))
        (setq pull (nth 1 temp))
        (setq playing (nth 2 temp))
        (setq notes (nth 3 temp))
        (setq added-notes (nth 4 temp))
        (setq push-card (nth 5 temp))
        (setq q-push (nth 6 temp))

        (gil::g-specify-sol-variables sp q-push)
        (gil::g-specify-percent-diff sp percent-diff)

        (cond
            ((string-equal branching "Top down")
                (loop :for i :from (- (length push-list) 1) :downto 0 :do
                    (gil::g-branch sp (append (nth i push-list) (nth i pull-list)) gil::SET_VAR_SIZE_MIN gil::SET_VAL_RND_INC)
                )
            )
            ((string-equal branching "Full")
                (progn
                    (setq branch-push (list))
                    (setq branch-pull (list))
                    (loop :for l :in push-list :do
                        (setq branch-push (append branch-push l))
                    )
                    (loop :for l :in pull-list :do
                        (setq branch-pull (append branch-pull l))
                    )
                    (gil::g-branch sp (append branch-push branch-pull) gil::SET_VAR_SIZE_MIN gil::SET_VAL_RND_INC)
                )
            )
            ((string-equal branching "Top down random")
                (loop :for i :from (- (length push-list) 1) :downto 0 :do
                    (gil::g-branch sp (append (nth i push-list) (nth i pull-list)) gil::SET_VAR_RND gil::SET_VAL_RND_INC)
                )
            )
        )

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

        (print "new-melodizer CSP constructed")
        ; return
        (list se push pull tstop sopts bars quant push-list pull-list playing-list debug debug2)
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
    (let (pull push notes playing pushMap pushMap-card pullMap block-list positions max-notes sub-push sub-pull
          push-card added-push added-notes added-push-card q-push q-push-card

         (bars (bar-length rock-csp))
         (quant 192)
         (prevNotes (list))
         (major-natural (list 2 2 1 2 2 2 1))
         (max-pitch 127))
        (print bars)
        (print "get subblocks")
        (setq max-notes (* 127 (+ (* bars quant) 1)))
        ;; (setq max-notes 1)

        ;initialize the variables

        
        (setq push (gil::add-set-var-array sp (+ (* bars quant) 1) 0 max-pitch 0 max-pitch))
        (setq pull (gil::add-set-var-array sp (+ (* bars quant) 1) 0 max-pitch 0 max-pitch))
        (setq playing (gil::add-set-var-array sp (+ (* bars quant) 1) 0 max-pitch 0 max-pitch))

        (setq push-list (nconc push-list (list push)))
        (setq pull-list (nconc pull-list (list pull)))
        (setq playing-list (nconc playing-list (list playing)))

        ;channeling array with time as index to array with pitch as index
        
        (setq pushMap (gil::add-set-var-array sp (+ max-pitch 1) 0 (+ (* bars quant) 1) 0 (+ (* bars quant) 1)))
        (setq pullMap (gil::add-set-var-array sp (+ max-pitch 1) 0 (+ (* bars quant) 1) 0 (+ (* bars quant) 1)))
        (gil::g-channel sp push pushMap)
        (gil::g-channel sp pull pullMap)

        (setq pushMap-card (gil::add-int-var-array sp 128 0 (+ (* bars quant) 1)))
        
        (loop :for i :from 0 :below (length pushMap) :by 1 :do
            (gil::g-card-var sp (nth i pushMap) (nth i pushMap-card))
        )

        ;--------------------------------------
        ;Not all rocks block have no block-list
        ;--------------------------------------
        ;; (if (typep rock-csp 'mldz::rock)
        ;;     (progn 
        ;;     (setq block-list (block-list rock-csp))
        ;;     (if (not (typep block-list 'list))
        ;;         (setq block-list (list block-list))
        ;;     )
        ;;     (setq positions (position-list rock-csp))
        ;;     )
        ;; )
        ;; (if (or (typep rock-csp 'mldz::a) (typep rock-csp 'mldz::b))
        ;;     (progn 
        ;;     (setq block-list 
        ;;         (list (s-block rock-csp)(r-block rock-csp)(d-block rock-csp)(c-block rock-csp)))
        ;;     (setq positions (list 0 1 2 3))
        ;;     )
        ;; )
        ;; (if (or (typep rock-csp 'mldz::s) 
        ;;     (typep rock-csp 'mldz::r) 
        ;;     (typep rock-csp 'mldz::d) 
        ;;     (typep rock-csp 'mldz::c))
        ;;     (progn 
        ;;     (if (melody-source rock-csp)
        ;;         (progn
        ;;             (setq block-list (list (melody-source rock-csp)))
        ;;             (setq positions (list 0))
        ;;         )
        ;;     )
        ;;     )
        ;; )


        ;; DEBUG BY USING WHAT WAS DONE IN melodizer-csp.lisp
        (setq positions (position-list rock-csp))
        (print "before positions")
        (print positions)

        ;initial constraint on pull, push, playing and durations
        (gil::g-empty sp (first pull)) ; pull[0] == empty
        (gil::g-empty sp (car (last push)))  ; push[bars*quant] == empty
        (gil::g-empty sp (car (last playing)))  ; playing[bars*quant] == empty
        (gil::g-rel sp (first push) gil::SRT_EQ (first playing)) ; push[0] == playing [0]

        ;compute notes
        (setq notes (gil::add-int-var sp 0 max-notes))
        (setq push-card (gil::add-int-var-array sp (+ (* bars quant) 1) 0 127))

        (loop :for i :from 0 :below (+ (* bars quant) 1) :by 1 :do
            (gil::g-card-var sp (nth i push) (nth i push-card))
        )
        (gil::g-sum sp notes push-card)

        
        ;compute added notes
        (setq added-push (gil::add-set-var-array sp (+ (* bars quant) 1) 0 max-pitch 0 max-pitch))
        (setq sub-push (gil::add-set-var-array sp (+ (* bars quant) 1) 0 max-pitch 0 max-pitch))
        (setq sub-pull (gil::add-set-var-array sp (+ (* bars quant) 1) 0 max-pitch 0 max-pitch))
        (setq added-notes (gil::add-int-var sp 0 127))
        (setq added-push-card (gil::add-int-var-array sp (+ (* bars quant) 1) 0 127))


        (loop :for i :from 0 :below (+ (* bars quant) 1) :by 1 :do
            (gil::g-card-var sp (nth i added-push) (nth i added-push-card))
        )
        (gil::g-sum sp added-notes added-push-card)


        ;compute q-push
        (setq q-push (gil::add-set-var-array sp (* bars (get-quant (quantification rock-csp))) 0 max-pitch 0 max-pitch))
        (loop :for i :from 0 :below (length q-push) :by 1 :do
            (gil::g-rel sp (nth i q-push) gil::SRT_EQ (nth (* i (get-length (quantification rock-csp))) push))
        )
        (setq q-push-card (gil::add-int-var-array sp (length q-push) 0 127))
        (loop :for i :from 0 :below (length q-push) :by 1 :do
            (gil::g-card-var sp (nth i q-push) (nth i q-push-card))
        )
        
        ;connect push, pull and playing
        (loop :for j :from 1 :below (+ (* bars quant) 1) :do ;for each interval
            (let (temp z c)
                (setq temp (gil::add-set-var sp 0 max-pitch 0 max-pitch)); temporary variables
                (gil::g-op sp (nth (- j 1) playing) gil::SOT_MINUS (nth j pull) temp); temp[0] = playing[j-1] - pull[j]
                (gil::g-op sp temp gil::SOT_UNION (nth j push) (nth j playing)); playing[j] == playing[j-1] - pull[j] + push[j] Playing note
                (gil::g-rel sp (nth j pull) gil::SRT_SUB (nth (- j 1) playing)) ; pull[j] <= playing[j-1] cannot pull a note not playing
                (gil::g-set-op sp (nth (- j 1) playing) gil::SOT_MINUS (nth j pull) gil::SRT_DISJ (nth j push)); push[j] || playing[j-1] - pull[j] Cannot push a note still playing

            )
        )
        
        ;------------------------------------------
        ; Part to change: adapt to A A B A
        ;------------------------------------------
        (if (not (endp block-list))
            ; make the push and pull array supersets of the corresponding array of the child blocks
            (let ((sub-push-list (list)) (sub-pull-list (list)))
                (loop :for i :from 0 :below (+ (* bars quant) 1) :by 1 :do
                    (setq temp1 (gil::add-set-var-array sp (length block-list) 0 max-pitch 0 max-pitch))
                    (setq temp2 (gil::add-set-var-array sp (length block-list) 0 max-pitch 0 max-pitch))
                    (gil::g-setunion sp (nth i sub-push) temp1)
                    (setq sub-push-list (nconc sub-push-list (list temp1)))
                    (gil::g-setunion sp (nth i sub-pull) temp2)
                    (setq sub-pull-list (nconc sub-pull-list (list temp2)))
                    (gil::g-op sp (nth i push) gil::SOT_MINUS (nth i sub-push) (nth i added-push))
                )

                (loop :for i :from 0 :below (length block-list) :by 1 :do
                      (let (tempPush tempPull tempPlaying tempList (start (* (nth i positions) quant)))
                        ;-------------------------------------------------
                        ; Part to change: the recursive not good for rock
                        ;-------------------------------------------------
                           (setq tempList (get-sub-rock-values sp (nth i block-list)))
                           (setq tempPush (first tempList))
                           (setq tempPull (second tempList))
                           (setq tempPlaying (third tempList))
                           (setq prevNotes (nth 7 tempList))

                           (loop :for j :from start :below (+ start (length tempPlaying)) :by 1 :do
                                (gil::g-rel sp (nth (- j start) tempPush) gil::SRT_SUB (nth j push))
                                (gil::g-rel sp (nth (- j start) tempPull) gil::SRT_SUB (nth j pull))
                                (gil::g-rel sp (nth (- j start) tempPlaying) gil::SRT_SUB (nth j playing))
                           )

                           (loop :for j :from 0 :below (length push) :by 1 :do
                                (if (and (>= j start) (< j (+ start (length tempPlaying))))
                                    (gil::g-rel sp (nth (- j start) tempPush) gil::SRT_EQ (nth i (nth j sub-push-list)))
                                    (gil::g-empty sp (nth i (nth j sub-push-list)))
                                )
                           )

                           (loop :for j :from 0 :below (length pull) :by 1 :do
                                (if (and (>= j start) (< j (+ start (length tempPlaying))))
                                    (gil::g-rel sp (nth (- j start) tempPull) gil::SRT_EQ (nth i (nth j sub-pull-list)))
                                    (gil::g-empty sp (nth i (nth j sub-pull-list)))
                                )
                           )
                      )
                )
                
            )
            ; if no block-list
            (progn
                (gil::g-rel sp added-notes gil::SRT_EQ notes)
                (loop :for p :in sub-push :do (gil::g-empty sp p))
                (loop :for p :in sub-pull :do (gil::g-empty sp p))
            )


        )
        (print "At the end of get-sub-rock-values (sp rock-csp)")
        ;constraints
        (post-optional-constraints sp rock-csp push pull playing pushMap pushMap-card notes added-notes push-card sub-push sub-pull q-push q-push-card)
        (pitch-range sp push (min-pitch rock-csp) (max-pitch rock-csp))
        (list push pull playing notes added-notes push-card q-push)
    )
)

;posts the optional constraints specified in the list
; TODO CHANGE LATER SO THE FUNCTION CAN BE CALLED FROM THE STRING IN THE LIST AND NOT WITH A SERIES OF IF STATEMENTS
(defun post-optional-constraints (sp rock push pull playing pushMap pushMap-card notes added-notes push-card sub-push sub-pull q-push q-push-card)

    ; rock constraints
    (if (voices rock)
        (gil::g-card sp playing 0 (voices rock))
    )

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

    (if (min-notes rock)
        (progn
            (gil::g-rel sp notes gil::IRT_GQ (min-notes rock))
        )
    )

    (if (max-notes rock)
        (gil::g-rel sp notes gil::IRT_LQ (max-notes rock))
    )

    (if (min-added-notes rock)
        (gil::g-rel sp added-notes gil::IRT_GQ (min-added-notes rock))
    )

    (if (max-added-notes rock)
        (if (= 0 (max-added-notes rock))
            (progn
                (loop :for i :from 0 :below (length push) :by 1 :do
                    (gil::g-rel sp (nth i push) gil::SRT_EQ (nth i sub-push))
                )
            )
            (gil::g-rel sp added-notes gil::IRT_LQ (max-added-notes rock))
        )

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

    (if (rhythm-repetition rock)
        (set-rhythm-repetition sp push-card (get-length (rhythm-repetition rock)))
    )

    (if (pause-quantity-flag rock)
        (set-pause-quantity sp q-push-card (pause-quantity rock) (bar-length rock) (get-quant (quantification rock)))
    )

    (if (pause-repartition-flag rock)
        (set-pause-repartition sp q-push-card (pause-repartition rock))
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


    (if (note-repetition-flag rock)
        (cond
          ((string-equal (note-repetition-type rock) "Random")
            (random-repeat-note sp push (note-repetition rock) (get-length (quantification rock))))
          ((string-equal (note-repetition-type rock) "Soft")
            (soft-repeat-note sp (note-repetition rock) pushMap-card))
          ((string-equal (note-repetition-type rock) "Hard")
            (hard-repeat-note sp (note-repetition rock) pushMap-card (length q-push)))
        )
    )
)

;;;;;;;;;;;;;;;
; SEARCH-NEXT ;
;;;;;;;;;;;;;;;

; <l> is a list containing the search engine for the problem and the variables
; <melodizer-object> is a melodizer object
; this function finds the next solution of the CSP using the search engine given as an argument
(defmethod new-rock-next (l melodizer-object)
    (let ((se (first l))
         (push (second l))
         (pull (third l))
         (tstop (fourth l))
         (sopts (fifth l))
         (bars (sixth l))
         (quant (seventh l))
         (push-list (eighth l))
         (pull-list (ninth l))
         (playing-list (nth 9 l))
         (debug (nth 10 l))
         (debug2 (nth 11 l))
         (check t); for the while loop
         sol score)

         (print "in search")

        (om::while check :do
            (gil::time-stop-reset tstop);reset the tstop timer before launching the search
            (setq sol (gil::search-next se)); search the next solution
            (if (null sol)
                (stopped-or-ended (gil::stopped se) (stop-search melodizer-object) tstop); check if there are solutions left and if the user wishes to continue searching
                (setf check nil); we have found a solution so break the loop
            )
        )

         ;créer score qui retourne la liste de pitch et la rhythm tree
        (setq score-voice (build-voice sol push pull bars quant 80))


        (make-instance 'om::voice
            :chords (first score-voice)
            :tree (second score-voice)
        )
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
