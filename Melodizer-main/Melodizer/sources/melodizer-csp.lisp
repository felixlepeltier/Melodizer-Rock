(in-package :mldz)

;;;;;;;;;;;;;;;;;
; NEW-MELODIZER ;
;;;;;;;;;;;;;;;;;

; <input> is a voice object with the chords on top of which the melody will be played
; <rhythm> the rhythm of the melody to be found in the form of a voice object
; <optional-constraints> is a list of optional constraint names that have to be applied to the problem
; <global interval> is the global interval that the melody should cover if the mostly increasing/decreasing constraint is selected
; <key> is the key in which the melody is
; <mode> is the mode of the tonality (major, minor)
; This function creates the CSP by creating the space and the variables, posting the constraints and the branching, specifying
; the search options and creating the search engine.
(defmethod new-melodizer (block-csp)
    (let ((sp (gil::new-space)); create the space;
        push pull playing pushMap pullMap dfs tstop sopts scaleset pitch temp
        (max-pitch 127)
        (bars (bar-length block-csp))
        (quant (get-quant (quantification block-csp)))
        (min-length 1) ;minimum length of a note with associated constraint
        (chord-rhythm 2) ;a chord is played every [chord-rhythm] quant
        (chord-min-length 2) ; minimum length of a chord with associated constraint
        (major-natural (list 2 2 1 2 2 2 1)) ; represent intervals of the scale we are composing in
        (chord-prog (list 1 5 6 4))) ; represent the chord progression we want to follow
        (setf scaleset (build-scaleset major-natural))
        (setf chordset (build-chordset chord-prog major-natural))
        (setf progsize (length chord-prog))

        (print block-csp)
        (setq temp (get-sub-block-values sp block-csp))
        (setq push (nth 0 temp))
        (setq pull (nth 1 temp))
        (setq playing (nth 2 temp))



        ;initialize the variables
        ;(setq push (gil::add-set-var-array sp (* bars quant) 0 max-pitch 0 max-pitch))
        ;(setq pull (gil::add-set-var-array sp (* bars quant) 0 max-pitch 0 max-pitch))
        ;(setq playing (gil::add-set-var-array sp (* bars quant) 0 max-pitch 0 max-pitch))

        ;channeling array with time as index to array with pitch as index
        ;(setq pushMap (gil::add-set-var-array sp (+ max-pitch 1) 0 (* bars quant) 0 (* bars quant)))
        ;(setq pullMap (gil::add-set-var-array sp (+ max-pitch 1) 0 (* bars quant) 0 (* bars quant)))
        ;(gil::g-channel sp push pushMap)
        ;(gil::g-channel sp pull pullMap)

        ;initial constraint on pull, push and playing
        ;(gil::g-empty sp (first pull)) ; pull[0] == empty
        ;(gil::g-empty sp (car (last push)))  ; push[bars*quant] == empty
        ;(gil::g-rel sp (first push) gil::SRT_EQ (first playing)) ; push[0] == playing [0]

        ;connect push, pull and playing
        ;(loop :for j :from 1 :below (* bars quant) :do ;for each interval
        ;    (let (temp)
        ;        (setq temp (gil::add-set-var sp 0 max-pitch 0 max-pitch)); temporary variables
        ;
        ;        (gil::g-op sp (nth (- j 1) playing) gil::SOT_MINUS (nth j pull) temp); temp[0] = playing[j-1] - pull[j]
        ;        (gil::g-op sp temp gil::SOT_UNION (nth j push) (nth j playing)); playing[j] == playing[j-1] - pull[j] + push[j] Playing note
        ;
        ;        (gil::g-rel sp (nth j pull) gil::SRT_SUB (nth (- j 1) playing)) ; pull[j] <= playing[j-1] cannot pull a note not playing
        ;
        ;        (gil::g-set-op sp (nth (- j 1) playing) gil::SOT_UNION (nth j pull) gil::SRT_DISJ (nth j push)); push[j] || playing[j-1] + pull[j] Cannot push a note still playing
        ;    )
        ;)

         ;cardinality constraint
        ;(gil::g-card sp playing 0 5) ; piano can only play 10 notes at a time
        ;(gil::g-card sp pull 0 10) ; can't release more notes than we play
        ;(gil::g-card sp push 0 5) ; can't start playing more than 5 notes at a time

        ;(post-optional-constraits sp block)
        ;(pitch-range sp push (min-pitch block) (max-pitch block))

        ; Following a scale
        ;(loop :for j :from 0 :below (* bars quant) :do
        ;    (gil::g-rel sp (nth j push) gil::SRT_SUB scaleset)
        ;)

        ;Following a chord progression
        ;(loop :for j :from 0 :below (length chordset) :by 1 :do
        ;    (loop :for k :from 0 :below (/ (* bars quant) progsize) :by 1 :do
        ;        (gil::g-rel sp (nth (+ k (/ (* (* bars quant) j) progsize)) push) gil::SRT_SUB (nth j chordset))
        ;    )
        ;)

        ; pitch range limitation
        ;(loop :for j :below (* bars quant) :by 1 :do
        ;    (gil::g-dom-ints sp (nth j push) gil::SRT_SUB 50 80)
        ;)

        ; Minimum length of note
        ;(loop :for j :from 0 :below (* bars quant) :by 1 :do
        ;     (loop :for k :from 1 :below min-length :while (< (+ j k) (* bars quant)) :do
        ;        (gil::g-rel sp (nth (+ j k) pull) gil::SRT_DISJ (nth j push))
        ;     )
        ;)

        ; chord rhythm
        ;(loop :for j :from 0 :below (* bars quant) :by 1 :do
        ;      (if (= (mod j chord-rhythm) 0)
        ;         (gil::g-card sp (nth j push) 3 3)
        ;          (gil::g-card sp (nth j push) 0 1)
        ;      )
        ;)

        ; chord length (need previous constraint to work)
        ;(loop :for j :from 0 :below (* bars quant) :by 1 :do
        ;      (if (= (mod j chord-rhythm) 0)
        ;          (loop :for k :from 1 :below chord-min-length :while (< (+ j k) (* bars quant)) :do
        ;              (gil::g-rel sp (nth (+ j k) pull) gil::SRT_DISJ (nth j push))
        ;          )
        ;      )
        ;)

        ; branching
        (gil::g-branch sp push nil nil)
        (gil::g-branch sp pull nil nil)

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

        (print "new-melodizer CSP constructed")
        ; return
        (list se playing tstop sopts bars quant)
    )
)

(defun get-sub-block-values (sp block-csp)
    ; for block child of block-csp
    ; (pull supersets de get-sub-block-values(block) )
    ; constraints
    ; return pull push playing
    (let (pull push playing pushMap pullMap block-list positions
         (bars (bar-length block-csp))
         (quant (get-quant (quantification block-csp)))
         (major-natural (list 2 2 1 2 2 2 1))
         (max-pitch 127))
         (setf scaleset (build-scaleset major-natural))

         (print bars)
         (print quant)

        ;initialize the variables

        (setq push (gil::add-set-var-array sp (* bars quant) 0 max-pitch 0 max-pitch))
        (setq pull (gil::add-set-var-array sp (* bars quant) 0 max-pitch 0 max-pitch))
        (setq playing (gil::add-set-var-array sp (* bars quant) 0 max-pitch 0 max-pitch))

        ;channeling array with time as index to array with pitch as index
        (setq pushMap (gil::add-set-var-array sp (+ max-pitch 1) 0 (* bars quant) 0 (* bars quant)))
        (setq pullMap (gil::add-set-var-array sp (+ max-pitch 1) 0 (* bars quant) 0 (* bars quant)))
        (gil::g-channel sp push pushMap)
        (gil::g-channel sp pull pullMap)


        (setq block-list (block-list block-csp))
        (setq positions (position-list block-csp))

        ;initial constraint on pull, push and playing
        (gil::g-empty sp (first pull)) ; pull[0] == empty
        (gil::g-empty sp (car (last push)))  ; push[bars*quant] == empty
        (gil::g-rel sp (first push) gil::SRT_EQ (first playing)) ; push[0] == playing [0]

        ;connect push, pull and playing
        (loop :for j :from 1 :below (* bars quant) :do ;for each interval
            (let (temp)
                (setq temp (gil::add-set-var sp 0 max-pitch 0 max-pitch)); temporary variables

                (gil::g-op sp (nth (- j 1) playing) gil::SOT_MINUS (nth j pull) temp); temp[0] = playing[j-1] - pull[j]
                (gil::g-op sp temp gil::SOT_UNION (nth j push) (nth j playing)); playing[j] == playing[j-1] - pull[j] + push[j] Playing note

                (gil::g-rel sp (nth j pull) gil::SRT_SUB (nth (- j 1) playing)) ; pull[j] <= playing[j-1] cannot pull a note not playing

                (gil::g-set-op sp (nth (- j 1) playing) gil::SOT_UNION (nth j pull) gil::SRT_DISJ (nth j push)); push[j] || playing[j-1] + pull[j] Cannot push a note still playing
            )
        )
        (print "allez")

        ; make the push and pull array supersets of the corresponding array of the child blocks
        (loop :for i :from 0 :below (length block-list) :by 1 :do
              (let (tempPush tempPull tempList (start (* (nth i positions) quant)))
                   (setq tempList (get-sub-block-values sp (nth i block-list)))
                   (setq tempPush (first tempList))
                   (setq tempPull (second tempList))
                   (print "on est la")
                   (loop :for j :from start :below (+ start (length tempPush)) :by 1 :do
                        (gil::g-rel sp (nth j push) gil::SRT_SUP (nth j tempPush))
                        (gil::g-rel sp (nth j pull) gil::SRT_SUP (nth j tempPull))
                   )
                   (print "sheesh")
              )
        )
        (print "AAAAAH")

        ;constraints
        (post-optional-constraints sp block-csp push pull playing scaleset)
        (pitch-range sp push (min-pitch block-csp) (max-pitch block-csp))
        (list push pull playing)
    )
)

;posts the optional constraints specified in the list
; TODO CHANGE LATER SO THE FUNCTION CAN BE CALLED FROM THE STRING IN THE LIST AND NOT WITH A SERIES OF IF STATEMENTS
(defun post-optional-constraints (sp block push pull playing scaleset)
    (scale-follow sp push scaleset)

    ; Block constraints
    (if (voices block)
      (gil::g-card sp playing 0 (voices block))
    )
    (if (min-added-note block)
        (if (max-added-note block)
            (num-added-note sp playing (min-added-note block) (max-added-note block))
            (num-added-note sp playing (min-added-note block) 127)
        )
        (if (max-added-note block)
            (num-added-note sp playing 0 (max-added-note block))
        )
    )

    ; Time constraints
    (if (min-note-length block)
        (note-min-length sp push pull (min-note-length block))
    )

    ; Pitch constraints

)

;;;;;;;;;;;;;;;
; SEARCH-NEXT ;
;;;;;;;;;;;;;;;

; <l> is a list containing the search engine for the problem and the variables
; <rhythm> is the input rhythm as given by the user
; <melodizer-object> is a melodizer object
; this function finds the next solution of the CSP using the search engine given as an argument
(defmethod search-next (l rhythm melodizer-object)
    (let ((se (first l))
         (pitch* (second l))
         (tstop (third l))
         (sopts (fourth l))
         (intervals (fifth l))
         (check t); for the while loop
         sol pitches)

        (om::while check :do
            (gil::time-stop-reset tstop);reset the tstop timer before launching the search
            (setq sol (gil::search-next se)); search the next solution
            (if (null sol)
                (stopped-or-ended (gil::stopped se) (stop-search melodizer-object) tstop); check if there are solutions left and if the user wishes to continue searching
                (setf check nil); we have found a solution so break the loop
            )
        )

        (setq pitches (to-midicent (gil::g-values sol pitch*))); store the values of the solution
        (print "solution found")

        ;return a voice object that is the solution we just found
        (make-instance 'voice
            :tree rhythm
            :chords pitches
            :tempo (om::tempo (input-rhythm melodizer-object))
        )
    )
)

; <l> is a list containing the search engine for the problem and the variables
; <rhythm> is the input rhythm as given by the user
; <melodizer-object> is a melodizer object
; this function finds the next solution of the CSP using the search engine given as an argument
(defmethod new-search-next (l melodizer-object)
    (let ((se (first l))
         (playing (second l))
         (tstop (third l))
         (sopts (fourth l))
         (bars (fifth l))
         (quant (sixth l))
         (check t); for the while loop
         sol score)

        (om::while check :do
            (gil::time-stop-reset tstop);reset the tstop timer before launching the search
            (setq sol (gil::search-next se)); search the next solution
            (if (null sol)
                (stopped-or-ended (gil::stopped se) (stop-search melodizer-object) tstop); check if there are solutions left and if the user wishes to continue searching
                (setf check nil); we have found a solution so break the loop
            )
        )

        (print bars)
        (print quant)

         ;créer score qui retourne la liste de pitch et la rhythm tree

        (setq score (build-score sol playing bars quant)); store the values of the solution
        (print "out")
        (print (first score))
        (print (third score))

        (print "la")

        ;return a voice object that is the solution we just found
        (make-instance 'voice
            :tree (second score)
            :chords (first score)
            :ties (third score)
            ;:tempo (om::tempo (input-rhythm melodizer-object))
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
