(in-package :mldz)

(defun link-push-pull-playing (sp push pull playing max-pitch max-simultaneous-notes)

    ;initial constraint on pull, push, playing and durations
    ;; (gil::g-empty sp (first pull)) ; pull[0] == empty
    ;;-------------------------------------------
    ;; les 3 arrays on une variable de plus pour eviter d'imposer un silence en derniere note
    ;; mais les deux contraintes interdisent un push et playing en dernier lieu rendent 100%
    ;; de diff impossible pour trouver une seconde solution
    ;;-------------------------------------------
    ;; (gil::g-empty sp (car (last push)))  ; push[bars*quant] == empty
    ;; (gil::g-empty sp (car (last playing)))  ; playing[bars*quant] == empty
    (gil::g-rel sp (first push) gil::SRT_EQ (first playing)) ; push[0] == playing [0]

    ;connect push, pull and playing
    (loop :for j :from 1 :below (length push) :do ;for each interval
        (let (temp z c)
            ;; I think the cardinality should be (gil::add-set-var sp 0 max-pitch 0 max-simultaneous-notes) 
            ;; but doesn't change much right now
            (setq temp (gil::add-set-var sp 0 max-pitch 0 max-simultaneous-notes)); temporary variables
            (gil::g-op sp (nth (- j 1) playing) gil::SOT_MINUS (nth j pull) temp); temp[0] = playing[j-1] - pull[j]
            (gil::g-op sp temp gil::SOT_UNION (nth j push) (nth j playing)); playing[j] == playing[j-1] - pull[j] + push[j] Playing note
            (gil::g-rel sp (nth j pull) gil::SRT_SUB (nth (- j 1) playing)) ; pull[j] <= playing[j-1] cannot pull a note not playing
            (gil::g-set-op sp (nth (- j 1) playing) gil::SOT_MINUS (nth j pull) gil::SRT_DISJ (nth j push)); push[j] || playing[j-1] - pull[j] Cannot push a note still playing
        )
    )
)




(defun constrain-srdc-from-parent (srdc-parent push pull playing push-acc pull-acc playing-acc quant max-pitch max-simultaneous-notes sp)
    ;; call some variant of the post-optional-constraints function to constrain the push pull playing arrays
    ;; from srdc values
    (if (typep srdc-parent 'mldz::a)
        ;; 
        (constrain-srdc-from-A srdc-parent push pull playing push-acc pull-acc playing-acc quant max-pitch max-simultaneous-notes sp)
        ;; 
        (constrain-srdc-from-B srdc-parent push pull playing push-acc pull-acc playing-acc quant max-pitch max-simultaneous-notes sp)
    )
)



(defun constrain-srdc-from-A (A-block push pull playing push-acc pull-acc playing-acc quant max-pitch max-simultaneous-notes sp)
    (print "constrain-srdc-from-A")

    ;; bars*quant elements and starts at startidx
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

        (setq startidx-r (* bars quant))
        (setq temp-push-r (sublst push startidx-r notes-in-subblock))
        (setq temp-pull-r (sublst pull startidx-r notes-in-subblock))
        (setq temp-playing-r (sublst playing startidx-r notes-in-subblock))
        (setq temp-push-r-acc (sublst push-acc startidx-r notes-in-subblock))
        (setq temp-pull-r-acc (sublst pull-acc startidx-r notes-in-subblock))
        (setq temp-playing-r-acc (sublst playing-acc startidx-r notes-in-subblock))
        
        (setq startidx-d (* 2 (* bars quant)))
        (setq temp-push-d (sublst push startidx-d notes-in-subblock))
        (setq temp-pull-d (sublst pull startidx-d notes-in-subblock))
        (setq temp-playing-d (sublst playing startidx-d notes-in-subblock))
        (setq temp-push-d-acc (sublst push-acc startidx-d notes-in-subblock))
        (setq temp-pull-d-acc (sublst pull-acc startidx-d notes-in-subblock))
        (setq temp-playing-d-acc (sublst playing-acc startidx-d notes-in-subblock))
        
        (setq startidx-c (* 3 (* bars quant)))
        (setq temp-push-c (sublst push startidx-c notes-in-subblock))
        (setq temp-pull-c (sublst pull startidx-c notes-in-subblock))
        (setq temp-playing-c (sublst playing startidx-c notes-in-subblock))
        (setq temp-push-c-acc (sublst push-acc startidx-c notes-in-subblock))
        (setq temp-pull-c-acc (sublst pull-acc startidx-c notes-in-subblock))
        (setq temp-playing-c-acc (sublst playing-acc startidx-c notes-in-subblock))



        ;; set constraints on these arrays from the values saved in the slots of s-block 
        ;; s
        (print "constraining s")
        (constrain-s sp s-block A-block temp-push-s temp-pull-s temp-playing-s
                                        temp-push-s-acc temp-pull-s-acc temp-playing-s-acc max-pitch max-simultaneous-notes)

        ;; r

        (print "constraining r")
        (constrain-r sp r-block A-block temp-push-r temp-pull-r temp-playing-r  
                                        temp-push-r-acc temp-pull-r-acc temp-playing-r-acc
                                        temp-push-s temp-pull-s temp-playing-s
                                        max-pitch max-simultaneous-notes)
    
        ;; d

        (print "constraining d")
        (constrain-d sp d-block A-block temp-push-d temp-pull-d temp-playing-d
                                        temp-push-d-acc temp-pull-d-acc temp-playing-d-acc
                                        max-pitch max-simultaneous-notes)

        ;; c
        
        (print "constraining c")
        (constrain-c sp c-block A-block temp-push-c temp-pull-c temp-playing-c
                                        temp-push-c-acc temp-pull-c-acc temp-playing-c-acc
                                        max-pitch max-simultaneous-notes)

    )
)

(defun constrain-srdc-from-B (B-block push pull playing push-acc pull-acc playing-acc quant max-pitch max-simultaneous-notes sp)
    (print "constrain-srdc-from-B")
    ;; for now A and B behave the same way so it suffices to call the function written for A
    ;; when B will have different constraints then this function will have to be changed
    ;; to accomodate this.
    (constrain-srdc-from-A B-block push pull playing push-acc pull-acc playing-acc quant max-pitch max-simultaneous-notes sp)
)


;; for now these constrain-srdc functions take the parent block as argument in case it comes in handy 
;; when we implement more constraints which could be specified through slots of the parent block
(defun constrain-s (sp s-block s-parent push pull playing push-acc pull-acc playing-acc max-pitch max-simultaneous-notes)
    ;; if a source melody is given, then use it to generate push pull playing 
    ;; then constrain the push pull and playing of s to be equal to these arrays

    (if (= (block-position s-parent) 0)
        ;; then we're in the first block of the global structure and the 
        ;; s subblock needs to correspond to the source melody
        ;; (   ;; then a source melody has been passed as argument and its value != nil
        (if (melody-source (parent s-parent))
            (let (push-source pull-source playing-source ppp-source
                var-push-source var-pull-source var-playing-source
                )
                (print "Setting the first s block to the source melody")
                (setq ppp-source (create-push-pull (melody-source (parent s-parent)) 16))

                (setq push-source (first ppp-source))
                (setq pull-source (second ppp-source))
                (setq playing-source (third ppp-source))

                ;; they already have the correct size so no need to create new variables
                ;; set constraints on ppp directly

                (print "push-source")
                (print push-source)

                (print "pull-source")
                (print pull-source)

                (print "playing-source")
                (print playing-source)

                ;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                (setq var-push-source (gil::add-set-var-array sp (length push-source) 0 max-pitch 0 max-simultaneous-notes))
                (setq var-pull-source (gil::add-set-var-array sp (length pull-source) 0 max-pitch 0 max-simultaneous-notes))
                (setq var-playing-source (gil::add-set-var-array sp (length playing-source) 0 max-pitch 0 max-simultaneous-notes))
                
                (loop :for i :from 0 :below (length push-source) :by 1 :do
                    (if (or (typep (nth i push-source) 'list) (/= (nth i push-source) -1))
                        (gil::g-rel sp (nth i var-push-source) gil::SRT_EQ (nth i push-source))
                    )

                    (if (and (not (typep (nth i push-source) 'list)) (= (nth i push-source) -1))
                        (gil::g-empty sp (nth i var-push-source))
                    )
                    (gil::g-rel sp (nth i push) gil::SRT_EQ (nth i var-push-source))
                )
                (loop :for i :from 0 :below (- (length pull-source) 1) :by 1 :do
                    (if (or (typep (nth i pull-source) 'list) (/= (nth i pull-source) -1))
                        (gil::g-rel sp (nth i var-pull-source) gil::SRT_EQ (nth i pull-source))
                    )
                    ;; (if (and (not (typep (nth i pull-source) 'list)) (= (nth i pull-source) -1))
                        ;; (gil::g-empty sp (nth i var-pull-source))
                    ;; )
                    (gil::g-rel sp (nth i pull) gil::SRT_EQ (nth i var-pull-source))
                )
                (loop :for i :from 0 :below (length playing-source) :by 1 :do
                    (if (or (typep (nth i playing-source) 'list) (/= (nth i playing-source) -1))
                        (gil::g-rel sp (nth i var-playing-source) gil::SRT_EQ (nth i playing-source))
                    )
                    ;; (if (and (not (typep (nth i playing-source) 'list)) (= (nth i playing-source) -1))
                    ;;     (gil::g-empty sp (nth i var-playing-source))
                    ;; )
                    (gil::g-rel sp (nth i playing) gil::SRT_EQ (nth i var-playing-source))
                )

                ;; (gil::g-count-setvararray sp push var-push-source 100)
                ;; (gil::g-count-setvararray sp push var-pull-source 100)
                ;; (gil::g-count-setvararray sp push var-playing-source 100)
                ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

                ;; (loop :for i :from 0 :below (length push-source) :by 1 :do
                ;;     (if (or (typep (nth i push-source) 'list) (/= (nth i push-source) -1))
                ;;         (setq var-push-source-temp (gil::add-set-var sp 0 max-pitch 0 max-simultaneous-notes))
                ;;         (gil::g-rel sp (nth i push) gil::SRT_EQ (nth i push-source))
                ;;     )
                ;; )
                ;; (loop :for i :from 0 :below (length pull-source) :by 1 :do
                ;;     (if (or (typep (nth i pull-source) 'list) (/= (nth i pull-source) -1))
                ;;         (gil::g-rel sp (nth i pull) gil::SRT_EQ (nth i pull-source))
                ;;     )
                ;; )
                ;; (loop :for i :from 0 :below (length playing-source) :by 1 :do
                ;;     (if (or (typep (nth i playing-source) 'list) (/= (nth i playing-source) -1))
                ;;         (gil::g-rel sp (nth i playing) gil::SRT_EQ (nth i playing-source))
                ;;     )
                ;; )

                (print "First s block has been set to the source melody")

                ;; now we have to impose that the set-var-arrays push pull playing are equal 
                ;; to the lists push-source, pull-source and playing-source, but how ?
                
                ;; have to do this since the r block depends on these variables due to the 
                ;; similarity constraints between the two blocks

                ;; idea: void 	Gecode::dom (Home home, SetVar x, SetRelType r, const IntSet &s)
                ;; https://www.gecode.org/doc/6.2.0/reference/group__TaskModelSetDom.html
                ;; allows for setting the domain of variable x to an IntSet
                ;; so we can add this constraint to GiL as well as adding IntSets to GiL
                ;; such that we can create IntSets from lists (this is the format
                ;; that sets are returned as from the create-push-pull function)

                ;; this way we could then create a generalized domain constraint like above
                ;; but for SetVarArray, and we could also create a function that transforms
                ;; lists of lists into lists of IntSets to feed the constraint ?
            
            )
            (post-optional-rock-constraints sp s-block push pull playing)
        )
        ;; )
        (post-optional-rock-constraints sp s-block push pull playing)
    )
    
    ;; accompaniment
    (post-optional-rock-constraints sp (accomp s-block) push-acc pull-acc playing-acc)
)

(defun constrain-r (sp r-block r-parent push pull playing push-acc pull-acc playing-acc
                                        push-s pull-s playing-s max-pitch max-simultaneous-notes)

    ;; post optional constraints defined in the rock csp
    ;; dont constrain if source melody given
    (if (melody-source (parent r-parent))
        (post-optional-rock-constraints sp r-block push pull playing)
    )
    (post-optional-rock-constraints sp (accomp r-block) push-acc pull-acc playing-acc)

    ;; constrain r such that it has a similarity of (similarity-percent-s r-block) with notes played in s-block
    (let ((sim (similarity-percent-s r-block)))
        (gil::g-count-setvararray sp playing playing-s sim)
        (gil::g-count-setvararray sp push push-s sim)
    )
)

(defun constrain-d (sp d-block d-parent push pull playing push-acc pull-acc playing-acc max-pitch max-simultaneous-notes)
    (post-optional-rock-constraints sp d-block push pull playing)
    (post-optional-rock-constraints sp (accomp d-block) push-acc pull-acc playing-acc)
)

(defun constrain-c (sp c-block c-parent push pull playing push-acc pull-acc playing-acc max-pitch max-simultaneous-notes)
    (post-optional-rock-constraints sp c-block push pull playing)
    (post-optional-rock-constraints sp (accomp c-block) push-acc pull-acc playing-acc)

    ;; constrain c such that is respects the cadence specific rules
    ;; if cadence-type of the parent block is "Default", then pick cadence depending on the position of the block
    ;; in the global structure for example in the AABA structure, B would be in position 2 and would therefore not
    ;; have a Plagal cadence as this usually marks the end of a complete musical piece

    (print "Before the case on cadence-type")
    (let ((block-list-len (length (block-list (parent c-parent)))) ;; how many blocks are in the global structure
        (position (block-position c-parent)) ;; position of the current block in the global structure (start index is 0)
        (c-type (cadence-type c-parent)))
        (cond 
            ((string= c-type "Default") 
                (print "cadence-type")
                (print "Default")
            )
            ((string= c-type "Perfect") 
                (print "cadence-type")
                (print "Perfect")
                ;; Cadence parfaite : accord du cinquième degré suivi du premier degré. C’est très conclusif et c’est en général utilisé à la fin d’une phrase/section/pièce pour marquer la fin d’une partie, pas nécessairement du morceau complet.
            )
            ((string= c-type "Plagal") 
                (print "cadence-type")
                (print "Plagal")
                ;; Cadence plagale : accord du quatrième degré suivi du premier degré. C’est moins conclusif et moins fréquemment utilisé
            )
            ((string= c-type "Semi") 
                (print "cadence-type")
                (print "Semi")
                ;; Demi cadence : n’importe quel accord vers le cinquième degré. Ca crée de la tension parce que l’harmonie reste en suspension et ne se résoud pas. 
            )
            ((string= c-type "Deceptive") 
                (print "cadence-type")
                (print "Deceptive")
                ;; Deceptive cadence : accord du cinquième degré vers le sixième ou troisième degré. Ca crée une surprise puisque l’oreille s’attend à entendre le premier degré, et c’est souvent utilisé pour prolonger une phrase ou entre 2 sections pour donner un sentiment de continuité.
            )
        )
    )
    (print "After the case on cadence-type")

)


(defun chord-key-cst (sp push rock)
    (if (chord-key rock)
        (if (chord-quality rock)
            (let ((bool (gil::add-bool-var sp 0 1)) ; créer le booleen pour la reify
                    (bool2 (gil::add-bool-var sp 0 1))
                    (chord (get-chord (chord-quality rock)))  ;if - mode selectionné
                    (offset (- (name-to-note-value (chord-key rock)) 60))
                    (all-notes (gil::add-set-var sp 0 127 0 127))
                    chordset notesets bool-array)
                    (setq chordset (build-scaleset chord offset))
                    (scale-follow-reify sp push chordset bool)
                    (setq notesets (build-notesets chord offset))
                    (setq bool-array (gil::add-bool-var-array sp (length notesets) 0 1))
                    (loop :for i :from 0 :below (length notesets) :do
                        (let ((push-bool-array (gil::add-bool-var-array sp (length push) 0 1)))
                            (loop :for j :from 0 :below (length push) :do
                                (gil::g-rel-reify sp (nth j push) gil::SRT_DISJ (nth i notesets) (nth j push-bool-array))
                            )
                            (gil::g-rel sp gil::BOT_AND push-bool-array (nth i bool-array))
                        )
                    )

                    (gil::g-rel sp gil::BOT_OR bool-array bool2)
                    (gil::g-rel sp bool gil::SRT_EQ 1)
            )

            (let ((bool (gil::add-bool-var sp 0 1)) ; créer le booleen pour la reify
                    (chord (get-chord (chord-quality rock)))  ;if - mode selectionné
                    (offset (- (name-to-note-value (chord-key rock)) 60))
                    (all-notes (gil::add-set-var sp 0 127 0 127))
                    chordset)
                    (gil::g-setunion sp all-notes push)
                    (setq chordset (build-scaleset chord offset))
                    (gil::g-rel sp bool gil::SRT_EQ 1) ;forcer le reify a true dans ce cas
                    (scale-follow-reify sp push chordset bool))
        )
        (if (chord-quality rock)
            (let (chord chordset notesets
                    (bool-array (gil::add-bool-var-array sp 12 0 1)); créer le booleen pour la reify
                    (all-notes (gil::add-set-var sp 0 127 01 127)))
                (gil::g-setunion sp all-notes push)
                (loop :for key :from 0 :below 12 :by 1 :do
                    (let ((bool1 (gil::add-bool-var sp 0 1))
                            (bool2 (gil::add-bool-var sp 0 1))
                            (bool-array-note (gil::add-bool-var-array sp (length notesets) 0 1))
                            chordset notesets)
                        (setq chord (get-chord (chord-quality rock)))
                        (setq chordset (build-scaleset chord key))
                        (setq notesets (build-notesets chord key))

                        (loop :for i :from 0 :below (length notesets) :do
                                (gil::g-rel-reify sp all-notes gil::SRT_DISJ (nth i notesets) (nth i bool-array-note))
                        )
                        (gil::g-rel sp gil::BOT_AND bool-array-note bool1)
                        (scale-follow-reify sp push chordset bool2)
                        (gil::g-op sp (nth key bool-array) gil::BOT_AND bool 0))
                )
                (gil::g-rel sp gil::BOT_OR bool-array 1)
            )
            (let (chord chordset
                    (bool-array (gil::add-bool-var-array sp 12 0 1)))
                (loop :for key :from 0 :below 12 :by 1 :do
                    (setq chord (get-chord (chord-quality rock)))
                    (setq chordset (build-scaleset chord key))
                    (scale-follow-reify sp push chordset (nth key bool-array))
                )
                (gil::g-rel sp gil::BOT_OR bool-array 1)
            )

        )
    )
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; LIMITING MINIMUM NOTE LENGTH ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun note-min-length-rock (sp push pull min-length)
    ;; (floor number divisor) => quotient remainder
    ;; because (length push) == 16, we always have l == min-length
    ;; (setq l (floor (*  (- (length push) 1) min-length) 16))

    (loop :for j :from 0 :below (length push) :by 1 :do
        (loop :for k :from 1 :below min-length :while (< (+ j k) (length pull)) :do
             (gil::g-rel sp (nth (+ j k) pull) gil::SRT_DISJ (nth j push))
        )
    )
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; LIMITING MAXIMUM NOTE LENGTH ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun note-max-length-rock (sp push pull max-length)
    (setq l max-length)
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