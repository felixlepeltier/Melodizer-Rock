(in-package :mldz)

(defun link-push-pull-playing-set (sp push pull playing max-pitch max-simultaneous-notes)
    (print "link-push-pull-playing-set")
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

    ;connect push, pull and playing
    (loop :for j :from 1 :below (length push) :do ;for each interval
        (let (temp z c)
            (setq temp (gil::add-set-var sp 0 max-pitch 0 max-simultaneous-notes)); temporary variables
            (gil::g-op sp (nth (- j 1) playing) gil::SOT_MINUS (nth j pull) temp); temp[0] = playing[j-1] - pull[j]
            (gil::g-op sp temp gil::SOT_UNION (nth j push) (nth j playing)); playing[j] == playing[j-1] - pull[j] + push[j] Playing note
            (gil::g-rel sp (nth j pull) gil::SRT_SUB (nth (- j 1) playing)) ; pull[j] <= playing[j-1] cannot pull a note not playing
            (gil::g-set-op sp (nth (- j 1) playing) gil::SOT_MINUS (nth j pull) gil::SRT_DISJ (nth j push)); push[j] || playing[j-1] - pull[j] Cannot push a note still playing
            ;; (gil::g-rel sp (nth j pull) gil::SRT_DISJ (nth j push))
        )
    )
)

(defun link-push-pull-playing-int (sp push pull playing max-pitch)
    (print "start link-push-pull-playing")
    ;initial constraint on pull, push, playing and durations
    (gil::g-rel sp (first pull) gil::IRT_EQ -1) ; pull[0] == empty
    (gil::g-rel sp (first push) gil::IRT_EQ (first playing)) ; push[0] == playing [0]

    ;connect push, pull and playing
    (loop :for j :from 1 :below (length push) :do ;for each interval
        (let (
            playing-j-playing-j-one
            push-j-pull-j
            push-j-playing-j
            pull-j-playing-j-one
            pull-j-one
            pull-j-nq-one
            push-j-one
            push-j-nq-one
            playing-j-one
            playing-j-nq-one
            pull-j-nq-playing-j-one
            playing-j-nq-playing-j-one
            pull-j-nq-playing-j-one
            playing-j-nq-playing-j-one
            )
            (setq 
                playing-j-playing-j-one (gil::add-bool-var-expr sp (nth j playing) gil::IRT_EQ (nth (- j 1) playing))
                push-j-pull-j (gil::add-bool-var-expr sp (nth j push) gil::IRT_EQ (nth j pull))
                push-j-playing-j (gil::add-bool-var-expr sp (nth j push) gil::IRT_EQ (nth j playing))
                pull-j-playing-j-one (gil::add-bool-var-expr sp (nth j pull) gil::IRT_EQ (nth (- j 1) playing))
                pull-j-one (gil::add-bool-var-expr sp (nth j pull) gil::IRT_EQ -1)
                pull-j-nq-one (gil::add-bool-var-expr sp (nth j pull) gil::IRT_NQ -1)
                push-j-one (gil::add-bool-var-expr sp (nth j push) gil::IRT_EQ -1)
                push-j-nq-one (gil::add-bool-var-expr sp (nth j push) gil::IRT_NQ -1)
                playing-j-one (gil::add-bool-var-expr sp (nth j playing) gil::IRT_EQ -1)
                playing-j-nq-one (gil::add-bool-var-expr sp (nth j playing) gil::IRT_NQ -1)
                pull-j-nq-playing-j-one (gil::add-bool-var-expr sp (nth (- j 1) playing) gil::IRT_NQ (nth j pull))
                playing-j-nq-playing-j-one (gil::add-bool-var-expr sp (nth (- j 1) playing) gil::IRT_NQ (nth j playing))
            )

            (gil::g-op sp playing-j-playing-j-one gil::BOT_OR push-j-playing-j 1)
            (gil::g-op sp push-j-playing-j gil::BOT_OR push-j-one 1)
            (gil::g-op sp pull-j-playing-j-one gil::BOT_OR pull-j-one 1)

            (gil::g-op sp push-j-nq-one gil::BOT_IMP pull-j-playing-j-one 1)

            ;; Playing note
            ;; Playing[j] can only be equal to the preceding played note or a new pushed note
            (gil::g-op sp playing-j-playing-j-one gil::BOT_OR push-j-playing-j 1)
            ;; No note playing implies no note pushed and previous note pulled
            (gil::g-op sp playing-j-one gil::BOT_IMP push-j-one 1)
            (gil::g-op sp playing-j-one gil::BOT_IMP pull-j-playing-j-one 1)
            ;; Same note playing implies push[j] = pull[j] <=> playing[j] = playing[j-1]
            (gil::g-op sp playing-j-playing-j-one gil::BOT_IMP push-j-pull-j 1)
            (gil::g-op sp push-j-pull-j gil::BOT_IMP playing-j-playing-j-one 1)
            

            ;; Pulled note
            ;; A note can be pulled only if it was previously playing
            ;; (pull[j] = playing[j-1] OR pull[j] = -1) = 1
            (gil::g-op sp pull-j-playing-j-one gil::BOT_OR pull-j-one 1)
            ;; (gil::g-op sp pull-j-playing-j-one gil::BOT_IMP push-j-playing-j 1)
            ;; (gil::g-op sp push-j-playing-j gil::BOT_IMP pull-j-playing-j-one 1)
            

            ;; Pushed note
            ;; A note is pushed when a note is playing and the previous note was pulled
            ;; push[j] /= -1 if
            ;; playing[j] /= -1 AND pull[j] /= -1
            ;; (gil::g-op sp playing-j-nq-one gil::BOT_AND pull-j-nq-one push-j-playing-j)
            ;; push[j] can only equal the current note playing or -1
            (gil::g-op sp push-j-playing-j gil::BOT_OR push-j-one 1)
            (gil::g-op sp push-j-nq-one gil::BOT_IMP pull-j-playing-j-one 1)
        
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

        (setq startidx-r notes-in-subblock)
        (setq temp-push-r (sublst push startidx-r notes-in-subblock))
        (setq temp-pull-r (sublst pull startidx-r notes-in-subblock))
        (setq temp-playing-r (sublst playing startidx-r notes-in-subblock))
        (setq temp-push-r-acc (sublst push-acc startidx-r notes-in-subblock))
        (setq temp-pull-r-acc (sublst pull-acc startidx-r notes-in-subblock))
        (setq temp-playing-r-acc (sublst playing-acc startidx-r notes-in-subblock))
        
        (setq startidx-d (+ startidx-r notes-in-subblock))
        (setq temp-push-d (sublst push startidx-d notes-in-subblock))
        (setq temp-pull-d (sublst pull startidx-d notes-in-subblock))
        (setq temp-playing-d (sublst playing startidx-d notes-in-subblock))
        (gil::g-rel sp (nth 0 temp-pull-d) gil::IRT_EQ (nth (- startidx-d 1) playing)) ; pull[0]=playing[previous]
        (setq temp-push-d-acc (sublst push-acc startidx-d notes-in-subblock))
        (setq temp-pull-d-acc (sublst pull-acc startidx-d notes-in-subblock))
        (setq temp-playing-d-acc (sublst playing-acc startidx-d notes-in-subblock))
        
        (setq startidx-c (+ startidx-d notes-in-subblock))
        (gil::g-rel sp (nth startidx-c pull) gil::IRT_EQ (nth (- startidx-c 1) playing)) ; pull[0]=playing[previous]
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
    (gil::g-rel sp (first pull) gil::IRT_EQ -1) ; pull[0] == empty
    
    ;; ;; if a source melody is given, then use it to generate push pull playing 
    ;; ;; then constrain the push pull and playing of s to be equal to these arrays

    (if (= (block-position s-parent) 0)
        ;; then we're in the first block of the global structure and the 
        ;; s subblock needs to correspond to the source melody
        ;; (   ;; then a source melody has been passed as argument and its value != nil
        (if (melody-source (parent s-parent))
            (let (push-source pull-source playing-source ppp-source
                var-push-source var-pull-source var-playing-source
                )
                (print "Setting the first s block to the source melody")
                (setq ppp-source (create-push-pull-int (melody-source (parent s-parent)) 16))

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

                (loop :for i :from 0 :below (length push-source) :by 1 :do
                    (gil::g-rel sp (nth i push) gil::IRT_EQ (nth i push-source))
                )
                (loop :for i :from 0 :below (- (length pull-source) 1) :by 1 :do
                    (gil::g-rel sp (nth i pull) gil::IRT_EQ (nth i pull-source))
                )
                (loop :for i :from 0 :below (length playing-source) :by 1 :do
                    (gil::g-rel sp (nth i playing) gil::IRT_EQ (nth i playing-source))
                )

                (print "First s block has been set to the source melody")


            
            )
            (post-optional-rock-constraints sp s-block push pull playing)
        )
        ;; )
        (post-optional-rock-constraints sp s-block push pull playing)
    )
    
    ;; ;; accompaniment
    (post-optional-rock-constraints sp (accomp s-block) push-acc pull-acc playing-acc)
)

(defun constrain-r (sp r-block r-parent push pull playing push-acc pull-acc playing-acc
                                        push-s pull-s playing-s max-pitch max-simultaneous-notes)

    (gil::g-rel sp (first pull) gil::IRT_EQ (nth (- (length playing-s) 1) playing-s)) ; pull[0]=playing-s[quant-1]

    ;; post optional constraints defined in the rock csp
    ;; dont constrain if source melody given

    (post-optional-rock-constraints sp r-block push pull playing)
    (post-optional-rock-constraints sp (accomp r-block) push-acc pull-acc playing-acc)

    ;; constrain r such that it has a similarity of (similarity-percent-s r-block) with notes played in s-block
    (let ((sim (similarity-percent-s r-block)))
        (cst-common-vars sp push-s push sim)
        ;; (cst-common-vars sp pull-s pull sim)
        ;; (cst-common-vars sp playing-s playing sim)
    )
)

(defun constrain-d (sp d-block d-parent push pull playing push-acc pull-acc playing-acc max-pitch max-simultaneous-notes)
    ;; (gil::g-rel sp (first pull) gil::IRT_EQ -1) ; pull[0] == empty
    (post-optional-rock-constraints sp d-block push pull playing)
    (post-optional-rock-constraints sp (accomp d-block) push-acc pull-acc playing-acc)
)

(defun constrain-c (sp c-block c-parent push pull playing push-acc pull-acc playing-acc max-pitch max-simultaneous-notes)
    ;; (gil::g-rel sp (first pull) gil::IRT_EQ -1) ; pull[0] == empty
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
    (print "chord-key-cst")
    (if (chord-key rock)
        (if (chord-quality rock)
            (let ((bool (gil::add-bool-var sp 0 1)) ; créer le booleen pour la reify
                    (bool2 (gil::add-bool-var sp 0 1))
                    (chord (get-chord (chord-quality rock)))  ;if - mode selectionné
                    (offset (- (name-to-note-value (chord-key rock)) 60))
                    (all-notes (gil::add-set-var sp 0 127 0 127))
                    chordset notesets bool-array)
                    (setq chordset (build-scaleset chord offset))
                    (print chordset)
                    (scale-follow-reify sp push chordset bool)
                    (setq notesets (build-notesets chord offset))
                    (print notesets)
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

(defun scale-follow-reify-int (sp push chordset bool)
    (setq r (gil::add-bool-var-array sp (length push) 0 1))
    (loop :for j :from 0 :below (length push) :do
        (let ((push-bool-array (gil::add-bool-var-array sp (length chordset) 0 1)))
            (loop :for i :from 0 :below (length chordset) :do
                (gil::g-rel-reify sp (nth j push) gil::IRT_EQ (nth i chordset) (nth i push-bool-array))
            )
            (gil::g-rel sp gil::BOT_OR push-bool-array (nth j r))
        )
    )
    (gil::g-rel sp gil::BOT_AND r bool)
)

(defun chord-key-cst-int (sp push rock)
    ;; (let ((bool (gil::add-bool-var sp 0 1)) ; créer le booleen pour la reify
    ;;     (bool2 (gil::add-bool-var sp 0 1))
    ;;     (chord (get-chord (chord-quality rock)))  ;if - mode selectionné
    ;;     (offset (- (name-to-note-value (chord-key rock)) 60))
    ;;     (all-notes (gil::add-set-var sp 0 127 0 127))
    ;;     chordset notesets bool-array)
    ;;     (setq chordset (append '(-1) (build-scaleset chord offset)))
    ;;     (print chordset)
    ;;     (scale-follow-reify-int sp push chordset bool)
    ;;     (gil::g-rel sp bool gil::IRT_EQ 1)
    ;; )
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; LIMITING MINIMUM NOTE LENGTH ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun note-min-length-rock (sp push pull min-length)
    (loop :for j :from 0 :below (length push) :by 1 :do
        (loop :for k :from 1 :below min-length :by 1 :while (< (+ j k) (length pull)) :do
            (if (typep (nth j push) 'gil::int-var)
                (let (bool-temp bool2 bool3 bool4)
                    (setq bool-temp (gil::add-bool-var-expr sp (nth j push) gil::IRT_NQ -1))
                    (setq bool3 (gil::add-bool-var-expr sp (nth (+ j k) pull) gil::IRT_EQ -1))
                    (gil::g-op sp bool-temp gil::BOT_IMP bool3 1)
                    
                )
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

(defun limit-intervals-cst (sp playing)
    (let ((max-interval 7) (augmented-intervals '(1 6)))
        (loop :for i :from 1 :below (length playing) :do
            (let (bool-interval-max interval interval-abs bool-pi bool-pi-one bool)
                (setq bool-pi (gil::add-bool-var-expr sp (nth i playing) gil::IRT_EQ -1))
                (setq bool-pi-one (gil::add-bool-var-expr sp (nth (- i 1) playing) gil::IRT_EQ -1))

                ;; Define the interval between the two notes
                ;; interval = |playing[i] - playing[i-1]|
                (setq interval (gil::add-int-var-expr sp (nth i playing) gil::IOP_SUB (nth (- i 1) playing)))
                (setq interval-abs (gil::add-int-var sp 0 127))
                (gil::g-abs sp interval interval-abs)

                ;; The maximum interval
                ;; interval <= 7 (perfect fifth)
                (setq bool-interval-max (gil::add-bool-var-expr sp interval-abs gil::IRT_LQ max-interval))

                (setq bool (gil::add-bool-var sp 0 1))
                (gil::g-op sp bool-pi gil::BOT_OR bool-pi-one bool)
                (gil::g-op sp bool gil::BOT_OR bool-interval-max 1)
                ;; (gil::g-rel sp bool-interval-max gil::IRT_EQ 1)
            )
        )
    )
)