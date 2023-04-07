(in-package :mldz)

(defun link-push-pull-playing (sp push pull playing max-pitch)

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
            (setq temp (gil::add-set-var sp 0 max-pitch 0 1)); temporary variables
            (gil::g-op sp (nth (- j 1) playing) gil::SOT_MINUS (nth j pull) temp); temp[0] = playing[j-1] - pull[j]
            (gil::g-op sp temp gil::SOT_UNION (nth j push) (nth j playing)); playing[j] == playing[j-1] - pull[j] + push[j] Playing note
            (gil::g-rel sp (nth j pull) gil::SRT_SUB (nth (- j 1) playing)) ; pull[j] <= playing[j-1] cannot pull a note not playing
            (gil::g-set-op sp (nth (- j 1) playing) gil::SOT_MINUS (nth j pull) gil::SRT_DISJ (nth j push)); push[j] || playing[j-1] - pull[j] Cannot push a note still playing
        )
    )
)

(defun link-push-push-card (sp push push-card)
    ;compute quardinality of pushed notes
    (loop :for i :from 0 :below (length push-card) :by 1 :do
        (gil::g-card-var sp (nth i push) (nth i push-card))
    )
)

(defun min-pushed-notes-cst (sp push-card min-pushed-notes)
    (loop :for i :from 0 :below (length push-card) :by 1 :do
        (setq b1 (gil::add-bool-var sp 0 1))
        (gil::g-rel-reify sp (nth i push-card) gil::IRT_EQ 0 b1)
        (setq b2 (gil::add-bool-var sp 0 1))
        (gil::g-rel-reify sp (nth i push-card) gil::IRT_GQ min-pushed-notes b2)
        (gil::g-rel sp b1 gil::BOT_OR b2)
    )
)



(defun constrain-srdc-from-parent (srdc-parent push pull playing push-card quant sp)
    ;; call some variant of the post-optional-constraints function to constrain the push pull playing arrays
    ;; from srdc values
    (if (typep srdc-parent 'mldz::a)
        ;; 
        (constrain-srdc-from-A srdc-parent push pull playing push-card quant sp)
        ;; 
        (constrain-srdc-from-B srdc-parent push pull playing push-card quant sp)
    )
)



(defun constrain-srdc-from-A (A-block push pull playing push-card quant sp)
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
         temp-push-s temp-pull-s temp-playing-s temp-push-card-s
         temp-push-r temp-pull-r temp-playing-r temp-push-card-r
         temp-push-d temp-pull-d temp-playing-d temp-push-card-d
         temp-push-c temp-pull-c temp-playing-c temp-push-card-c
         )


        ;; notes in each sub block (s/r/d/c)
        (setq notes-in-subblock (* bars quant))
        ;; sectioning the array into the respective parts for s r d c

        ;; access push pull playing push-card arrays for the section related to s
        ;; (sublst x y z) creates a list based on list x from index y and of z sequential elements
        (setq startidx-s 0)
        (setq temp-push-s (sublst push startidx-s notes-in-subblock))
        (setq temp-pull-s (sublst pull startidx-s notes-in-subblock))
        (setq temp-playing-s (sublst playing startidx-s notes-in-subblock))
        (setq temp-push-card-s (sublst push-card startidx-s notes-in-subblock))

        (setq startidx-r (* bars quant))
        (setq temp-push-r (sublst push startidx-r notes-in-subblock))
        (setq temp-pull-r (sublst pull startidx-r notes-in-subblock))
        (setq temp-playing-r (sublst playing startidx-r notes-in-subblock))
        (setq temp-push-card-r (sublst push-card startidx-r notes-in-subblock))
        
        (setq startidx-d (* 2 (* bars quant)))
        (setq temp-push-d (sublst push startidx-d notes-in-subblock))
        (setq temp-pull-d (sublst pull startidx-d notes-in-subblock))
        (setq temp-playing-d (sublst playing startidx-d notes-in-subblock))
        (setq temp-push-card-d (sublst push-card startidx-d notes-in-subblock))
        
        (setq startidx-c (* 3 (* bars quant)))
        (setq temp-push-c (sublst push startidx-c notes-in-subblock))
        (setq temp-pull-c (sublst pull startidx-c notes-in-subblock))
        (setq temp-playing-c (sublst playing startidx-c notes-in-subblock))
        (setq temp-push-card-c (sublst push-card startidx-c notes-in-subblock))



        ;; set constraints on these arrays from the values saved in the slots of s-block 
        ;; s
        (print "constraining s")
        (constrain-s sp s-block A-block temp-push-s temp-pull-s temp-playing-s temp-push-card-s)

        ;; r

        (print "constraining r")
        (constrain-r sp r-block A-block temp-push-r temp-pull-r temp-playing-r temp-push-card-r 
                                        temp-push-s temp-pull-s temp-playing-s temp-push-card-s)
    
        ;; d

        (print "constraining d")
        (constrain-d sp d-block A-block temp-push-d temp-pull-d temp-playing-d temp-push-card-d)

        ;; c
        
        (print "constraining c")
        (constrain-c sp c-block A-block temp-push-c temp-pull-c temp-playing-c temp-push-card-c)

    )
)

(defun constrain-srdc-from-B (B-block push pull playing push-card quant sp)
    (print "constrain-srdc-from-B")
    ;; for now A and B behave the same way so it suffices to call the function written for A
    ;; when B will have different constraints then this function will have to be changed
    ;; to accomodate this.
    (constrain-srdc-from-A B-block push pull playing push-card quant sp)
)


;; for now these constrain-srdc functions take the parent block as argument in case it comes in handy 
;; when we implement more constraints which could be specified through slots of the parent block
(defun constrain-s (sp s-block s-parent push pull playing push-card)
    (print (min-note-length s-block))
    (post-optional-rock-constraints sp s-block push pull playing push-card)
)

(defun constrain-r (sp r-block r-parent push pull playing push-card
                                        push-s pull-s playing-s push-card-s)

    ;; post optional constraints defined in the rock csp
    (post-optional-rock-constraints sp r-block push pull playing push-card)


    ;; constrain r such that it has a similarity of (similarity-percent-s r-block) with notes played in s-block
    ;; count-var-set-val ? in 'gecode-wrapper.lisp'

    ;; SetVar relation flags
    ;; (defparameter gil::SRT_EQ 0)    ; equality relation
    ;; (defparameter gil::SRT_NQ 1)    ; inequality
    ;; (defparameter gil::SRT_SUB 2)   ; Subset
    ;; (defparameter gil::SRT_SUP 3)   ; Superset
    ;; (defparameter gil::SRT_DISJ 4)  ; Disjoint
    ;; (defparameter gil::SRT_CMPL 5)  ; Complement
    ;; (defparameter gil::SRT_LQ 6)    ; Less or equal
    ;; (defparameter gil::SRT_LE 7)    ; Strictly lower
    ;; (defparameter gil::SRT_GQ 8)    ; Greater or equal
    ;; (defparameter gil::SRT_GR 9)    ; Strictly greater

    (let ((sim (similarity-percent-s r-block))
          notes-in-subblock
          min-sim-note-number)
        (setq notes-in-subblock (length push))
        (setq min-sim-note-number (floor (* notes-in-subblock (/ sim 100.0))))
        ;; TODO: add gil constraint that posts a constraint on playing such that
        ;;       at least min-sim-note-number notes are equal in playing[i] and playing-s[i]
    )


)

(defun constrain-d (sp d-block d-parent push pull playing push-card)
    (post-optional-rock-constraints sp d-block push pull playing push-card)
)

(defun constrain-c (sp c-block c-parent push pull playing push-card)
    (post-optional-rock-constraints sp c-block push pull playing push-card)
)


(defun key-selection-cst (sp push key-selection mode-selection)

    (if mode-selection
        (let (scaleset
                (bool (gil::add-bool-var sp 0 1)) ; créer le booleen pour la reify
                (scale (get-scale mode-selection))  ;if - mode selectionné
                (offset (- (name-to-note-value key-selection) 60)))
                (setq scaleset (build-scaleset scale offset))
                (gil::g-rel sp bool gil::SRT_EQ 1) ;forcer le reify a true dans ce cas
                (scale-follow-reify sp push scaleset bool))
        (let (scaleset
                (bool (gil::add-bool-var sp 0 1)) ; créer le booleen pour la reify
                (scale (get-scale "ionian (major)"))  ;else - pas de mode selectionné => major natural
                (offset (- (name-to-note-value key-selection) 60)))
                (gil::g-rel sp bool gil::SRT_EQ 1) ;forcer le reify a true dans ce cas
                (setq scaleset (build-scaleset scale offset))
                (scale-follow-reify sp push scaleset bool))
    )
    (if mode-selection
        (let ((bool-array (gil::add-bool-var-array sp 12 0 1))) ; créer le booleen pour la reify
            (loop :for key :from 0 :below 12 :by 1 :do
                (setq scale (get-scale mode-selectio))
                (setq scaleset (build-scaleset scale key))
                (scale-follow-reify sp push scaleset (nth key bool-array))
            )
            (gil::g-rel sp gil::BOT_OR bool-array 1)
        )
    )

)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; LIMITING MINIMUM NOTE LENGTH ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun note-min-length-rock (sp push pull min-length)
    (setq l (floor (*  (length push) min-length) 16))
    (print "note-min-length")
    (print l)
    (print (length push))
    (loop :for j :from 0 :below (length push) :by 1 :do
        (loop :for k :from 1 :below l  :while (< (+ j k) (length pull)) :do
             (gil::g-rel sp (nth (+ j k) pull) gil::SRT_DISJ (nth j push))
        )
    )
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; LIMITING MAXIMUM NOTE LENGTH ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun note-max-length-rock (sp push pull max-length)
    (setq l (floor (* (length push) max-length) 16))
    (loop :for j :from 0 :below (+ (- (length push) l) 1) :by 1 :do
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