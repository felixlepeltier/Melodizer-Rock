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

;; ;; rock-csp : 
;; (defun constrain-ppp-from-srdc (srdc-parent push pull playing)
;;     ;; call some variant of the post-optional-constraints function to constrain the push pull playing arrays
;;     ;; from srdc values
;; )


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

    ;; s
    (let ((bars (bar-length (s-block A-block)))
         (s-block (s-block A-block))
         notes-in-subblock startidx
         temp-push temp-pull temp-playing temp-push-card)

        (print "constraining s")

        (setq notes-in-subblock (* bars quant))
        ;; access push pull playing push-card arrays for the section related to s
        ;; (sublst x y z) creates a list based on list x from index y and of z sequential elements
        (setq startidx 0)
        (setq temp-push (sublst push startidx notes-in-subblock))
        (setq temp-pull (sublst pull startidx notes-in-subblock))
        (setq temp-playing (sublst playing startidx notes-in-subblock))
        (setq temp-push-card (sublst push-card startidx notes-in-subblock))

        ;; set constraints on these arrays from the values saved in the slots of s-block 
        (post-optional-rock-constraints sp s-block temp-push temp-pull temp-playing temp-push-card)

    )
    ;; r
    (let ((bars (bar-length (r-block A-block)))
         (r-block (r-block A-block))
         notes-in-subblock startidx
         temp-push temp-pull temp-playing temp-push-card)
         
        (print "constraining r")

        (setq notes-in-subblock (* bars quant))

        (setq startidx (* bars quant))
        (setq temp-push (sublst push startidx notes-in-subblock))
        (setq temp-pull (sublst pull startidx notes-in-subblock))
        (setq temp-playing (sublst playing startidx notes-in-subblock))
        (setq temp-push-card (sublst push-card startidx notes-in-subblock))
        
        (post-optional-rock-constraints sp r-block temp-push temp-pull temp-playing temp-push-card)

    )
    ;; d
    (let ((bars (bar-length (d-block A-block)))
         (d-block (d-block A-block))
         notes-in-subblock startidx
         temp-push temp-pull temp-playing temp-push-card)
         
        (print "constraining d")

        (setq notes-in-subblock (* bars quant))

        (setq startidx (* 2 (* bars quant)))
        (setq temp-push (sublst push startidx notes-in-subblock))
        (setq temp-pull (sublst pull startidx notes-in-subblock))
        (setq temp-playing (sublst playing startidx notes-in-subblock))
        (setq temp-push-card (sublst push-card startidx notes-in-subblock))

        (post-optional-rock-constraints sp d-block temp-push temp-pull temp-playing temp-push-card)

    )
    ;; c
    (let ((bars (bar-length (c-block A-block)))
         (c-block (c-block A-block))
         notes-in-subblock startidx
         temp-push temp-pull temp-playing temp-push-card)
         
        (print "constraining c")
        
        (setq notes-in-subblock (* bars quant))

        (setq startidx (* 3 (* bars quant)))
        (setq temp-push (sublst push startidx notes-in-subblock))
        (setq temp-pull (sublst pull startidx notes-in-subblock))
        (setq temp-playing (sublst playing startidx notes-in-subblock))
        (setq temp-push-card (sublst push-card startidx notes-in-subblock))

        (post-optional-rock-constraints sp c-block temp-push temp-pull temp-playing temp-push-card)

    )
)

(defun constrain-srdc-from-B (B-block push pull playing push-card quant sp)
    (print "constrain-srdc-from-B")
    ;; for now A and B behave the same way so it suffices to call the function written for A
    ;; when B will have different constraints then this function will have to be changed
    ;; to accomodate this.
    (constrain-srdc-from-A B-block push pull playing push-card quant sp)
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