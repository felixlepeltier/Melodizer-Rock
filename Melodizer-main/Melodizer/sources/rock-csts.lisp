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