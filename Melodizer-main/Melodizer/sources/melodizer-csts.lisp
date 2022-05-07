(in-package :mldz)

;;;;;;;;;;;;;;;;;;;;;
; FOLLOWING A SCALE ;
;;;;;;;;;;;;;;;;;;;;;

(defun scale-follow (sp push scaleset)
    (loop :for j :from 0 :below (length push) :do
            (gil::g-rel sp (nth j push) gil::SRT_SUB scaleset)
    )
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; FOLLOWING A SCALE WITH REIFICATION ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun scale-follow-reify (sp push scaleset reify)
    (loop :for j :from 0 :below (length push) :do
            (gil::g-rel-reify sp (nth j push) gil::SRT_SUB scaleset reify)
    )
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; FOLLOWING A CHORD PROGRESSION ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun chordprog-follow (sp push chordset progsize)
    (loop :for j :from 0 :below (length chordset) :by 1 :do
        (loop :for k :from 0 :below (/ (length push) progsize) :by 1 :do
            (gil::g-rel sp (nth (+ k (/ (* (length push) j) progsize)) push) gil::SRT_SUB (nth j chordset))
        )
    )
)

;;;;;;;;;;;;;;;;;;;;;;;;
; LIMITING PITCH RANGE ;
;;;;;;;;;;;;;;;;;;;;;;;;

(defun pitch-range (sp push min-pitch max-pitch)
    (loop :for j :below (length push) :by 1 :do
        (gil::g-dom-ints sp (nth j push) gil::SRT_SUB min-pitch max-pitch)
    )
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; LIMITING MINIMUM NOTE LENGTH ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun note-min-length (sp push pull min-length)
    (loop :for j :from 0 :below (length push) :by 1 :do
        (loop :for k :from 1 :below min-length :while (< (+ j k) (length pull)) :do
             (gil::g-rel sp (nth (+ j k) pull) gil::SRT_DISJ (nth j push))
        )
    )
)

;;;;;;;;;;;;;;;;;;;;;;;;;
; DEFINING CHORD RHYTHM ;
;;;;;;;;;;;;;;;;;;;;;;;;;

(defun chords-rhythm (sp push chord-rhythm chord-size-min chord-size-max)
    (loop :for j :from 0 :below (length push) :by 1 :do
        (if (= (mod j chord-rhythm) 0)
             (gil::g-card sp (nth j push) chord-size-min chord-size-max)
             (gil::g-card sp (nth j push) 0 1)
        )
    )
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; LIMITING MINIMUM CHORD LENGTH ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun chords-length (sp push pull chord-rhythm chord-min-length)
    (loop :for j :from 0 :below (length push) :by 1 :do
         (if (= (mod j chord-rhythm) 0)
             (loop :for k :from 1 :below chord-min-length :while (< (+ j k) (length pull)) :do
                 (gil::g-rel sp (nth (+ j k) pull) gil::SRT_DISJ (nth j push))
             )
         )
    )
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; LIMIT NUMBER OF ADDED NOTE ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun num-added-note (sp playing min-card max-card)
    (gil::g-card sp playing min-card max-card)
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; SETS QUANTIFICATION FOR PUSH ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun set-quantification (sp push pull quantification)
    (setq q (/ 192 (get-quant quantification)))
    (loop :for j :from 0 :below (length push) :by 1 :do
         (if (/= (mod j q) 0)
            (gil::g-empty sp (nth j push))
         )
    )
)

;;;;;;;;;;;;;;;;;;;
; PITCH DIRECTION ;
;;;;;;;;;;;;;;;;;;;

(defun increasing-pitch (sp playing isPlayed)
    (loop :for i :from 0 :below (- (length playing) 1) :by 1 :do
          (loop :for j :from (+ i 1) :below (length playing) :by 1 :do
                (let ((tempBool (gil::add-bool-var sp 0 1)))
                     (gil::g-op sp (nth i isPlayed) gil::BOT_AND (nth j isPlayed) tempBool)
                     (gil::g-rel-reify sp (gil::g-setmin sp (nth i playing)) gil::IRT_LQ (gil::g-setmin sp (nth j playing)) tempBool gil::RM_IMP)
                )
          )
    )
)

(defun decreasing-pitch (sp playing isPlayed)
    (loop :for i :from 0 :below (- (length playing) 1) :by 1 :do
          (loop :for j :from (+ i 1) :below (length playing) :by 1 :do
                (let ((tempBool (gil::add-bool-var sp 0 1)))
                     (gil::g-op sp (nth i isPlayed) gil::BOT_AND (nth j isPlayed) tempBool)
                     (gil::g-rel-reify sp (gil::g-setmax sp (nth i playing)) gil::IRT_GQ (gil::g-setmax sp (nth j playing)) tempBool gil::RM_IMP)
                )
          )
    )
)










