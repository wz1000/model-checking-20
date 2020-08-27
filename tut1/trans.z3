; T(x1 x0,y1 y0) : (y0 <=> x1) /\ ~y1

(define-fun T ((x1 Bool) (x0 Bool) (y1 Bool) (y0 Bool)) (Bool)
  (and (= x1 y0) (not y1)))

(declare-const x0 Bool)
(declare-const x1 Bool)
(declare-const y0 Bool)
(declare-const y1 Bool)

; (assert (and x0 (T x1 x0 y1 y0)))
; 
; (check-sat)
; (get-model)

; (assert (not (and (= x0 true) (= x1 false) (= y1 false) (= y0 false))))
; (check-sat)
; (get-model)
; 
; (assert (not (and (= x0 true) (= x1 true) (= y1 false) (= y0 true))))
; (check-sat)

; Two steps:

; (assert (exists ((z1 Bool) (z0 Bool))
;           (and (T x1 x0 z1 z0)
;                (T z1 z0 y1 y0))))
; (check-sat)
; (get-model)
; 
; (assert y1)
; (check-sat)
