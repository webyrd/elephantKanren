(load "mk.scm")

;;; Terminology
;;;
;;; fresh body ('fb'):
;;; (lambda (loe lor) ge)    loe [input] = list of events we've seen up to now
;;;                     lor [output] = list of results
;;;                     ge = goal expression
;;;

;;; Assumptions:
;;;
;;; * we assume that we reify varible 1

;;; Might be able to use tabling of the 'loe' to get incremental
;;; solving for "free"

;;; list of events
(define *loe* '())

;;; list of fresh bodies
;;; entries are pairs: (fb . side-effect-proc) 
(define *fbs* '())

(define handle-event
  (lambda (event)
    (set! *loe* (cons event *loe*))
    (for-each
      (lambda (fb/proc)
        (let ((fb (car fb/proc))
              (proc (cdr fb/proc)))
          (let ((goal (kernel fb)))
            (let ((ans (run 1 (dummy) goal)))
              (proc ans)))))
      *fbs*)))

(define kernel
  (lambda (fb)
    (fresh (lor)
      (fb *loe* lor))))

(begin
  (let ((fb (lambda (loe lor)
              (fresh (a rest)
                (== `(,a ,a ,a . ,rest) loe)
                (== 'LINE lor))))
        (proc (lambda (ans)
                (if (null? ans)
                    (printf "No soup tonight!\n")
                    (printf "Found a turtle track!\n")))))
    (set! *fbs* (list (cons fb proc)))) 
  (handle-event 'FORWARD)
  (handle-event 'FORWARD)
  (handle-event 'FORWARD)
  (handle-event 'FORWARD)
  (handle-event 'BACKWARD))

(printf "\ntest 2\n\n")

(begin
  (let ((fb (lambda (loe lor)
              (fresh (a rest)
                (== `(,a ,a ,a . ,rest) loe)
                (== 'LINE lor))))
        (proc (lambda (ans)
                (if (null? ans)
                    (printf "No soup tonight!\n")
                    (printf "Found a turtle track!\n")))))
    (set! *fbs* (list (cons fb proc)))) 
  (handle-event 'FORWARD)
  (handle-event 'FORWARD)
  (handle-event 'FORWARD)
  (handle-event 'FORWARD)
  (handle-event 'BACKWARD)
  (handle-event 'BACKWARD)
  (handle-event 'BACKWARD)
  (handle-event 'FORWARD))
