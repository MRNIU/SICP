#lang racket
(define (pascal-triangle row col)
  (cond ((or (= row 0) (= col 0) (> col row)) 'error)
        ((or (= col 1) (= col row)) 1)        
        (else
         (+
          (pascal-triangle (- row 1) (- col 1))
          (pascal-triangle (- row 1) col)))))