#lang racket

(define parse 
  (map 
    (lambda (s) 
      (match-let ([(list x y) (string-split s)]) 
        (cons x (string->number y)))) 
    (file->lines "input")))

(define part1 
  (match-let 
    ([(cons x y) 
        (let* ([l parse])  
          (foldl 
            (match-lambda**
              [((cons "forward" n) (cons x y)) 
                (cons (+ x n) y)]
              [((cons "down" n) (cons x y)) 
                (cons x (+ n y))]
              [((cons "up" n) (cons x y)) 
                (cons x (- y n))]) 
              (cons 0 0) 
              l))]) 
    (* x y)))

(define part2 
  (match-let 
    ([(list x y a) 
        (let* ([l parse])  
          (foldl 
            (match-lambda**
              [((cons "forward" n) (list x y a)) 
                (list (+ x n) (+ y (* a n)) a)]
              [((cons "down" n) (list x y a)) 
                (list x y (+ a n))]
              [((cons "up" n) (list x y a)) 
                (list x y (- a n))]) 
              (list 0 0 0) 
              l))]) 
    (* x y)))

part1
part2