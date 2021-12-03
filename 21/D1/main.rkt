#lang racket

(define parse
  (map string->number (file->lines "input")))

(define part1 
  (let* ([l parse]
         [prev (list-ref l 0)])  
    (foldl 
      (match-lambda** 
        [(x (cons n prev)) 
            (if (> x prev) (cons (+ n 1) x) (cons n x))]) 
      (cons 0 prev) 
      (list-tail l 0))))

(define part2
  (let* ([l parse]
         [prev (list (list-ref l 0) (list-ref l 1) (list-ref l 2))])  
    (foldl 
      (match-lambda** 
        [(x (list n (list prev1 prev2 prev3))) 
          (if (> (+ x prev1 prev2) (+ prev1 prev2 prev3)) 
            (list (+ n 1) (list x prev1 prev2)) 
            (list n (list x prev1 prev2)))]) 
      (list 0 prev) 
      (list-tail l 2))))

part1
part2