#lang racket

(require compose-app)

(define (char->number x) 
  (- (char->integer x) 48))

(define (number->char x) 
  (integer->char (+ x 48)))

(define (list->number l)
  (string->number (list->string (map number->char l)) 2))

(define parse 
  (map 
    ((curry map char->number) 
      .. string->list)   
    (file->lines "input")))

(define (plus l1 l2)
  (match* (l1 l2)
    [(l1 '()) l1]
    [('() l2) l2]
    [((cons h1 t1) (cons h2 t2)) (cons (+ h1 h2) (plus t1 t2))]))

(define weights
  (match-let* 
    ([l parse]
    [(cons acc n)
      (foldl 
        (match-lambda** 
          [(l1 (cons l2 n))  
            (cons (plus l1 l2) (+ n 1)) ]) 
        (cons (list 0 0 0 0 0 0 0 0 0 0 0 0) 0)
        l)]) 
    (map (λ (x) (if (>= x (/ n 2)) 1 0)) acc)))

(define part1
  (let* 
    ([w weights]
    [gamma (list->number w)]
    [epsilon (list->number (map (λ (x) (if (= x 0) 1 0)) w))])
    (* gamma epsilon)))

(define (weight l n)
  (match-let 
    ([(cons w n) 
        (foldl 
          (λ (x acc) (cons (+ (list-ref x n) (car acc)) (+ (cdr acc) 1))) 
          (cons 0 0) 
          l)]) 
    (if (>= w (/ n 2)) 1 0)))

(define part2
  (letrec
    ([l parse]
    [aux 
      (λ (n l p) 
        (let* 
          ([w (weight l n)] 
          [l2 (filter (curry p n w) l)]) 
          (if (= (length l2) 1) 
            (list-ref l2 0) 
            (aux (+ n 1) l2 p))))]
    [o-rating (aux 0 l (λ (n w x) (= (list-ref x n) w)))]
    [co2-rating (aux 0 l (λ (n w x) (not (= (list-ref x n) w))))]
    [o (list->number o-rating)]
    [co2 (list->number co2-rating)]) 
    (* o co2)))

part1
part2