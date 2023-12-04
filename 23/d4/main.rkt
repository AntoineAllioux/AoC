#lang racket

(require rackjure/threading)
(require compose-app)
(require megaparsack megaparsack/text)
(require data/monad)
(require data/applicative)

(struct card (id winning numbers) #:transparent)

(define card/p
  (let ([separator
         (do [_ <- (many/p space/p)]
             [_ <- (string/p "|")]
             (many/p space/p))])
    (do [_ <- (string/p "Card")]
        [_ <- (many+/p space/p)]
        [id <- integer/p]
        [_ <- (char/p #\:)]
        [_ <- (many+/p space/p)]
        [(cons winning _) <- (many-until/p integer/p
                                           #:end (try/p separator)
                                           #:sep (many+/p space/p))]
        [numbers <- (many+/p integer/p #:sep (many+/p space/p))]
        (pure (card id winning numbers)))))

(define cards
  (~>> (file->lines "input")
       (map (parse-result!
             .. curry parse-string card/p))))

(define (sum l)
  (foldl + 0 l))

(define (winning-cards c)
  (match c
    [(card _ winning numbers)
     (filter (λ (x) (member x winning)) numbers)]))

(define (score c)
  (let ([won-cards (winning-cards c)])
    (if (null? won-cards) 0 (expt 2 (sub1 (length won-cards))))))

(define part1
  (~>> cards
       (map score)
       sum))

(define (cards-count cards)
  (let ([total
         (make-immutable-hash
          (map (λ (c) (cons (card-id c) 1)) cards))]
        [aux
         (λ (c acc)
           (let ([n (hash-ref acc (card-id c))]
                 [won-cards (length (winning-cards c))])
             (if (> won-cards 0)
                 (foldl (λ (id acc) (hash-update acc id (λ (x) (+ x n))))
                        acc
                        (range (add1 (card-id c)) (+ (add1 (card-id c)) won-cards)))
                 acc)))])
    (foldl aux total cards)))

(define part2
  (~>> (cards-count cards)
       hash-values
       sum))

part1
part2
