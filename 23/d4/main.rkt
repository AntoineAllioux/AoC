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
  (match c
    [(card _ winning numbers)
     (let ([won-cards (winning-cards c)])
       (if (null? won-cards) 0 (expt 2 (sub1 (length won-cards)))))]))

(define part1
  (~>> cards
       (map score)
       sum))

(define (cards-count cards)
  (let ([total
         (make-immutable-hash
          (map (match-lambda [(card id winning numbers) (cons id 1)])
               cards))]
        [aux
         (λ (c acc)
           (match c
             [(card id winning numbers)
              (let ([n (hash-ref acc id)]
                    [won-cards (length (winning-cards c))])
                (if
                 (> won-cards 0)
                 (foldl (λ (id acc) (hash-update acc id (λ (x) (+ x n))))
                        acc
                        (range (add1 id) (+ (add1 id) won-cards)))
                 acc))]))])
    (foldl aux total cards)))

(define part2
  (~>> (cards-count cards)
       hash-values
       sum))

part1
part2
