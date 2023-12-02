#lang racket

(require rackjure/threading)
(require compose-app)
(require megaparsack megaparsack/text)
(require (rename-in data/functor (map fmap)))
(require data/monad)
(require data/applicative)

(struct game (id sets) #:transparent)

(define (get-set-colour set colour)
  (hash-ref set colour (λ () 0)))

(define alpha-string/p
  (fmap list->string (many/p letter/p)))

(define set/p
  (do [_ <- (char/p #\space)]
      [n <- integer/p]
      [_ <- (char/p #\space)]
      [c <- (fmap string->symbol alpha-string/p)]
      (pure (cons c n))))

(define sets/p
  (do [assoc <- (many/p set/p #:sep (char/p #\,))]
    (pure (make-hash assoc))))

(define game/p
  (do [_ <- (string/p "Game ")]
      [id <- integer/p]
      [_ <- (char/p #\:)]
      [sets <- (many/p sets/p #:sep (char/p #\;)) ]
      (pure (game id sets))))

(define games
  (~>> (file->lines "input")
       (map (parse-result!
             .. curry parse-string game/p))))

(define (sum l)
  (foldl + 0 l))

(define part1
  (let* ([set-valid?
          (λ (set)
            (and (<= (get-set-colour set 'red) 12)
                 (<= (get-set-colour set 'green) 13)
                 (<= (get-set-colour set 'blue) 14)))]
         [game-valid?
          (λ (game)
            (foldl (λ (set acc) (and acc (set-valid? set))) #t (game-sets game)))])
    (~>> games
         (filter game-valid?)
         (map (λ (game) (game-id game)))
         sum)))

(define part2
  (let* ([min-colour
          (λ (sets colour)
            (foldl (λ (set acc) (max acc (get-set-colour set colour))) 0 sets))]
         [min-power
          (match-lambda
            [(game _ sets)
             (* (min-colour sets 'red)
                (min-colour sets 'green)
                (min-colour sets 'blue))])])
    (~>> games
         (map min-power)
         sum)))

part1
part2

