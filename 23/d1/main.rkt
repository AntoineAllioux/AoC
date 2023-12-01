#lang racket

(require rackjure/threading)
(require compose-app)
(require megaparsack megaparsack/text)
(require data/applicative
         (rename-in data/functor (map fmap))
         data/monad)

(define input
  (file->lines "input"))

(define (try-string/p s)
  (try/p (string/p s)))

(define text-digit/p
  (or/p (chain (const (pure 1)) (try-string/p "one"))
        (chain (const (pure 2)) (try-string/p "two"))
        (chain (const (pure 3)) (try-string/p "three"))
        (chain (const (pure 4)) (try-string/p "four"))
        (chain (const (pure 5)) (try-string/p "five"))
        (chain (const (pure 6)) (try-string/p "six"))
        (chain (const (pure 7)) (try-string/p "seven"))
        (chain (const (pure 8)) (try-string/p "eight"))
        (chain (const (pure 9)) (try-string/p "nine"))))

(define parser/p
  (fmap (curry filter (not .. false?) .. flatten)
        (many/p
         (list/p
          (lookahead/p
           (or/p text-digit/p
                 (fmap (string->number .. string) digit/p)
                 (chain (const (pure #f)) any-char/p)))
          (chain (const (pure #f)) any-char/p)))))

(define (sum l)
  (foldl + 0 l))

(define part1
  (~>> input
       (map (string->number
             .. string-append*
             .. (λ (x) (list (first x) (last x)))
             .. (curry regexp-match* #rx"[0-9]")))
       sum))

(define part2
  (let* ([list->calib
          (λ (l)
            (string->number
             (string-append (number->string (first l))
                            (number->string (last l)))))])
    (~>> input
         (map (list->calib
               .. parse-result!
               .. (curry parse-string parser/p)))
         sum)))

part1
part2
