#lang racket

(require rackjure/threading)
(require compose-app)
(require megaparsack megaparsack/text)
(require (rename-in data/functor (map fmap)))

(define input
  (file->lines "input"))

(define (try-string/p s)
  (try/p (string/p s)))


(define text-digit/p
  (or/p (fmap (const 1) (try-string/p "one"))
        (fmap (const 2) (try-string/p "two"))
        (fmap (const 3) (try-string/p "three"))
        (fmap (const 4) (try-string/p "four"))
        (fmap (const 5) (try-string/p "five"))
        (fmap (const 6) (try-string/p "six"))
        (fmap (const 7) (try-string/p "seven"))
        (fmap (const 8) (try-string/p "eight"))
        (fmap (const 9) (try-string/p "nine"))))

(define parser/p
  (fmap (curry filter (not .. false?) .. flatten)
        (many/p
         (list/p
          (lookahead/p
           (or/p text-digit/p
                 (fmap (string->number .. string) digit/p)
                 (fmap (const #f) any-char/p)))
          (fmap (const #f) any-char/p)))))

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
