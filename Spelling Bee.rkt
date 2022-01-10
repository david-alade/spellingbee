;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname |Spelling Bee|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/universe)
(require 2htdp/image)
(require 2htdp/batch-io)

(define-struct letters [required available])

;; A Letters is a: (make-letters 1String [List-of 1String])
;; Interpretation: Letters to be used in Spelling Bee game
;; - required: required in every words
;; - available: letters that can be used in words
;; Examples:

(define LETTERS-1 (make-letters "a" (list "b" "c" "d" "e" "f" "g")))
(define LETTERS-2 (make-letters "r" (list "g" "f" "m" "z" "x" "a")))
(define LETTERS-3 (make-letters "n" (list "r" "h" "n" "x" "t" "h")))
(define TEST-LETTERS (make-letters "n" (list "e" "g" "h" "o" "t" "u")))
(define TEST-LETTERS-2 (make-letters "c" (list "a" "d" "e" "t" "v" "x"))) 

(define (letters-temp l)
  (... (letters-required l) ...
       (lo1s-temp (letters-available l)) ...))

(define-struct world [word-ip letters so-far score])
;; A World is a (make-world String Letters [List-of String] Number)
;; Interpretation: Contains words found by the user and the letters available
;; - word-ip: word in progess
;; - letters the word is constructed from
;; - so-far: words that have been found so far
;; - score: score the player has 
;; Examples:

(define WORLD-1 (make-world "" LETTERS-1 (list "dead" "cdeaf") 3))
(define WORLD-2 (make-world "g" LETTERS-2 (list "grfm" "garfm") 3))
(define WORLD-3 (make-world "nr" LETTERS-3 (list "nxth" "rnxth") 3))
(define TEST-WORLD (make-world "" TEST-LETTERS '() 0))
(define TEST-WORLD-2 (make-world "" TEST-LETTERS-2 '() 0))
(define REAL-WORLD (make-world "toughen" TEST-LETTERS (list "noon") 0))
(define REAL-WORLD-2 (make-world "noon" TEST-LETTERS (list "toughen") 0))
(define REAL-WORLD-3 (make-world "excavated" TEST-LETTERS-2 (list "excavated") 0))
(define REAL-WORLD-4 (make-world "exceeded" TEST-LETTERS-2 (list "excavated") 0))

(define (world-temp w)
  (... (world-word-ip w) ...
       (letters-temp (world-letters w)) ...
       (los-temp (world-so-far w))
       (world-score w) ...))

(define LETTERS-4 (make-letters "a" (list "b" "c" "d" "e")))
(define LETTERS-5 (make-letters "r" (list "g" "f" "m" "z" "x")))
(define LETTERS-6 (make-letters "n" (list "r" "h" "n" "x" "t" "h")))

(check-expect (letters->image LETTERS-4)
              (beside
               (text "a" 24 "Red")
               (text "b" 24 "Black")
               (text "c" 24 "Black")
               (text "d" 24 "Black")
               (text "e" 24 "Black")))

(check-expect (letters->image LETTERS-5)
              (beside
               (text "r" 24 "Red")
               (text "g" 24 "Black")
               (text "f" 24 "Black")
               (text "m" 24 "Black")
               (text "z" 24 "Black")
               (text "x" 24 "Black")))

(check-expect (letters->image LETTERS-6)
              (beside
               (text "n" 24 "Red")
               (text "r" 24 "Black") 
               (text "h" 24 "Black")
               (text "n" 24 "Black")
               (text "x" 24 "Black")
               (text "t" 24 "Black")
               (text "h" 24 "Black")))

;; letters->image: Letters -> Image
;; Turns letters into an image

(define (letters->image l)
  (beside
   (text (letters-required l) 24 "red")
   (row (letters-available l))))

;; row: [NEList-of 1String] -> Image
;; puts the available letters in a row

(check-expect (row (list "b" "c" "d" "e"))
              (beside
               (text "b" 24 "black")
               (text "c" 24 "black")
               (text "d" 24 "black")
               (text "e" 24 "black")))

(check-expect (row (list "g" "f" "m" "z" "x"))
              (beside
               (text "g" 24 "black")
               (text "f" 24 "black")
               (text "m" 24 "black")
               (text "z" 24 "black")
               (text "x" 24 "black")))

(define (row lo1s)
  (foldr beside empty-image (map word->image lo1s)))

;; world->image : World -> Image
;; Display a World.

(define (world->image w)
  (overlay
   (above
    (word->image (number->string (world-score w)))
    (text "How many words can you construct" 14 "red")
    (beside/align
     "top"
     (above
      (letters->image (world-letters w))
      (word->image (world-word-ip w)))
     (column (world-so-far w))))
   (empty-scene 800 600)))

(check-expect (world->image WORLD-1)             
              (overlay
               (above
                (word->image (number->string 3))
                (text "How many words can you construct" 14 "red")
                (beside/align
                 "top"
                 (above
                  (letters->image LETTERS-1)
                  (word->image ""))
                 (foldr above empty-image (map word->image (list "dead" "cdeaf")))))
               (empty-scene 800 600)))

(check-expect (world->image WORLD-2)              
              (overlay
               (above
                (word->image (number->string 3))
                (text "How many words can you construct" 14 "red")
                (beside/align
                 "top"
                 (above
                  (letters->image LETTERS-2)
                  (word->image "g"))
                 (foldr above empty-image (map word->image (list "grfm" "garfm")))))
               (empty-scene 800 600)))

(check-expect (world->image WORLD-3)              
              (overlay
               (above
                (word->image (number->string 3))
                (text "How many words can you construct" 14 "red")
                (beside/align
                 "top"
                 (above
                  (letters->image LETTERS-3)
                  (word->image "nr"))
                 (foldr above empty-image (map word->image (list "nxth" "rnxth")))))
               (empty-scene 800 600)))

;; word->image : String -> Image
;; Displays a word as an image with the color and size that we want

(define (word->image w)
  (text w 24 "black"))

(check-expect (word->image "hello") (text "hello" 24 "black"))
(check-expect (word->image "red") (text "red" 24 "black"))


;; column: [NEList-of String] -> Image
;; displays the words contructed in a column

(check-expect (column (list "toughen")) (word->image "toughen"))
(check-expect (column (list "toughen" "noon")) (above
                                                (word->image "toughen")
                                                (word->image "noon")))
(define (column los)
  (foldr above empty-image (map word->image los)))

;; key-pressed : World KeyEvent -> World
;; Produce a new World in reaction to a key-press.

(check-expect (key-pressed WORLD-1 "\r") WORLD-1)
(check-expect (key-pressed WORLD-1 "a") (make-world "a" (world-letters WORLD-1) (list "dead" "cdeaf")
                                                    3))
(check-expect (key-pressed WORLD-3 "\b") (make-world "n" (world-letters WORLD-3) (list "nxth" "rnxth")
                                                     3))
(check-expect (key-pressed WORLD-1 "\b") WORLD-1)
(check-expect (key-pressed REAL-WORLD "\r") (make-world "" (world-letters REAL-WORLD)
                                                        (list "toughen" "noon") 11))
(check-expect (key-pressed REAL-WORLD-2 "\r") (make-world "" (world-letters REAL-WORLD)
                                                          (list "noon" "toughen") 1))

(define (key-pressed world key)
  (cond
    [(available? (list*
                  (letters-required (world-letters world))
                  (letters-available (world-letters world))) key) ;; letter inputed is valid
     (make-world (string-append (world-word-ip world) key)
                 (world-letters world) (world-so-far world) (world-score world))]
    [(string=? key "\b") ;; backspace
     (make-world 
      (remove-last (world-word-ip world))
      (world-letters world)
      (world-so-far world)
      (world-score world))]
    [(and (string=? key "\r") ;; press enter
          (string-contains-ci? (letters-required (world-letters world)) (world-word-ip world)) ; req
          (ormap (λ (x) (string=? (world-word-ip world) x)) (read-lines "words.txt")) ; dictionary 
          (>= (string-length (world-word-ip world)) 4) ; 4 letters
          (not (ormap (λ (x) (string=? (world-word-ip world) x)) (world-so-far world)))) ; duplicate
     (make-world ""
                 (world-letters world)
                 (cons (world-word-ip world) (world-so-far world))
                 (score world))]
    [else world]))

;; available?: [NEList-of String] String -> Boolean
;; checks if key is in the list of strings

(check-expect (available? (list "g" "f" "m" "z" "x") "d") #false)
(check-expect (available? (list "g" "f" "m" "z" "x") "f") #true)

(define (available? los key)
  (ormap (λ (x) (string=? key x)) los))

;; remove-last: String -> String
;; removes last item of string

(check-expect (remove-last "sad") "sa")
(check-expect (remove-last "") "")

(define (remove-last s)
  (if (> (string-length s) 0) 
      (substring s 0 (- (string-length s) 1))
      s))

;; score: World -> Integer
;; calculates score of an input

(check-expect (score REAL-WORLD-2) 1)
(check-expect (score REAL-WORLD-3) 11)
(check-expect (score REAL-WORLD-4) 4)

(define (score w)
  (if (bonus (world-word-ip w) (available w))
      (+ (world-score w) 11)
      (if (>= (string-length (world-word-ip w)) 8)
          (+ (world-score w)
             (- (string-length (world-word-ip w)) 4))
          (+ (world-score w)
             (remainder (string-length (world-word-ip w)) 4) 1))))

;; bonus: String Letters -> Boolean
;; determines if all letters are different

(check-expect (bonus "string" (list "s" "t" "r" "i" "n" "g")) #true)
(check-expect (bonus "slaps" (list "s" "l" "a" "p" "d")) #false)
(check-expect (bonus "excavate" (list "a" "d" "e" "t" "v" "x")) #false)

(define (bonus str letters)
  (local [;; different?: 1String -> Boolean
          ;; checks if the 1String is apart of the given string
          (define (different? l)
            (string-contains? l str))]
    (andmap different? letters)))

;; available: World -> [NEList-of String]
;; acceses available list of letters

(check-expect (available WORLD-1) (list "b" "c" "d" "e" "f" "g"))
(check-expect (available WORLD-2) (list "g" "f" "m" "z" "x" "a"))

(define (available w)
  (letters-available (world-letters w)))

(define LITTLE-DICTIONARY (list "explain" "plain" "nail" "lap"))

(define (play w)
  (big-bang
      w
    (to-draw world->image)
    (on-key key-pressed)))
