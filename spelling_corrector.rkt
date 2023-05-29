#lang plait

(require "trie.rkt")



(define-type-alias SpellingCorrector Trie)

(define (new-corrector)
  (make-trie #\space))



(define (add-word! [word : String] corrector) : SpellingCorrector
  (add-string corrector (string->list word)))


(define (add-words! corrector [words : (Listof String)]) : SpellingCorrector
  (foldl add-word! corrector words))


; main logic function for spelling corrector
; returns the most likely word from the trie
; or returns a message saying that there are no suggestions
(define (suggest-word! [corrector : SpellingCorrector] [word : String]) : String
  (type-case Trie (find-string corrector (string->list word))
             [(child value freq children) word]
             [(empty-child number) (local [(define edit_dist1 (make-hash (gen-edit-dist word)))]
                                     (local [(define possible_words (filter (lambda (x) (child? (snd x))) (map (lambda (x) (pair (list->string x) (find-string corrector x))) (hash-keys edit_dist1))))]
                                       (case (length possible_words)
                                         [(0) (local [(define edit_dist2 (make-hash (gen-edit-dist2 (hash-keys edit_dist1))))]
                                                (local [(define possible_words2 (filter (lambda (x) (child? (snd x))) (map (lambda (x) (pair (list->string x) (find-string corrector x))) (hash-keys edit_dist2))))]
                                                  (case (length possible_words2)
                                                    [(0) "No Suggestions"]
                                                    [else (fst (maximum-func (lambda (x) (child-freq (snd x))) wrap possible_words2))])))]
                                         [else (fst (maximum-func (lambda (x) (child-freq (snd x))) wrap possible_words))])
                                       ))]))

; generates a list of words that are mutations of the input word
; The return value is to allow for it to be used in the hash table
; word: the word to generate mutations of
; returns a list of pairs of the mutated word
(define (gen-edit-dist [word : String]) : (Listof ((Listof Char) * (Listof Char)))
  (map (lambda (item) (pair item item)) (append (gen-deletion (string->list word) 0)
          (append (gen-transposition (string->list word))
          (append (gen-substitution (string->list word))
          (gen-insertion (string->list word)))))))

; generates a list of words that are mutations of the input word
; The return value is to allow for it to be used in the hash table
; edit-dist1: the list of words to generate mutations of
; returns a list of pairs of the mutated word
(define (gen-edit-dist2 [edit-dist1 : (Listof (Listof Char))]) : (Listof ('a * 'b))
  (flatten-list (map (lambda (item) (gen-edit-dist (list->string item))) edit-dist1)))

; generates a list of words that is missing one character from the input word
; word: the word to generate mutations of
; pos: the position of the character to remove
; returns a list of words that are missing one character from the input word
(define (gen-deletion [word : (Listof Char)] [pos : Number]) : (Listof (Listof Char))
  (if (= pos (length word))
      empty
       (append (list (append (take pos word) (drop (+ 1 pos) word))) (gen-deletion word (+ 1 pos)))))


; generates a list of words that have two characters transposed, that is switched around
; word: the word to generate mutations of
; returns a list of words that have two characters transposed
(define (gen-transposition [word : (Listof Char)]) : (Listof (Listof Char))
  (trans_helperI 0 word))

; helper function for gen-transposition
; the I for loop of the imperative version
; i: the first index to swap
; word: the word to generate mutations of
; returns a list of words that have two characters transposed
(define (trans_helperI [i : Number] [word : (Listof Char)]) : (Listof (Listof Char))
  (if (= i (length word))
       empty
       (append (trans_helperJ i (+ i 1) word) (trans_helperI (+ i 1) word))))

; helper function for gen-transposition
; the J for loop of the imperative version
; i: the first index to swap
; j: the second index to swap
; word: the word to generate mutations of
; returns a list of words that have two characters transposed
(define (trans_helperJ [i : Number] [j : Number] [word : (Listof Char)]) : (Listof (Listof Char))
  (if (= j (length word))
      empty
      (append (list (replace-char (list-ref word j) i (replace-char (list-ref word i) j word))) (trans_helperJ i (+ j 1) word))))

; helper function for gen-transposition
; replaces the character at the given position with the given character
; char: the character to replace with
; pos: the position to replace at
; word: the word to replace the character in
; returns the word with the character replaced
(define (replace-char [char : Char] [pos : Number] [word : (Listof Char)]) : (Listof Char)
  (append (append (take pos word) (list char)) (drop (+ 1 pos) word)))

; generates a list of words that have one character replaced with another
; word: the word to generate mutations of
; returns a list of words that have one character replaced with another
(define (gen-substitution [word : (Listof Char)]) : (Listof (Listof Char))
  (sub-helper1 word (string->list "abcdefghijklmnopqrstuvwxyz") 0))

; helper function for gen-substitution
; one of the functions that gets called recursively
; word: the word to generate mutations of
; alph: the alphabet to replace characters with
; pos: the position of the character to replace
; returns a list of words that have one character replaced with another
(define (sub-helper1 [word : (Listof Char)] [alph : (Listof Char)] [pos : Number]) : (Listof (Listof Char))
  (if (= pos (length word))
      empty
       (append (sub-helper2 word alph pos 0) (sub-helper1 word alph (+ 1 pos)))))

; helper function for gen-substitution
; one of the functions that gets called recursively
; word: the word to generate mutations of
; alph: the alphabet to replace characters with
; pos: the position of the character to replace
; index: the index of the character to replace with from the alphabet
; returns a list of words that have one character replaced with another
(define (sub-helper2 [word : (Listof Char)] [alph : (Listof Char)] [pos : Number] [index : Number]) : (Listof (Listof Char))
  (if (= index (length alph))
      empty
      (append (list (replace-char (list-ref alph index) pos word)) (sub-helper2 word alph pos (+ 1 index)))))

; generates a list of words that have one character inserted
; word: the word to generate mutations of
; returns a list of words that have one character inserted
(define (gen-insertion [word : (Listof Char)]) : (Listof (Listof Char))
  (ins-helper1 word (string->list "abcdefghijklmnopqrstuvwxyz") 0))

; helper function for gen-insertion
; one of the functions that gets called recursively
; word: the word to generate mutations of
; alph: the alphabet to insert characters from
; pos: the position to insert the character at
; returns a list of words that have one character inserted
(define (ins-helper1 [word : (Listof Char)] [alph : (Listof Char)] [pos : Number]) : (Listof (Listof Char))
  (if (= pos (length word))
      (ins-helper-end word alph)
      (append (ins-helper2 word alph pos 0) (ins-helper1 word alph (+ 1 pos)))))

; helper function for gen-insertion
; one of the functions that gets called recursively
; word: the word to generate mutations of
; alph: the alphabet to insert characters from
; pos: the position to insert the character at
; index: the index of the character to insert from the alphabet
; returns a list of words that have one character inserted
(define (ins-helper2 [word : (Listof Char)] [alph : (Listof Char)] [pos : Number] [index : Number]) : (Listof (Listof Char))
  (if (= index (length alph))
      empty
      (append (list (append (take pos word) (append (list (list-ref alph index)) (drop pos word)))) (ins-helper2 word alph pos (+ 1 index)))))

; helper function for gen-insertion
; gets called in the situation when the position is at the end of the word
; word: the word to generate mutations of
; alph: the alphabet to insert characters from
; returns a list of words that have one character inserted
(define (ins-helper-end [word : (Listof Char)] [alph : (Listof Char)]) : (Listof (Listof Char))
    (if (= 0 (length alph))
        empty
        (append (list (append word (take 1 alph))) (ins-helper-end word (drop 1 alph)))))
