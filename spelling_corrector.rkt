#lang plait

(require "trie.rkt")

;Because PLAIT is unable to infer the type of a list when an empty list is created with empty, this function
;creates an empty list by creating a list with one element and then dropping that element.
(define (empty-list a)
  (drop 1 (build-list 1 (lambda (x) a))))


(define-type-alias SpellingCorrector Trie)

(define (new-corrector)
  (make-trie #\space))



(define (add-word! [word : String] corrector) : SpellingCorrector
  (add-string corrector (string->list word)))


(define (add-words! corrector [words : (Listof String)]) : SpellingCorrector
  (foldl add-word! corrector words))



#;(define (suggest-word! corrector [word : String]) : Trie
  (type-case Trie (find-string corrector (string->list word))
             [(child value freq children) (child)]
             [(empty-child number) (local [(define edit_dist1 (make-hash (gen-edit-dist word)))])]))





(define (gen-edit-dist [word : String])
  (make-hash (map (lambda (item) (pair (list->string item) item)) (append (gen-deletion (string->list word) 0)
          (append (gen-transposition (string->list word))
          (append (gen-substitution (string->list word))
          (gen-insertion (string->list word))))))))



(define (gen-deletion [word : (Listof Char)] [pos : Number]) : (Listof (Listof Char))
  (if (= pos (length word))
       (empty-list (list #\space))
       (append (list (append (take pos word) (drop (+ 1 pos) word))) (gen-deletion word (+ 1 pos)))))


(define (gen-transposition [word : (Listof Char)]) : (Listof (Listof Char))
  (trans_helperI 0 word))


(define (trans_helperI [i : Number] [word : (Listof Char)]) : (Listof (Listof Char))
  (if (= i (length word))
       (empty-list (list #\space))
       (append (trans_helperJ i (+ i 1) word) (trans_helperI (+ i 1) word))))

(define (trans_helperJ [i : Number] [j : Number] [word : (Listof Char)]) : (Listof (Listof Char))
  (if (= j (length word))
      (empty-list (list #\space))
      (append (list (replace-char (list-ref word j) i (replace-char (list-ref word i) j word))) (trans_helperJ i (+ j 1) word))))

(define (replace-char [char : Char] [pos : Number] [word : (Listof Char)]) : (Listof Char)
  (append (append (take pos word) (list char)) (drop (+ 1 pos) word)))


(define (gen-substitution [word : (Listof Char)]) : (Listof (Listof Char))
  (sub-helper1 word (string->list "abcdefghijklmnopqrstuvwxyz") 0))

(define (sub-helper1 [word : (Listof Char)] [alph : (Listof Char)] [pos : Number]) : (Listof (Listof Char))
  (if (= pos (length word))
      (empty-list (list #\space))
       (append (sub-helper2 word alph pos 0) (sub-helper1 word alph (+ 1 pos)))))

(define (sub-helper2 [word : (Listof Char)] [alph : (Listof Char)] [pos : Number] [index : Number]) : (Listof (Listof Char))
  (if (= index (length alph))
      (empty-list (list #\space))
      (append (list (replace-char (list-ref alph index) pos word)) (sub-helper2 word alph pos (+ 1 index)))))

      
(define (gen-insertion [word : (Listof Char)]) : (Listof (Listof Char))
  (ins-helper1 word (string->list "abcdefghijklmnopqrstuvwxyz") 0))

(define (ins-helper1 [word : (Listof Char)] [alph : (Listof Char)] [pos : Number]) : (Listof (Listof Char))
  (if (= pos (length alph))
      (ins-helper-end word alph)
      (append (ins-helper2 word alph pos 0) (ins-helper1 word alph (+ 1 pos)))))

(define (ins-helper2 [word : (Listof Char)] [alph : (Listof Char)] [pos : Number] [index : Number]) : (Listof (Listof Char))
  (if (= index (length alph))
      (empty-list (list #\space))
      (append (list (append (take pos word) (append (list (list-ref alph index)) (drop pos word)))) (ins-helper2 word alph pos (+ 1 index)))))

(define (ins-helper-end [word : (Listof Char)] [alph : (Listof Char)]) : (Listof (Listof Char))
    (if (= 0 (length alph))
        (empty-list (list #\space))
        (append (list (append word (take 1 alph))) (ins-helper-end word (drop 1 alph)))))
