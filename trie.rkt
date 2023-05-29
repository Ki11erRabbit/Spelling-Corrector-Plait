#lang plait

(define (char->index c)
  (if (char=? c #\a) 0
      (if (char=? c #\b) 1
          (if (char=? c #\c) 2
              (if (char=? c #\d) 3
                  (if (char=? c #\e) 4
                      (if (char=? c #\f) 5
                          (if (char=? c #\g) 6
                              (if (char=? c #\h) 7
                                  (if (char=? c #\i) 8
                                      (if (char=? c #\j) 9
                                          (if (char=? c #\k) 10
                                              (if (char=? c #\l) 11
                                                  (if (char=? c #\m) 12
                                                      (if (char=? c #\n) 13
                                                          (if (char=? c #\o) 14
                                                              (if (char=? c #\p) 15
                                                                  (if (char=? c #\q) 16
                                                                      (if (char=? c #\r) 17
                                                                          (if (char=? c #\s) 18
                                                                              (if (char=? c #\t) 19
                                                                                  (if (char=? c #\u) 20
                                                                                      (if (char=? c #\v) 21
                                                                                          (if (char=? c #\w) 22
                                                                                              (if (char=? c #\x) 23
                                                                                                  (if (char=? c #\y) 24
                                                                                                      (if (char=? c #\z) 25
                                                                                                          26)))))))))))))))))))))))))))




(define (take [n : Number] [lst : (Listof 'a)]) : (Listof 'a)
  (if (> n 0)
      (cons (first lst)
            (take (- n 1) (rest lst)))
        empty))
(define (drop [n : Number] [lst : (Listof 'a)]) : (Listof 'a)
  (if (> n 0)
      (drop (- n 1) (rest lst))
      lst))

; Helper function that turns a list of lists into a single list
(define (flatten-list [lst : (Listof (Listof 'a))]) : (Listof 'a)
  (cond
    [(empty? lst) empty]
    [else (append (first lst) (flatten-list (rest lst)))]))

; max function but for lists
(define (maximum lst)
  (max-helper (first lst) (rest lst)))

; helper for maximum to find the max of a list
(define (max-helper item lst)
    (cond
        [(empty? lst) item]
        [else (max-helper (max item (first lst)) (rest lst))]))

; This is my way to replicate the Ord typeclass in Haskell

; f: a function that takes a tuple of 'a and Trie and returns a number
; f2: a function that takes a number and a tuple of 'a and Trie and returns a tuple of 'a and Trie
; lst: a list of tuples of 'a and Trie
; returns the tuple of 'a and Trie with the maximum value of 'a
(define (maximum-func [f : (('a * Trie) -> Number)] [f2 : (Number ('a * Trie) -> ('a * Trie))] [lst : (Listof ('a * Trie))]) : ('a * Trie)
  (max-func-helper f f2 (first lst) (rest lst)))

; helper for maximum-func to find the max of a list
; f: a function that takes a tuple of 'a and Trie and returns a number
; f2: a function that takes a number and a tuple of 'a and Trie and returns a tuple of 'a and Trie
; item: the current max tuple of 'a and Trie
; lst: a list of tuples of 'a and Trie
(define (max-func-helper [f : (('a * Trie) -> Number)] [f2 : (Number ('a * Trie) -> ('a * Trie))] [item : ('a * Trie) ] [lst : (Listof ('a * Trie))]) : ('a * Trie)
    (cond
        [(empty? lst) item]
        [else (max-func-helper f f2 (f2 (max (f item) (f (first lst))) item) (rest lst))]))

; a function that takes a number and a tuple of 'a and Trie and returns a tuple of 'a and Trie
(define (wrap num [value : ('a * Trie)]) : ('a * Trie)
  value)

; initializes the list of nodes for a trie
(define (make-children [val : Trie])
  (build-list 27 (lambda (v) val)))

; the data type for a trie data structure
(define-type Trie
  (empty-child[num : Number])
  (child
   [value : Char]
   [freq : Number]
   [children : (Listof Trie)]))

; creates an empty trie node
; value: the character that the node represents
; freq: the number of times the node has been visited
; returns a trie node with the given value and frequency
(define (make-trie-node [value : Char] [freq : Number]) : Trie
  (child value freq (build-list 26 (lambda (v) (empty-child 1)))))


; creates an empty trie node with just a value
; value: the character that the node represents
; returns a trie node with the given value and frequency
(define (make-trie [value : Char]) : Trie
  (child value 0 (build-list 26 (lambda (v) (empty-child 1)))))

; inserts a trie node into a list of trie nodes
; pos: the position in the list to insert the node
; val: the node to insert
; lst: the list to insert the node into
; returns the list with the node inserted
(define (trie-insert! [pos : Number] [val : Trie] [lst : (Listof Trie)]) : (Listof Trie)
  (append (take pos lst) (cons val (drop (+ 1 pos) lst))))

; increments the frequency of a trie node
; node: the node to increment
; returns the node with the frequency incremented
(define (trie-increment! [node : Trie]) : Trie
  (child (child-value node) (+ 1 (child-freq node)) (child-children node)))

; adds a Listof Char to a trie
; trie: the trie to add the string to
; word: the string to add to the trie
; returns the trie with the string added
(define (add-string [trie : Trie] [word : (Listof Char)]) : Trie
  (cond
    [(empty? word) (trie-increment! trie)]
    [else (local [(define chr (first word))]
            (local [(define str (rest word))]
              (type-case Trie (list-ref (child-children trie) (char->index chr))
                         [(empty-child num) (child (child-value trie) (child-freq trie) (trie-insert! (char->index chr) (add-string (make-trie chr) str) (child-children trie)))]
                         [(child value freq children) (child (child-value trie) (child-freq trie) (trie-insert! (char->index chr) (add-string (list-ref (child-children trie) (char->index chr)) str) (child-children trie)))]
                         )))]))

; finds a string in a trie
; trie: the trie to find the string in
; word: the string to find in the trie
; returns the trie with the string added
(define (find-string [trie : Trie] [word : (Listof Char)]) : Trie
  (cond
    [(empty? word) (if (= (child-freq trie) 0)
                   (empty-child 1)
                   trie)]
    [else (local [(define chr (first word))]
            (local [(define str (rest word))]
              (type-case Trie (list-ref (child-children trie) (char->index chr))
                         [(empty-child num) (empty-child 1)]
                         [(child value freq children) (find-string (list-ref (child-children trie) (char->index chr)) str)]
                         )))]))



