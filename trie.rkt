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



;(define-type-alias Children (Listof(Optionof Trie)))

(define (take [n : Number] [lst : (Listof 'a)]) : (Listof 'a)
  (if (> n 0)
      (cons (first lst)
            (take (- n 1) (rest lst)))
        empty))
(define (drop [n : Number] [lst : (Listof 'a)]) : (Listof 'a)
  (if (> n 0)
      (drop (- n 1) (rest lst))
      lst))


;(define (make-children [val : Trie])
;  (make-vector 27 val))

;(define-type Trie
;  (empty-child[num : Number])
;  (child
;   [value : Char]
;   [freq : Number]
;   [children : (Vectorof Trie)]))

;(define (make-trie-node value freq)
;  (child value freq (make-vector 26 (empty-child 1))))

;(define (make-trie)
;  (child #\0 -1 (make-vector 26 (empty-child 1))))

;(define (trie-add! pos val vec)
;  (vector-set! vec pos val))

;(define (insert-at-pos [pos : Number] [val : Trie] [vec : (Vectorof Trie)]) : (Vectorof Trie)
;  (vector-set! vec pos val))



(define (make-children [val : Trie])
  (build-list 27 (lambda (v) val)))

(define-type Trie
  (empty-child[num : Number])
  (child
   [value : Char]
   [freq : Number]
   [children : (Listof Trie)]))

(define (make-trie-node value freq)
  (child value freq (build-list 26 (lambda (v) (empty-child 1)))))

(define (make-trie value)
  (child value 0 (build-list 26 (lambda (v) (empty-child 1)))))

(define (trie-add! pos val vec)
  (vector-set! vec pos val))

;(define (insert-at-pos [pos : Number] [val : Trie] [vec : (Vectorof Trie)]) : (Vectorof Trie)
;  (vector-set! vec pos val))

(define (trie-insert! [pos : Number] [val : Trie] [lst : (Listof Trie)]) : (Listof Trie)
  (append (take pos lst) (cons val (drop (+ 1 pos) lst))))
  


(define (trie-increment! node)
  (child (child-value node) (+ 1 (child-freq node)) (child-children node)))

    
(define (add-string [trie : Trie] [word : (Listof Char)]) : Trie
  (local [(define chr (first word))]
    (local [(define str (rest word))]
      (cond
        [(empty? str) (trie-increment! trie)]
        [else (type-case Trie (list-ref (child-children trie) (char->index chr))
                         [(empty-child num) (child (child-value trie) (child-freq trie) (trie-insert! (char->index chr) (add-string (make-trie chr) str) (child-children trie)))]
                         [(child value freq children) (child (child-value trie) (child-freq trie) (trie-insert! (char->index chr) (add-string (list-ref (child-children trie) (char->index chr)) str) (child-children trie)))]
                         )]))))
      


