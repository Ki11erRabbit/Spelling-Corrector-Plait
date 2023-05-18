#lang plait

(require "spelling_corrector.rkt")
(require "dictionary.rkt")




(define (main word)
  (local [(define corrector (add-words! (new-corrector) dictionary))]
    (suggest-word! corrector word)))
  
