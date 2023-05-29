#lang plait

(require "spelling_corrector.rkt")
(require "dictionary.rkt")



; driver for the spelling corrector
; takes a word as input and returns the best suggestion for that word
; call with a string to get a result
(define (main [word : String]) : String
  (local [(define corrector (add-words! (new-corrector) dictionary))]
    (suggest-word! corrector word)))
  
