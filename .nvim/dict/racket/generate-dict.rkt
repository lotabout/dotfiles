
;; generate-dict.rkt

;; Generates a list of defined symbols in the default Racket namespace.
;; The output file can be used with Vim to enable autocompletion.


#lang racket/base


(define symbols
    (sort
        (map symbol->string
             (namespace-mapped-symbols (make-base-namespace)))
        string<?))


(call-with-output-file* "racket-dict.vim"
    (lambda (out)
        (for-each
            (lambda (symbol) (displayln symbol out))
            symbols))
    #:exists 'replace)

