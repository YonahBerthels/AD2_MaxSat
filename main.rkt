#lang r7rs
(import (scheme base)
        (a-d sat maxsat-dimacs-parser)
        (a-d sat logger)
        (a-d sat maxsat)
        (a-d sat cdcl standard)
        (a-d sat dpll config))

;De meegegeven voorbeelden

(define form1 (parse-dimacs-file "a-d/sat/examples/example1.wcnf"))
(define form2 (parse-dimacs-file "a-d/sat/examples/example2.wcnf"))
(define form3 (parse-dimacs-file "a-d/sat/examples/example3.wcnf"))
(define form4 (parse-dimacs-file "a-d/sat/examples/example4.wcnf"))
(define form5 (parse-dimacs-file "a-d/sat/examples/example5.wcnf"))
(define form6 (parse-dimacs-file "a-d/sat/examples/example6.wcnf"))

;Pas hieronder je code toe op enkele voorbeelden
;(lsu! dpll form2)