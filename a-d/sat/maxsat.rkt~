#lang r7rs
(define-library (maxsat)
  (export blocking-variable-transform! cost pseudo-boolean-layer lsu!)
  (import (scheme base)
          (scheme inexact)
          (scheme write)
          (a-d sat cnf)
          (a-d graph labeled config)
          (a-d graph-traversing dft-labeled)
          (a-d scheme-tools))
  (begin
    ;;;;;; Procedures of the LSU algorithm

    ;cost (formula interpretation -> number)
    
    ;Given a formula and an interpretation, returns the cost associated:
    ; - +inf.0 if hard(formula) is false under the interpretation
    ; - '() (= undefined) if hard(formula) is not false but formula is undefined under the interpretation
    ; - the sum of all weights of unsatisfied soft clauses of the formula otherwise
    (define (cost form interpret)
      (error "cost -- Procedure not yet implemented") cost)




    
    ;blocking-variable-transform! (formula -> ∅)
    
    ;Applies the Blocking Variable Transform to the formula, destructively changing it.
    (define (blocking-variable-transform! form)
      (error "blocking-variable-transform! -- Procedure not yet implemented" blocking-variable-transform!))

    
        
    
    ;pseudo-boolean-layer (formula number -> formula)

    ;Given a formula "F" and a number "k", returns the formula hard(F) ∧ D(F,k), which is the pseudo-Boolean layer of F.
    ;The GTE technique is used in two steps:
    ; 1) The GTE-tree is constructed, without labels
    ; 2) DFT is used to add the correct labels and add the clauses of D(F,k) to (a copy of) hard(F) at the same time

   (define (pseudo-boolean-layer form k)

      ;%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
      ;Auxiliary procedures to calculate the size of the tree
      (define (largest-2-power-smaller-than n)
        (expt 2 (floor (log n 2))))
      (define (calc-nodes n)
        (if (< n 2)
            n
            (let ((m (largest-2-power-smaller-than n)))
              (+ (- (* 2 m) 1)
                 (if (= m n)
                     0
                     (+ 1
                        (calc-nodes (- n m))))))))
      (define (calc-depth n)
        (ceiling (log n 2)))

      ;%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
      ;Variables used in the following procedures
      (define vars-amount (number-of-vars form)) ;This will keep track of the number of variables already used
      (define softs (soft form)) ;The soft clauses of form. This will be used to fill the leaves
      (define n (length softs)) ;The amount of literals in the pseudo-boolean constraint
      (define new-form (make-formula (hard form) '() vars-amount (length (hard form)))) ;We make a copy of hard(form) as a basis to add D(form, k) to

      ;%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
      ;Procedures that will do most of the work
      (define (make-tree) 
        (letrec ((tree (new #f (exact (calc-nodes n))))
                 (i 0)
                 (full-tree! (lambda (node d)
                               (unless (= d 0)
                                 (let ((node1 (+ i 1))
                                       (node2 (+ i 2)))
                                   (add-edge! tree node node1 'dummy)
                                   (add-edge! tree node node2 'dummy)
                                   (set! i node2)
                                   (full-tree! node1 (- d 1))
                                   (full-tree! node2 (- d 1)))))))
          (let loop ((node 0)
                     (n n))
            (let ((left-nodes (largest-2-power-smaller-than n))
                  (d (calc-depth n)))
              (if (= left-nodes n)
                  (full-tree! node d)
                  (begin
                    (let ((node1 (+ i 1))
                          (node2 (+ i 2)))
                      (add-edge! tree node node1 'dummy)
                      (add-edge! tree node node2 'dummy)
                      (set! i node2)
                      (full-tree! node1 (- d 1))
                      (loop node2 (- n left-nodes)))))))
          tree))
      
      (define (add-clauses-from-tree! tree) ;See the pdf for an explanation of the algorithm
        (dft tree
             root-nop                     ;root-discovered
             node-nop                     ;node-discovered
             (lambda (node node-label)    ;node-processed
               (when (eq? 'no-label node-label) ;we fill the leaves with the necessary literals
                 (let ((cls (car softs))) ;note that we made the tree so that there are exactly the correct amount of leaves for the literals
                   (label! tree node (list (cons (negate (car (literals cls))) (min (clause-weight cls) (+ k 1)))));Every leaf contains a list of a literal-weight cons-cel representing all negations of blocking variables of the soft clauses of the formula -- as they appear in the pseudo-boolean constraint -- trimmed to k+1
                   (set! softs (cdr softs))))) 
             edge-nop                     ;edge-discovered
             (lambda (from to edge-label) ;edge-processed
               (if (eq? (label tree from) 'no-label)     ;if no label exists, it's the first time we're going up in the tree to meet the to-node (=P)
                   (label! tree from (label tree to)) ;in this case, we will temporarily copy the list of literals and weights of its first descendant (=Q) for easy access
                   (let ((Q-label (label tree from))  ;if a label already exists, it is the copied label of the first descendant Q
                         (R-label (label tree to))    ;our from-node is R, the second descendant
                         (P-label '()))               ;we will build up the label of our parent P, while adding the corresponding clauses to the formula
                     (for-each (lambda (Q-cel)        ;the label of a node consists of a list of cons-cells, the car of which represents the literal and the cdr the weight                                               
                                 (set! vars-amount (+ vars-amount 1))
                                 (let ((new-P-lit (make-literal vars-amount #f)))
                                   (set! P-label (cons (cons new-P-lit (cdr Q-cel)) P-label))           ;add all weights from the node Q to P
                                   (add-clause-in-form! new-form (make-clause (list (negate (car Q-cel)) new-P-lit) 2 #f)))) ;add the new clause to the formula
                               Q-label)
                     (for-each (lambda (R-cel)
                                 (let* ((R-weight (cdr R-cel))
                                        (R-lit (car R-cel))
                                        (P-lits (memp (lambda (P-cel) (= R-weight (cdr P-cel))) ;looks for a variable already added to P with the correct weight 
                                                     P-label))
                                        (P-lit (if P-lits
                                                   (caar P-lits)
                                                   #f)))
                                   
                                   (when (not P-lit)                                            ;add all new weights from the node Q to P
                                     (set! vars-amount (+ vars-amount 1))
                                     (let ((new-P-lit (make-literal vars-amount #f))) 
                                       (set! P-label (cons (cons new-P-lit R-weight) P-label))
                                       (set! P-lit new-P-lit)))                         
                                   (add-clause-in-form! new-form (make-clause (list (negate R-lit) P-lit) 2 #f)) ;add the new clause to the formula

                                   (for-each (lambda (Q-cel)                                    
                                               (let* ((Q+R-weight (min (+ (cdr Q-cel) R-weight) (+ k 1))) ;the weight will be capped at k+1 since the exact weight (if it's higher) is irrelevant
                                                      (Q-lit (car Q-cel))
                                                      (P-lits (memp (lambda (P-cel) (= Q+R-weight (cdr P-cel))) ;looks for a variable already added to P with the correct weight 
                                                                    P-label))
                                                      (P-lit (if P-lits
                                                                 (caar P-lits)
                                                                 #f)))

                                                 (when (not P-lit)                                             ;add all new weights that are sums of weights from Q and R to P                                                   
                                                   (set! vars-amount (+ vars-amount 1))
                                                   (let ((new-P-lit (make-literal vars-amount #f)))
                                                     (set! P-label (cons (cons new-P-lit Q+R-weight) P-label))
                                                     (set! P-lit new-P-lit)))                                                 
                                                 (add-clause-in-form! new-form (make-clause (list (negate R-lit) (negate Q-lit) P-lit) 3 #f))));add the new clause to the formula
                                             Q-label)))
                               R-label)
                     (label! tree from P-label))))
             edge-nop                   ;edge-bumped
             ;roots
             )
        (add-clause-in-form! new-form (make-clause (list (negate (caar (memp (lambda (cel) (= (+ k 1) (cdr cel))) (label tree 0))))) 1 #f)) ;from the top node L, we add the clause !x^{k+1}_L, which needs to exist
        (number-of-vars! new-form vars-amount))
   ;%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
      ;The actual algorithm
      (add-clauses-from-tree! (make-tree))

      ;Returns the formula
      new-form)     
      
    ;lsu! (procedure formula -> interpretation number)
    ;From a SAT-solver and a formula, returns a list of an interpretation which
    ;solves the MaxSAT problem for the formula and its associated cost k

    ;DO NOT EDIT THIS PROCEDURE
    (define (lsu! sat-solver formula)
     
      (blocking-variable-transform! formula)

      (let* ((hard-f (make-formula (hard formula) '() (number-of-vars formula) (length (hard formula)))) 
             (base-sat (sat-solver hard-f)))
      
        (if (not (interpretation? base-sat))
            (begin (display "Hard(F) has no solutions.")
                   (newline)
                   base-sat)
            (let lsu-iter ((k (cost formula base-sat))
                           (interpretation base-sat))
              (display "Solution found for cost = ")
              (display k)
              (newline)
              (if (= k 0) ;A cost of 0 can never be optimized
                  (list interpretation k)
                  (begin 
                    (display "Looking for optimization")
                    (newline)
                    (let* ((hardform+pbl (pseudo-boolean-layer formula (- k 1)))
                           (sat (sat-solver hardform+pbl)))                  
                      (if (not (interpretation? sat))
                          (begin
                            (display "No optimization found")
                            (newline)
                            (list interpretation k))
                          (lsu-iter (cost formula sat) sat)))))))))))