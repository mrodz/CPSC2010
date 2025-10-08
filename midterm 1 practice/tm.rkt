#lang racket

(provide ins conf simulate)

(struct ins (c-state c-symbol n-state n-symbol dir) #:transparent) 
(struct conf (state ltape symbol rtape) #:transparent)

(define (i-match? state symbol inst)
  (and (eq? state (ins-c-state inst)) (eq? symbol (ins-c-symbol inst))))

(define (i-lookup state symbol mach)
  (if (empty? mach) #f
  (let [(x (car mach))] 
    (if (i-match? state symbol x) x (i-lookup state symbol (cdr mach)))
  )))

(define (halted? mach config)
  (eq? (i-lookup (conf-state config) (conf-symbol config) mach) #f)
  )

(define (change-state new-state config)
  (conf new-state (conf-ltape config) (conf-symbol config) (conf-rtape config)))

(define (write-symbol new-symbol config)
  (conf (conf-state config) (conf-ltape config) new-symbol (conf-rtape config)))


(define (normalize config)
  (let [(l (conf-ltape config))] 
  (if (or (empty? l) (not (eq? (car l) 'b))) 
      (let [(r (reverse (conf-rtape config)))]
        (if (or (empty? r) (not (eq? (car r) 'b))) config 
        (normalize (conf (conf-state config) (conf-ltape config) (conf-symbol config) (reverse (cdr r))))
        )
      )
      (normalize (conf (conf-state config) (cdr l) (conf-symbol config) (conf-rtape config))) 
    
  )))

(define (shift-head-left config)
  (let* [
    (l (conf-ltape config)) 
    (l_rev (reverse l)) 
    (new_symbol (if (empty? l_rev) 'b (car l_rev))) 
    (new_l (if (empty? l_rev) '() (reverse (cdr l_rev)))) 
    (new_r (cons (conf-symbol config) (conf-rtape config)))
  ]
    (normalize (conf (conf-state config) new_l new_symbol new_r))
  ))

(define (shift-head-right config)
  (let* [
    (l (conf-ltape config))
    (r (conf-rtape config))
    (new_l (append l (list (conf-symbol config))))
    (new_symbol (if (empty? r) 'b (car r)))
    (new_r (if (empty? r) '() (cdr r)))
  ]
    (normalize (conf (conf-state config) new_l new_symbol new_r))
  ))

(define (next-config mach config)
  (let* [
    (state (conf-state config))
    (symbol (conf-symbol config))
    (lookup (i-lookup state symbol mach))
  ]
    (if (eq? lookup #f) 
      config
      (let* [
        (new_state (ins-n-state lookup))
        (new_symbol (ins-n-symbol lookup))
        (dir (ins-dir lookup))
        (new_conf (write-symbol new_symbol (change-state new_state config)))
      ]
        ((if (eq? dir 'R) shift-head-right shift-head-left) new_conf)
      )
    )  
  )
  )

(define (simulate mach config n) 
  (cond
    ((<= n 0) (list config))
    ((halted? mach config) (list config))
    (else
     (cons config
           (simulate 
            mach (next-config mach config) (- n 1))))))