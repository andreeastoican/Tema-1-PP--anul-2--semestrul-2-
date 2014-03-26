#|
Interrogates a database given the specified query. Returns a list of substitutions.
|#

(define ask
  (lambda (db query)
    (letrec ([rezultate (λ (listaS ListLeg ) 
                          (if (null? listaS)
                              (cons ListLeg '())
                              (let reia ([L (car listaS)] [listaDB db])
                                (if (null? listaDB)
                                    '()
                                    (if (equal? (car L) 'if)
                                        (if (equal? (variabilaPredicat (cadr L)) '#f)
                                            (if (eval (cadr L))
                                                (if (null? (cdr listaS))
                                                    (cons ListLeg '())
                                                    (reia (cadr listaS) listaDB))
                                                '())
                                            (error "Unbound variable " (variabilaPredicat (cadr L))))
                                        (let ([verifica (verificaPotrivire L (car listaDB) ListLeg)])
                                          (if (equal? verifica #f)
                                              (reia L (cdr listaDB))
                                              (append 
                                               (rezultate (inlocuiesteVariabile (cdr listaS) verifica) verifica) 
                                               (reia L (cdr listaDB))))))))))])
      (let ([solutii (map (λ (rez) 
                            (map (λ (p) (cons (car p) (cons (cdr p) '())))
                                 (filter (λ (pereche) 
                                           (existaVar? pereche (listaSELECT query))) 
                                         rez))) 
                          (rezultate (listaSearch query) '()))])
        (if (= (length (listaSELECT query)) (length (car solutii)))
            solutii
            (error "Unbound variable " (existVarNelegata? (listaSELECT query) (car solutii))))))))



;; utility functions

#|
Checks if the provided value is a variable
|#
(define (variable? sym)
  (and (symbol? sym) (equal? (string-ref (symbol->string sym) 0) #\?)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; ((SEARCH (?x lives-in ?city) (?city is-in USA) (?x plays ?game) (?game player-no ?no) (if (>= ?no 2))) (SELECT (?x ?game))) => 
;         ((?x lives-in ?city) (?city is-in USA) (?x plays ?game) (?game player-no ?no) (if (>= ?no 2)))
(define listaSearch
  (λ (l)
    (cdar l)))

;;;;;;;;;;;;;;

; ((SEARCH (?x lives-in ?city) (?city is-in USA) (?x plays ?game) (?game player-no ?no) (if (>= ?no 2))) (SELECT (?x ?game))) => (?x ?game)
(define listaSELECT
  (λ (l)
    (cadr (cadr l))))

;;;;;;;;;;;;;;;;;;;;;

; Functie care verifica daca variabila data ca patametru se gaseste in lista de SELECT
(define existaVar? 
  (λ (pereche listaSel)
    (if (null? listaSel)
        #f
        (if (equal? (car pereche) (car listaSel))
            #t
            (existaVar? pereche (cdr listaSel))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Functie ce verifica daca 2 liste se potrivesc si returneaza #f daca nu se potrivesc sau lista de perechi (variabila . valoare) daca se potrivesc
(define verificaPotrivire
  (λ (clause db listaLegari)
    (cond 
      ((null? clause) listaLegari)
      ((and (list? (car clause)) (list? (car db)))
       (if (and 
            (verificaPotrivire (car clause) (car db) '() ) 
            (verificaPotrivire (inlocuiesteVarInLista (cdr clause) (verificaPotrivire (car clause) (car db) '()) ) (cdr db) listaLegari) )
           (append 
            (verificaPotrivire (inlocuiesteVarInLista (cdr clause) (verificaPotrivire (car clause) (car db) '()) ) (cdr db) listaLegari) 
            (verificaPotrivire (car clause) (car db) '()))
           #f))
      ((variable? (car clause)) (verificaPotrivire (cdr clause) (cdr db) (append listaLegari (cons (cons (car clause) (car db)) '()) )))
      ((equal? (car clause) (car db)) (verificaPotrivire (cdr clause) (cdr db) listaLegari))
      (else #f))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Functie care returneaza elementul la care este legata variabila data ca parametru sau returneaza tot variabila daca nu exista legare pentru ea
(define returnVal
  (λ (var listLeg)
    (if (null? listLeg)
        var
        (if (equal? var (caar listLeg))
            (cdar listLeg)
            (returnVal var (cdr listLeg))))))

; Functie care inlocuieste toate variabilele cu elementul la care a fost legat
(define inlocuiesteVarInLista
  (λ (L listaLeg)
    (map (λ (x) (if (list? x)
                    (inlocuiesteVarInLista x listaLeg)
                    (returnVal x listaLeg))) L)))

; Functie care aplica rezultatul functiei inlocuiesteVarInLista pe fiecare element din lista data ca parametru
(define inlocuiesteVariabile
  (λ (listaS listaLeg)
    (map (λ (x) (inlocuiesteVarInLista x listaLeg)) listaS)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Functie ce returneaza prima variabila care este gasita in corpul predicatului sau #f in caz ca nu exista
; Aceasta functie este folosita pentru a arunca eroare in cazul in care predicatul contine variabile nelegate
(define variabilaPredicat
  (λ (p)
    (cond
      ((null? p) #f)
      ((list? (car p)) (if (variabilaPredicat (car p)) 
                           (variabilaPredicat (car p))
                           (variabilaPredicat (cdr p))))
      ((variable? (car p)) (car p))
      (else (variabilaPredicat (cdr p))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Aceste functii sunt folosite pentru a arunca eroare in cazul in care partea SELECT contine variabile care nu exista in partea SEARCH

; Functie care intoarce #t daca variabila data ca parametru se gaseste in lista de rezultate sau #f altfel
(define existaVarDinSelect?
  (λ (var LRez)
    (cond
      ((null? LRez) #f)
      ((equal? var (caar LRez)) #t)
      (else (existaVarDinSelect? var (cdr LRez))))))

; Functie care intoarce variabila care este in plus in partea de SELECT 
(define existVarNelegata?
  (λ (LSel LRez) 
    (if (equal? (existaVarDinSelect? (car LSel) LRez) #f) 
        (car LSel)
        (existVarNelegata? (cdr LSel) LRez))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
