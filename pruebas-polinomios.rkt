#lang eopl
;Autores: Josue David Cocoma Tascon 2477087, Juan Diego Montaño Vergara 2477334

;; Taller 1 — Polinomios dispersos.
;; Parte 4: la misma batería de pruebas sobre las tres representaciones.

(require rackunit)
(require rackunit/text-ui)
(require (prefix-in listas: "polinomios-listas.rkt"))
(require (prefix-in procs:  "polinomios-procedimientos.rkt"))
(require (prefix-in dt:     "polinomios-datatypes.rkt"))

; auxiliar: check-poli
; contrato: (coeficiente-de) x polinomio x lista de (exponente coef) -> verificación
; proposito: comprueba, usando solo la interfaz, que el polinomio tiene
; exactamente los términos de `esperado` entre los exponentes 0 y 8:
; en los que aparecen, el coeficiente coincide, en los demás, coeficiente-de
; levanta el error de "exponente no registrado".

(define maximo-expo 8)

(define check-poli
  (lambda (coef p esperado)
    (letrec ((revisar
              (lambda (e)
                (if (> e maximo-expo)
                    #t
                    (let ((par (assv e esperado)))
                      (if par
                          (check-equal? (coef p e) (cadr par))
                          (check-exn #rx"no tiene termino"
                                     (lambda () (coef p e))))
                      (revisar (+ e 1)))))))
      (revisar 0))))

; batería comun: las cuatro funciones de la interfaz
; contrato: string x procedimientos de la interfaz -> test-suite
; proposito: se aplica sin cambios a las tres representaciones

(define suite-interfaz
  (lambda (nombre cero ins coef del)
    (let ((p (ins (ins (ins (cero 'x) 7 0) -3/2 2) 4 5)))   ; 4x^5 - (3/2)x^2 + 7
      (test-suite
       nombre

       ;polinomio-cero y polinomio nulo como caso base
       (test-case "cero: no tiene ningún término"
         (check-poli coef (cero 'x) '()))
       (test-case "cero: consultar el coeficiente de 0 es error"
         (check-exn #rx"no tiene termino" (lambda () (coef (cero 'x) 0))))
       (test-case "cero: eliminar sobre el nulo es error"
         (check-exn #rx"no tiene termino" (lambda () (del (cero 'x) 3))))
       (test-case "cero: insertar sobre el nulo crea un único término"
         (check-poli coef (ins (cero 'x) 3 5) '((5 3))))
       (test-case "cero: insertar coeficiente 0 sobre el nulo lo deja nulo"
         (check-poli coef (ins (cero 'x) 0 4) '()))
       (test-case "cero: funciona con otra variable"
         (check-poli coef (ins (cero 'y) -2 1) '((1 -2))))

       ;insertar termino
       (test-case "insertar: construcción del ejemplo del enunciado"
         (check-poli coef p '((5 4) (2 -3/2) (0 7))))
       (test-case "insertar: el orden de inserción no importa (otro orden)"
         (check-poli coef (ins (ins (ins (cero 'x) 4 5) 7 0) -3/2 2)
                     '((5 4) (2 -3/2) (0 7))))
       (test-case "insertar: exponente existente suma los coeficientes"
         (check-poli coef (ins p 1 2) '((5 4) (2 -1/2) (0 7))))
       (test-case "insertar: suma cero cancela el término"
         (check-poli coef (ins p 3/2 2) '((5 4) (0 7))))
       (test-case "insertar: cancelar el único término deja el nulo"
         (check-poli coef (ins (ins (cero 'x) 5 3) -5 3) '()))
       (test-case "insertar: coeficiente cero no altera el polinomio"
         (check-poli coef (ins p 0 3) '((5 4) (2 -3/2) (0 7))))
       (test-case "insertar: coeficiente cero sobre exponente existente no altera"
         (check-poli coef (ins p 0 2) '((5 4) (2 -3/2) (0 7))))
       (test-case "insertar: término de mayor exponente va al inicio"
         (check-poli coef (ins p 9/4 7) '((7 9/4) (5 4) (2 -3/2) (0 7))))
       (test-case "insertar: término intermedio queda en su lugar"
         (check-poli coef (ins p 1 3) '((5 4) (3 1) (2 -3/2) (0 7))))
       (test-case "insertar: suma de racionales que da entero"
         (check-equal? (coef (ins (ins (cero 'x) 1/2 1) 1/2 1) 1) 1))
       (test-case "insertar: ERROR con exponente negativo"
         (check-exn #rx"exponente debe ser un entero no negativo"
                    (lambda () (ins p 5 -1))))
       (test-case "insertar: ERROR con exponente no entero"
         (check-exn #rx"exponente debe ser un entero no negativo"
                    (lambda () (ins p 5 3/2))))
       (test-case "insertar: ERROR con coeficiente inexacto"
         (check-exn #rx"coeficiente debe ser un numero exacto"
                    (lambda () (ins p 0.5 1))))
       (test-case "insertar: ERROR con coeficiente que no es número"
         (check-exn #rx"coeficiente debe ser un numero exacto"
                    (lambda () (ins p 'a 1))))

       ;coeficiente
       (test-case "coeficiente-de: racional"
         (check-equal? (coef p 2) -3/2))
       (test-case "coeficiente-de: entero del primer término"
         (check-equal? (coef p 5) 4))
       (test-case "coeficiente-de: término independiente"
         (check-equal? (coef p 0) 7))
       (test-case "coeficiente-de: tras una suma que cambió el coeficiente"
         (check-equal? (coef (ins p 1 2) 2) -1/2))
       (test-case "coeficiente-de: ERROR con exponente no registrado"
         (check-exn #rx"no tiene termino con ese exponente"
                    (lambda () (coef p 3))))
       (test-case "coeficiente-de: ERROR con exponente mayor que todos"
         (check-exn #rx"no tiene termino con ese exponente"
                    (lambda () (coef p 100))))
       (test-case "coeficiente-de: ERROR sobre un término cancelado"
         (check-exn #rx"no tiene termino con ese exponente"
                    (lambda () (coef (ins p 3/2 2) 2))))

       ;; ---- eliminar-termino ----
       (test-case "eliminar: término intermedio"
         (check-poli coef (del p 2) '((5 4) (0 7))))
       (test-case "eliminar: primer término"
         (check-poli coef (del p 5) '((2 -3/2) (0 7))))
       (test-case "eliminar: término independiente"
         (check-poli coef (del p 0) '((5 4) (2 -3/2))))
       (test-case "eliminar: no modifica el polinomio original"
         (begin (del p 2)
                (check-poli coef p '((5 4) (2 -3/2) (0 7)))))
       (test-case "eliminar: eliminar todos los términos deja el nulo"
         (check-poli coef (del (del (del p 5) 2) 0) '()))
       (test-case "eliminar: ERROR con exponente no registrado"
         (check-exn #rx"no tiene termino con ese exponente"
                    (lambda () (del p 3))))
       (test-case "eliminar: ERROR al eliminar dos veces el mismo"
         (check-exn #rx"no tiene termino con ese exponente"
                    (lambda () (del (del p 2) 2))))))))


; PRUEBAS
(define listas-exponentes
  (lambda (ts)
    (if (listas:sin-terminos? ts)
        '()
        (cons (listas:expo-nat->k
               (listas:termino->expo (listas:mas-terminos->term ts)))
              (listas-exponentes (listas:mas-terminos->resto ts))))))

(define listas-inserta-todo
  (lambda (var pares)
    (if (null? pares)
        (listas:polinomio-cero var)
        (listas:insertar-termino (listas-inserta-todo var (cdr pares))
                                 (cadr (car pares)) (car (car pares))))))

(define suite-listas
  (test-suite
   "listas: constructores, observadores y unicidad"
   (test-case "coef-ent y coef-ent->n"
     (check-equal? (listas:coef-ent->n (listas:coef-ent 4)) 4)
     (check-true (listas:coef-ent? (listas:coef-ent 4)))
     (check-false (listas:coef-rac? (listas:coef-ent 4))))
   (test-case "coef-rac, numerador y denominador"
     (check-equal? (listas:coef-rac->num (listas:coef-rac -3 2)) -3)
     (check-equal? (listas:coef-rac->den (listas:coef-rac -3 2)) 2)
     (check-true (listas:coef-rac? (listas:coef-rac -3 2))))
   (test-case "expo-nat y expo-nat->k"
     (check-equal? (listas:expo-nat->k (listas:expo-nat 5)) 5)
     (check-true (listas:expo-nat? (listas:expo-nat 5))))
   (test-case "termino y sus extractores"
     (let ((t (listas:termino (listas:coef-ent 7) (listas:expo-nat 0))))
       (check-true (listas:termino? t))
       (check-equal? (listas:coef-ent->n (listas:termino->coef t)) 7)
       (check-equal? (listas:expo-nat->k (listas:termino->expo t)) 0)))
   (test-case "sin-terminos, mas-terminos y sus extractores"
     (let* ((t (listas:termino (listas:coef-ent 7) (listas:expo-nat 0)))
            (ts (listas:mas-terminos t (listas:sin-terminos))))
       (check-true (listas:sin-terminos? (listas:sin-terminos)))
       (check-true (listas:mas-terminos? ts))
       (check-false (listas:sin-terminos? ts))
       (check-equal? (listas:mas-terminos->term ts) t)
       (check-true (listas:sin-terminos? (listas:mas-terminos->resto ts)))))
   (test-case "poli, nombre-var y sus extractores"
     (let ((p (listas:poli (listas:nombre-var 'z) (listas:sin-terminos))))
       (check-true (listas:poli? p))
       (check-equal? (listas:nombre-var->s (listas:poli->var p)) 'z)
       (check-true (listas:sin-terminos? (listas:poli->terms p)))))
   (test-case "invariante: exponentes estrictamente decrecientes sin importar el orden"
     (check-equal?
      (listas-exponentes
       (listas:poli->terms (listas-inserta-todo 'x '((1 1) (4 1) (2 1) (5 1) (3 1)))))
      '(5 4 3 2 1))
     (check-equal?
      (listas-exponentes
       (listas:poli->terms (listas-inserta-todo 'x '((5 1) (3 1) (4 1) (1 1) (2 1)))))
      '(5 4 3 2 1)))
   (test-case "unicidad: mismo polinomio, mismas estructuras"
     (check-equal? (listas-inserta-todo 'x '((1 2) (4 3) (2 5)))
                   (listas-inserta-todo 'x '((2 5) (1 2) (4 3)))))
   (test-case "invariante: coeficientes guardados reducidos y con denominador positivo"
     (let* ((p (listas:insertar-termino (listas:polinomio-cero 'x) (/ 6 -4) 2))
            (c (listas:termino->coef
                (listas:mas-terminos->term (listas:poli->terms p)))))
       (check-true (listas:coef-rac? c))
       (check-equal? (listas:coef-rac->num c) -3)
       (check-equal? (listas:coef-rac->den c) 2)))))

; pruebas propias de la representación con procedimientos

(define procs-exponentes
  (lambda (ts)
    (if (procs:sin-terminos? ts)
        '()
        (cons (procs:expo-nat->k
               (procs:termino->expo (procs:mas-terminos->term ts)))
              (procs-exponentes (procs:mas-terminos->resto ts))))))

(define procs-inserta-todo
  (lambda (var pares)
    (if (null? pares)
        (procs:polinomio-cero var)
        (procs:insertar-termino (procs-inserta-todo var (cdr pares))
                                (cadr (car pares)) (car (car pares))))))

(define suite-procs
  (test-suite
   "procedimientos: constructores, observadores y unicidad"
   (test-case "coef-ent y coef-ent->n"
     (check-equal? (procs:coef-ent->n (procs:coef-ent 4)) 4)
     (check-true (procs:coef-ent? (procs:coef-ent 4)))
     (check-false (procs:coef-rac? (procs:coef-ent 4))))
   (test-case "coef-rac, numerador y denominador"
     (check-equal? (procs:coef-rac->num (procs:coef-rac -3 2)) -3)
     (check-equal? (procs:coef-rac->den (procs:coef-rac -3 2)) 2)
     (check-true (procs:coef-rac? (procs:coef-rac -3 2))))
   (test-case "expo-nat y expo-nat->k"
     (check-equal? (procs:expo-nat->k (procs:expo-nat 5)) 5)
     (check-true (procs:expo-nat? (procs:expo-nat 5))))
   (test-case "termino y sus extractores"
     (let ((t (procs:termino (procs:coef-ent 7) (procs:expo-nat 0))))
       (check-true (procs:termino? t))
       (check-equal? (procs:coef-ent->n (procs:termino->coef t)) 7)
       (check-equal? (procs:expo-nat->k (procs:termino->expo t)) 0)))
   (test-case "sin-terminos, mas-terminos y sus extractores"
     (let* ((t (procs:termino (procs:coef-ent 7) (procs:expo-nat 0)))
            (ts (procs:mas-terminos t (procs:sin-terminos))))
       (check-true (procs:sin-terminos? (procs:sin-terminos)))
       (check-true (procs:mas-terminos? ts))
       (check-false (procs:sin-terminos? ts))
       (check-eq? (procs:mas-terminos->term ts) t)
       (check-true (procs:sin-terminos? (procs:mas-terminos->resto ts)))))
   (test-case "poli, nombre-var y sus extractores"
     (let ((p (procs:poli (procs:nombre-var 'z) (procs:sin-terminos))))
       (check-true (procs:poli? p))
       (check-equal? (procs:nombre-var->s (procs:poli->var p)) 'z)
       (check-true (procs:sin-terminos? (procs:poli->terms p)))))
   (test-case "los datos son procedimientos: no se pueden inspeccionar como listas"
     (check-true (procedure? (procs:coef-ent 4)))
     (check-false (pair? (procs:coef-ent 4)))
     (check-false (pair? (procs:polinomio-cero 'x))))
   (test-case "invariante: exponentes estrictamente decrecientes sin importar el orden"
     (check-equal?
      (procs-exponentes
       (procs:poli->terms (procs-inserta-todo 'x '((1 1) (4 1) (2 1) (5 1) (3 1)))))
      '(5 4 3 2 1))
     (check-equal?
      (procs-exponentes
       (procs:poli->terms (procs-inserta-todo 'x '((5 1) (3 1) (4 1) (1 1) (2 1)))))
      '(5 4 3 2 1)))
   (test-case "invariante: coeficientes guardados reducidos y con denominador positivo"
     (let* ((p (procs:insertar-termino (procs:polinomio-cero 'x) (/ 6 -4) 2))
            (c (procs:termino->coef
                (procs:mas-terminos->term (procs:poli->terms p)))))
       (check-true (procs:coef-rac? c))
       (check-equal? (procs:coef-rac->num c) -3)
       (check-equal? (procs:coef-rac->den c) 2)))))


; Pruebas propias de la representación con datatypes: constructores,
; unicidad de la forma y la función sumar

(define dt-inserta-todo
  (lambda (var pares)
    (if (null? pares)
        (dt:polinomio-cero var)
        (dt:insertar-termino (dt-inserta-todo var (cdr pares))
                             (cadr (car pares)) (car (car pares))))))

; p = 4x^5 - (3/2)x^2 + 7   y   q = -4x^5 + (1/2)x^2 + 2x  (enunciado, Parte 3)
(define dt-p (dt-inserta-todo 'x '((5 4) (2 -3/2) (0 7))))
(define dt-q (dt-inserta-todo 'x '((5 -4) (2 1/2) (1 2))))

(define suite-datatypes
  (test-suite
   "datatypes: constructores, unicidad y sumar"

   ;construcción con los constructores del datatype
   (test-case "coeficientes y exponente"
     (check-true (dt:coeficiente-tad? (dt:coef-ent 4)))
     (check-true (dt:coeficiente-tad? (dt:coef-rac -3 2)))
     (check-true (dt:exponente-tad? (dt:expo-nat 5))))
   (test-case "termino"
     (check-true (dt:termino-tad? (dt:termino (dt:coef-rac -3 2) (dt:expo-nat 2)))))
   (test-case "terminos: sin-terminos y mas-terminos"
     (check-true (dt:terminos-tad? (dt:sin-terminos)))
     (check-true (dt:terminos-tad?
                  (dt:mas-terminos (dt:termino (dt:coef-ent 7) (dt:expo-nat 0))
                                   (dt:sin-terminos)))))
   (test-case "variable y poli armados a mano"
     (check-true (dt:variable-tad? (dt:nombre-var 'x)))
     (check-true
      (dt:polinomio-tad?
       (dt:poli (dt:nombre-var 'x)
                (dt:mas-terminos (dt:termino (dt:coef-ent 7) (dt:expo-nat 3))
                                 (dt:sin-terminos))))))
   (test-case "poli armado a mano coincide con el construido por la interfaz"
     (check-equal?
      (dt:poli (dt:nombre-var 'x)
               (dt:mas-terminos (dt:termino (dt:coef-ent 7) (dt:expo-nat 3))
                                (dt:sin-terminos)))
      (dt:insertar-termino (dt:polinomio-cero 'x) 7 3)))
   (test-case "los constructores rechazan campos de tipo incorrecto"
     (check-exn #rx"." (lambda () (dt:coef-ent 'a)))
     (check-exn #rx"." (lambda () (dt:termino 4 (dt:expo-nat 2)))))

   ;unicidad de la forma
   (test-case "unicidad: distinto orden de inserción, misma estructura"
     (check-equal? (dt-inserta-todo 'x '((1 2) (4 3) (2 5)))
                   (dt-inserta-todo 'x '((2 5) (1 2) (4 3))))
     (check-equal? dt-p
                   (dt:poli (dt:nombre-var 'x)
                            (dt:mas-terminos
                             (dt:termino (dt:coef-ent 4) (dt:expo-nat 5))
                             (dt:mas-terminos
                              (dt:termino (dt:coef-rac -3 2) (dt:expo-nat 2))
                              (dt:mas-terminos
                               (dt:termino (dt:coef-ent 7) (dt:expo-nat 0))
                               (dt:sin-terminos)))))))
   (test-case "unicidad: coeficiente cero no deja términos"
     (check-equal? (dt:insertar-termino dt-p 0 3) dt-p)
     (check-equal? (dt:insertar-termino (dt:polinomio-cero 'x) 0 3)
                   (dt:polinomio-cero 'x)))

   ;sumar
   (test-case "sumar: ejemplo del enunciado"
     (check-poli dt:coeficiente-de (dt:sumar dt-p dt-q) '((2 -1) (1 2) (0 7))))
   (test-case "sumar: el término de x^5 se cancela"
     (check-exn #rx"no tiene termino"
                (lambda () (dt:coeficiente-de (dt:sumar dt-p dt-q) 5))))
   (test-case "sumar: dos polinomios que se cancelan por completo"
     (let ((r (dt:sumar dt-p (dt-inserta-todo 'x '((5 -4) (2 3/2) (0 -7))))))
       (check-poli dt:coeficiente-de r '())
       (check-equal? r (dt:polinomio-cero 'x))))
   (test-case "sumar: ERROR con variables distintas"
     (check-exn #rx"misma variable"
                (lambda () (dt:sumar dt-p (dt-inserta-todo 'y '((1 1)))))))
   (test-case "sumar: con el polinomio nulo a ambos lados"
     (check-equal? (dt:sumar dt-p (dt:polinomio-cero 'x)) dt-p)
     (check-equal? (dt:sumar (dt:polinomio-cero 'x) dt-p) dt-p))
   (test-case "sumar: exponentes disjuntos se intercalan en orden"
     (check-poli dt:coeficiente-de
                 (dt:sumar (dt-inserta-todo 'x '((3 3) (0 1)))
                           (dt-inserta-todo 'x '((4 2) (1 5))))
                 '((4 2) (3 3) (1 5) (0 1))))
   (test-case "sumar: es conmutativa en el ejemplo"
     (check-equal? (dt:sumar dt-p dt-q) (dt:sumar dt-q dt-p)))
   (test-case "sumar: racionales que suman un entero"
     (check-equal? (dt:sumar (dt-inserta-todo 'x '((1 1/2)))
                             (dt-inserta-todo 'x '((1 1/2))))
                   (dt-inserta-todo 'x '((1 1)))))))


;ejecucion
(run-tests
 (test-suite
  "Taller 1: polinomios dispersos"
  (suite-interfaz "interfaz sobre LISTAS"
                  listas:polinomio-cero listas:insertar-termino
                  listas:coeficiente-de listas:eliminar-termino)
  (suite-interfaz "interfaz sobre PROCEDIMIENTOS"
                  procs:polinomio-cero procs:insertar-termino
                  procs:coeficiente-de procs:eliminar-termino)
  (suite-interfaz "interfaz sobre DATATYPES"
                  dt:polinomio-cero dt:insertar-termino
                  dt:coeficiente-de dt:eliminar-termino)
  suite-listas
  suite-procs
  suite-datatypes))