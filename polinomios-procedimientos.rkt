#lang eopl
;Autores: Josue David Cocoma Tascon 2477087, Juan Diego Montaño Vergara 2477334

;; Taller 1 — Polinomios dispersos.
;; Parte 2: representación basada en procedimientos.
;;
;; Interfaz del TAD. Cada función va comentada con su nombre, su contrato
;; (entrada -> salida) y su propósito, y ninguna recorre la lista de términos
;; más de una vez ni la ordena al final.
;;
;;   polinomio-cero    : symbol -> polinomio
;;   insertar-termino  : polinomio x coeficiente x exponente -> polinomio
;;   coeficiente-de    : polinomio x exponente -> coeficiente
;;   eliminar-termino  : polinomio x exponente -> polinomio

(provide polinomio-cero insertar-termino coeficiente-de eliminar-termino)

;;Constructores y observadores:

;; Crea un polinomio con su variable y su lista de terminos
(define poli
  (lambda (var terms)
    (lambda (mensaje)
      (cond
        ((eqv? mensaje 'tipo) 'poli)
        ((eqv? mensaje 'poli->var) var)
        ((eqv? mensaje 'poli->terms) terms)
        (else #f)))))
 
;; Dice si x es un polinomio
(define poli?
  (lambda (x)
    (and (procedure? x) (eqv? (x 'tipo) 'poli))))
 
;; Saca la variable del polinomio
(define poli->var
  (lambda (p)
    (p 'poli->var)))
 
;; Saca la lista de trrminos del polinomio
(define poli->terms
  (lambda (p)
    (p 'poli->terms)))
 
;; Crea una variable
(define nombre-var
  (lambda (s)
    (lambda (mensaje)
      (cond
        ((eqv? mensaje 'tipo) 'nombre-var)
        ((eqv? mensaje 'nombre-var->s) s)
        (else #f)))))
 
;; Dice si x es una variable
(define nombre-var?
  (lambda (x)
    (and (procedure? x) (eqv? (x 'tipo) 'nombre-var))))
 
;; Saca el símbolo de la variable
(define nombre-var->s
  (lambda (v)
    (v 'nombre-var->s)))
 
;; Crea la lista vacía de terminos
(define sin-terminos
  (lambda ()
    (lambda (mensaje)
      (cond
        ((eqv? mensaje 'tipo) 'sin-terminos)
        (else #f)))))
 
;; Dice si x es la lista vacía de terminos
(define sin-terminos?
  (lambda (x)
    (and (procedure? x) (eqv? (x 'tipo) 'sin-terminos))))
 
;; Crea una lista de terminos con un primer término y el resto
(define mas-terminos
  (lambda (term resto)
    (lambda (mensaje)
      (cond
        ((eqv? mensaje 'tipo) 'mas-terminos)
        ((eqv? mensaje 'mas-terminos->term) term)
        ((eqv? mensaje 'mas-terminos->resto) resto)
        (else #f)))))
 
;; Dice si x es una lista de terminos que no esta vacía
(define mas-terminos?
  (lambda (x)
    (and (procedure? x) (eqv? (x 'tipo) 'mas-terminos))))
 
;; saca el primer termino de la lista
(define mas-terminos->term
  (lambda (ts)
    (ts 'mas-terminos->term)))
 
;; Saca el resto de la lista 
(define mas-terminos->resto
  (lambda (ts)
    (ts 'mas-terminos->resto)))
 
;; Crea un término
(define termino
  (lambda (coef expo)
    (lambda (mensaje)
      (cond
        ((eqv? mensaje 'tipo) 'termino)
        ((eqv? mensaje 'termino->coef) coef)
        ((eqv? mensaje 'termino->expo) expo)
        (else #f)))))
 
;; Dice si x es un termino
(define termino?
  (lambda (x)
    (and (procedure? x) (eqv? (x 'tipo) 'termino))))
 
;; Saca el coeficiente del termino
(define termino->coef
  (lambda (t)
    (t 'termino->coef)))
 
;; Saca el exponente del termino
(define termino->expo
  (lambda (t)
    (t 'termino->expo)))
 
;; Crea un coeficiente entero
(define coef-ent
  (lambda (n)
    (lambda (mensaje)
      (cond
        ((eqv? mensaje 'tipo) 'coef-ent)
        ((eqv? mensaje 'coef-ent->n) n)
        (else #f)))))
 
;; Dice si x es un coeficiente entero
(define coef-ent?
  (lambda (x)
    (and (procedure? x) (eqv? (x 'tipo) 'coef-ent))))
 
;; Saca el entero del coeficiente
(define coef-ent->n
  (lambda (c)
    (c 'coef-ent->n)))
 
;; Crea un coeficiente racional con numerador y denominador
(define coef-rac
  (lambda (num den)
    (lambda (mensaje)
      (cond
        ((eqv? mensaje 'tipo) 'coef-rac)
        ((eqv? mensaje 'coef-rac->num) num)
        ((eqv? mensaje 'coef-rac->den) den)
        (else #f)))))
 
;; Dice si x es un coeficiente racional
(define coef-rac?
  (lambda (x)
    (and (procedure? x) (eqv? (x 'tipo) 'coef-rac))))
 
;; Saca el numerador del coeficiente racional
(define coef-rac->num
  (lambda (c)
    (c 'coef-rac->num)))
 
;; Saca el denominador del coeficiente racional
(define coef-rac->den
  (lambda (c)
    (c 'coef-rac->den)))
 
;; Crea un exponente
(define expo-nat
  (lambda (k)
    (lambda (mensaje)
      (cond
        ((eqv? mensaje 'tipo) 'expo-nat)
        ((eqv? mensaje 'expo-nat->k) k)
        (else #f)))))
 
;; Dice si x es un exponente
(define expo-nat?
  (lambda (x)
    (and (procedure? x) (eqv? (x 'tipo) 'expo-nat))))
 
;; Saca el entero del exponente
(define expo-nat->k
  (lambda (e)
    (e 'expo-nat->k)))
 
;;  Funciones de la interfaz 
 
;; Pasa un número a coef-ent o coef-rac.
(define a-coeficiente
  (lambda (c)
    (if (integer? c)
        (coef-ent c)
        (coef-rac (numerator c) (denominator c)))))
 
;; Pasa un coeficiente coef-ent o coef-rac a número
(define de-coeficiente
  (lambda (c)
    (if (coef-ent? c)
        (coef-ent->n c)
        (/ (coef-rac->num c) (coef-rac->den c)))))
 
;; Crea el polinomio nulo
(define polinomio-cero
  (lambda (variable)
    (poli (nombre-var variable) (sin-terminos))))
 
;; Inserta c*x^e en una lista decreciente recorriéndola una sola vez,
;; si c=0 no cambia, si el exponente existe, suma coeficientes
(define insertar-en-terminos
  (lambda (terms c e)
    (if (sin-terminos? terms)
        (if (= c 0)
            (sin-terminos)
            (mas-terminos (termino (a-coeficiente c) (expo-nat e))
                          (sin-terminos)))
        (let ((primero (mas-terminos->term terms))
              (resto (mas-terminos->resto terms)))
          (let ((ex (expo-nat->k (termino->expo primero)))
                (co (de-coeficiente (termino->coef primero))))
            (cond
              ((> e ex)
               (if (= c 0)
                   terms
                   (mas-terminos (termino (a-coeficiente c) (expo-nat e)) terms)))
              ((= e ex)
               (if (= (+ c co) 0)
                   resto
                   (mas-terminos (termino (a-coeficiente (+ c co)) (expo-nat e))
                                 resto)))
              (else
               (mas-terminos primero (insertar-en-terminos resto c e)))))))))
 
;; Agrega un término manteniendo el orden decreciente y sin coeficientes cero,
;; lanza error si el exponente no es entero positivo o el coeficiente no es exacto
(define insertar-termino
  (lambda (polinomio coeficiente exponente)
    (cond
      ((not (and (integer? exponente) (exact? exponente) (>= exponente 0)))
       (eopl:error 'insertar-termino "El exponente debe ser un entero no negativo"))
      ((not (and (number? coeficiente) (exact? coeficiente)))
       (eopl:error 'insertar-termino "El coeficiente debe ser un numero exacto"))
      (else
       (poli (poli->var polinomio)
             (insertar-en-terminos (poli->terms polinomio) coeficiente exponente))))))
 
;; Busca el término con exponente e y devuelve su coeficiente
(define buscar-en-terminos
  (lambda (terms e)
    (if (sin-terminos? terms)
        (eopl:error 'coeficiente-de "El polinomio no tiene termino con ese exponente")
        (let ((primero (mas-terminos->term terms)))
          (if (= e (expo-nat->k (termino->expo primero)))
              (de-coeficiente (termino->coef primero))
              (buscar-en-terminos (mas-terminos->resto terms) e))))))
 
;; Devuelve el coeficiente del término que tiene ese exponente
(define coeficiente-de
  (lambda (polinomio exponente)
    (buscar-en-terminos (poli->terms polinomio) exponente)))
 
;; Devuelve la lista de términos sin el que tiene exponente e, dejando los demás en el mismo orden
(define eliminar-de-terminos
  (lambda (terms e)
    (if (sin-terminos? terms)
        (eopl:error 'eliminar-termino "El polinomio no tiene termino con ese exponente")
        (let ((primero (mas-terminos->term terms))
              (resto (mas-terminos->resto terms)))
          (if (= e (expo-nat->k (termino->expo primero)))
              resto
              (mas-terminos primero (eliminar-de-terminos resto e)))))))
 
;; Devuelve un polinomio nuevo sin el término de ese exponente
(define eliminar-termino
  (lambda (polinomio exponente)
    (poli (poli->var polinomio)
          (eliminar-de-terminos (poli->terms polinomio) exponente))))
 