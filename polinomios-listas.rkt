#lang eopl
;Autores: Josue David Cocoma Tascon 2477087, Juan Diego Montaño Vergara 2477334

;; Taller 1 — Polinomios dispersos.
;; Parte 1: representación basada en listas.
;;
;; Interfaz del TAD. Cada función va comentada con su nombre, su contrato
;; (entrada -> salida) y su propósito, y ninguna recorre la lista de términos
;; más de una vez ni la ordena al final.
;;
;;   polinomio-cero    : symbol -> polinomio
;;   insertar-termino  : polinomio x coeficiente x exponente -> polinomio
;;   coeficiente-de    : polinomio x exponente -> coeficiente
;;   eliminar-termino  : polinomio x exponente -> polinomio

(provide (all-defined-out))

;;Constructores y observadores:
 
;; Crea un polinomio con su variable y su lista de terminos
(define poli
  (lambda (var terms)
    (list 'poli var terms)))
 
;; Dice si x es un polinomio
(define poli?
  (lambda (x)
    (and (pair? x) (eqv? (car x) 'poli))))
 
;; Saca la variable del polinomio
(define poli->var
  (lambda (p)
    (car (cdr p))))
 
;; Saca la lista de terminos del polinomio
(define poli->terms
  (lambda (p)
    (car (cdr (cdr p)))))
 
;; Crea una variable
(define nombre-var
  (lambda (s)
    (list 'nombre-var s)))
 
;; Sice si x es una variable
(define nombre-var?
  (lambda (x)
    (and (pair? x) (eqv? (car x) 'nombre-var))))
 
;; Saca el simbolo de la variable
(define nombre-var->s
  (lambda (v)
    (car (cdr v))))
 
;; Crea la lista vacia de terminos
(define sin-terminos
  (lambda ()
    (list 'sin-terminos)))
 
;; Dice si x es la lista vacia
(define sin-terminos?
  (lambda (x)
    (and (pair? x) (eqv? (car x) 'sin-terminos))))
 
;; Crea una lista de terminos con un primer termino y el resto.
(define mas-terminos
  (lambda (term resto)
    (list 'mas-terminos term resto)))
 
;; Dice si x es una lista de terminos que no esta vacia
(define mas-terminos?
  (lambda (x)
    (and (pair? x) (eqv? (car x) 'mas-terminos))))
 
;; Saca el primer termino de la lista
(define mas-terminos->term
  (lambda (ts)
    (car (cdr ts))))
 
;; Saca el resto de la lista
(define mas-terminos->resto
  (lambda (ts)
    (car (cdr (cdr ts)))))
 
;; Crea un termino
(define termino
  (lambda (coef expo)
    (list 'termino coef expo)))
 
;; Dice si x es un termino
(define termino?
  (lambda (x)
    (and (pair? x) (eqv? (car x) 'termino))))
 
;; Saca el coeficiente del termino
(define termino->coef
  (lambda (t)
    (car (cdr t))))
 
;; Saca el exponente del termino
(define termino->expo
  (lambda (t)
    (car (cdr (cdr t)))))
 
;; Crea un coeficiente entero
(define coef-ent
  (lambda (n)
    (list 'coef-ent n)))
 
;; Dice si x es un coeficiente entero
(define coef-ent?
  (lambda (x)
    (and (pair? x) (eqv? (car x) 'coef-ent))))
 
;; Saca el entero del coeficiente
(define coef-ent->n
  (lambda (c)
    (car (cdr c))))
 
;; Crea un coeficiente racional con numerador y denominador
(define coef-rac
  (lambda (num den)
    (list 'coef-rac num den)))
 
;; Dice si x es un coeficiente racional
(define coef-rac?
  (lambda (x)
    (and (pair? x) (eqv? (car x) 'coef-rac))))
 
; Saca el numerador del coeficiente racional
(define coef-rac->num
  (lambda (c)
    (car (cdr c))))
 
;; Saca el denominador del coeficiente racional
(define coef-rac->den
  (lambda (c)
    (car (cdr (cdr c)))))
 
;; Crea un exponente
(define expo-nat
  (lambda (k)
    (list 'expo-nat k)))
 
;; Dice si x es un exponente
(define expo-nat?
  (lambda (x)
    (and (pair? x) (eqv? (car x) 'expo-nat))))
 
;; Saca el entero del exponente
(define expo-nat->k
  (lambda (e)
    (car (cdr e))))

;; Funciones de la interfaz:

;; Pasa un numero a coef-ent si es entero o coef-rac
(define a-coeficiente
  (lambda (c)
    (if (integer? c)
        (coef-ent c)
        (coef-rac (numerator c) (denominator c)))))

;; Pasa un coeficiente coef-ent o coef-rac a numero 
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

;; Agrega un termino al polinomio manteniendo el orden decreciente y sin coeficientes cero
(define insertar-termino
  (lambda (polinomio coeficiente exponente)
    (cond
      ((not (and (integer? exponente) (exact? exponente) (>= exponente 0)))
       (eopl:error 'insertar-termino "El exponente debe ser un entero no negativo"))
      ((not (and (number? coeficiente) (real? coeficiente) (exact? coeficiente)))
       (eopl:error 'insertar-termino "El coeficiente debe ser un numero exacto"))
      (else
       (poli (poli->var polinomio)
             (insertar-en-terminos (poli->terms polinomio) coeficiente exponente))))))

;; Busca el termino con exponente e y devuelve su coeficiente
(define buscar-en-terminos
  (lambda (terms e)
    (if (sin-terminos? terms)
        (eopl:error 'coeficiente-de "El polinomio no tiene termino con ese exponente")
        (let ((primero (mas-terminos->term terms)))
          (if (= e (expo-nat->k (termino->expo primero)))
              (de-coeficiente (termino->coef primero))
              (buscar-en-terminos (mas-terminos->resto terms) e))))))

;; Devuelve el coeficiente del termino que tiene ese exponente
(define coeficiente-de
  (lambda (polinomio exponente)
    (buscar-en-terminos (poli->terms polinomio) exponente)))

;; Elimina el termino con exponente e recorriendo la lista una sola vez y manteniendo el orden
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
