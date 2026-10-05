#lang eopl
;Autores: Josue David Cocoma Tascon 2477087, Juan Diego Montaño Vergara 2477334

;; Taller 1 — Polinomios dispersos.
;; Parte 3: representación con datatypes.
;;
;; Interfaz del TAD. Cada función va comentada con su nombre, su contrato
;; (entrada -> salida) y su propósito, y ninguna recorre la lista de términos
;; más de una vez ni la ordena al final.
;;
;;   polinomio-cero    : symbol -> polinomio
;;   insertar-termino  : polinomio x coeficiente x exponente -> polinomio
;;   coeficiente-de    : polinomio x exponente -> coeficiente
;;   eliminar-termino  : polinomio x exponente -> polinomio
;;   sumar             : polinomio x polinomio -> polinomio

(provide polinomio-cero insertar-termino coeficiente-de eliminar-termino sumar)

;; Datatypes: Se implementa uno por cada categoria gramatica. Se les pone al final -tad
;; para que ningun nombre de tipo sea igual al de una variante

(define-datatype exponente-tad exponente-tad?
  (expo-nat (k integer?)))

(define-datatype coeficiente-tad coeficiente-tad?
  (coef-ent (n integer?))
  (coef-rac (num integer?) (den integer?)))

(define-datatype termino-tad termino-tad?
  (termino (coef coeficiente-tad?) (expo exponente-tad?)))

(define-datatype terminos-tad terminos-tad?
  (sin-terminos)
  (mas-terminos (term termino-tad?) (resto terminos-tad?)))

(define-datatype variable-tad variable-tad?
  (nombre-var (s symbol?)))

(define-datatype polinomio-tad polinomio-tad?
  (poli (var variable-tad?) (terms terminos-tad?)))

;; Funciones auxiliares 

;; Pasa un número a coef-ent (si es entero) o coef-rac
(define a-coeficiente
  (lambda (c)
    (if (integer? c)
        (coef-ent c)
        (coef-rac (numerator c) (denominator c)))))

;; Pasa un coeficiente (coef-ent o coef-rac) a numero
(define de-coeficiente
  (lambda (c)
    (cases coeficiente-tad c
      (coef-ent (n) n)
      (coef-rac (num den) (/ num den)))))

;; Devuelve el exponente de un término como numero
(define expo-del-termino
  (lambda (t)
    (cases termino-tad t
      (termino (coef expo)
        (cases exponente-tad expo
          (expo-nat (k) k))))))

;; Devuelve el coeficiente de un término como numero
(define coef-del-termino
  (lambda (t)
    (cases termino-tad t
      (termino (coef expo) (de-coeficiente coef)))))

;; Devuelve el simbolo de una variable
(define nombre-de-variable
  (lambda (v)
    (cases variable-tad v
      (nombre-var (s) s))))

;; Funciones de la interfaz

;; Crea el polinomio nulo en la variable dada
(define polinomio-cero
  (lambda (variable)
    (poli (nombre-var variable) (sin-terminos))))

;; Inserta c*x^e en una lista decreciente recorriéndola una vez
;; Si c=0 no cambia; si el exponente existe, suma coeficientes y elimina el término si da 0
(define insertar-en-terminos
  (lambda (terms c e)
    (cases terminos-tad terms
      (sin-terminos ()
        (if (= c 0)
            (sin-terminos)
            (mas-terminos (termino (a-coeficiente c) (expo-nat e))
                          (sin-terminos))))
      (mas-terminos (primero resto)
        (let ((ex (expo-del-termino primero))
              (co (coef-del-termino primero)))
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

;; Agrega el termino coeficiente*x^exponente al polinomio y deja
;; el resultado ordenado y sin ceros, da error si el exponente es negativo
;; o si el coeficiente no es un número exacto
(define insertar-termino
  (lambda (polinomio coeficiente exponente)
    (cond
      ((not (and (integer? exponente) (exact? exponente) (>= exponente 0)))
       (eopl:error 'insertar-termino "El exponente debe ser un entero no negativo"))
      ((not (and (number? coeficiente) (exact? coeficiente)))
       (eopl:error 'insertar-termino "El coeficiente debe ser un numero exacto"))
      (else
       (cases polinomio-tad polinomio
         (poli (var terms)
           (poli var (insertar-en-terminos terms coeficiente exponente))))))))

;; Busca el termino con exponente e y devuelve su coeficiente
(define buscar-en-terminos
  (lambda (terms e)
    (cases terminos-tad terms
      (sin-terminos ()
        (eopl:error 'coeficiente-de "El polinomio no tiene termino con ese exponente"))
      (mas-terminos (primero resto)
        (if (= e (expo-del-termino primero))
            (coef-del-termino primero)
            (buscar-en-terminos resto e))))))

;; Devuelve el coeficiente del término que tiene ese exponente
(define coeficiente-de
  (lambda (polinomio exponente)
    (cases polinomio-tad polinomio
      (poli (var terms)
        (buscar-en-terminos terms exponente)))))

;; Devuelve la lista de términos sin el que tiene exponente e,
;; dejando los demás en el mismo orden
(define eliminar-de-terminos
  (lambda (terms e)
    (cases terminos-tad terms
      (sin-terminos ()
        (eopl:error 'eliminar-termino "El polinomio no tiene termino con ese exponente"))
      (mas-terminos (primero resto)
        (if (= e (expo-del-termino primero))
            resto
            (mas-terminos primero (eliminar-de-terminos resto e)))))))

;; Devuelve un polinomio nuevo sin el twrmino de ese exponente
(define eliminar-termino
  (lambda (polinomio exponente)
    (cases polinomio-tad polinomio
      (poli (var terms)
        (poli var (eliminar-de-terminos terms exponente))))))

;; Suma dos listas decrecientes recorriendolas en paralelo una sola vez,
;; si coinciden exponentes, suma coeficientes, si una se agota, anexa el resto.
(define sumar-terminos
  (lambda (a b)
    (cases terminos-tad a
      (sin-terminos () b)
      (mas-terminos (ta ra)
        (cases terminos-tad b
          (sin-terminos () a)
          (mas-terminos (tb rb)
            (let ((ea (expo-del-termino ta))
                  (eb (expo-del-termino tb)))
              (cond
                ((> ea eb)
                 (mas-terminos ta (sumar-terminos ra b)))
                ((< ea eb)
                 (mas-terminos tb (sumar-terminos a rb)))
                (else
                 (let ((suma (+ (coef-del-termino ta) (coef-del-termino tb))))
                   (if (= suma 0)
                       (sumar-terminos ra rb)
                       (mas-terminos (termino (a-coeficiente suma) (expo-nat ea))
                                     (sumar-terminos ra rb)))))))))))))

;; Devuelve la suma de dos polinomios, los dos deben estar en la
;; misma variable, si no, da error.
(define sumar
  (lambda (p q)
    (cases polinomio-tad p
      (poli (vp tp)
        (cases polinomio-tad q
          (poli (vq tq)
            (if (eqv? (nombre-de-variable vp) (nombre-de-variable vq))
                (poli vp (sumar-terminos tp tq))
                (eopl:error 'sumar "Los polinomios deben estar en la misma variable"))))))))
