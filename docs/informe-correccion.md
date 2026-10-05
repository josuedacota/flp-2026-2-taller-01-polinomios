# Informe de corrección — Taller 1: polinomios dispersos

**Curso:** Fundamentos de Interpretación y Compilación de Lenguajes
de Programación — Universidad del Valle, Sede Tuluá.

**Integrantes del grupo:**

| Nombre | Código | Correo institucional |
|--------|--------|----------------------|
| Josue David Cocoma Tascon | 2477087 | josue.cocoma@correounivalle.edu.co |
| Juan Diego Montaño Vergara | 2477334 | juan.diego.montano@correounivalle.edu.co |

---

## 1. Marco formal

### 1.1 Corrección de programas recursivos

Sea $f : A \to B$ una función y $A$ un conjunto definido
recursivamente. Sea $P_f$ un programa recursivo en Racket que pretende
calcular $f$. Decimos que $P_f$ es correcto con respecto a su
especificación si se cumple:

$$
\forall a \in A \,:\, P_f(a) = f(a)
$$

La estrategia de demostración es **inducción estructural** sobre $A$.
Aquí $A$ es el conjunto de listas de términos que genera la gramática:

- **Caso base:** $a = \text{sin-terminos}()$, y se verifica
  $P_f(a) = f(a)$ directamente.
- **Caso inductivo:** $a = \text{mas-terminos}(t, r)$. Se asume la
  **hipótesis de inducción** $P_f(r) = f(r)$ sobre el resto de la
  lista y se demuestra $P_f(a) = f(a)$.

Las tres funciones analizadas (`buscar-en-terminos`,
`eliminar-de-terminos` e `insertar-en-terminos`) están escritas con
recursión estructural directa, sin acumuladores; por eso la hipótesis de
inducción basta y no se necesita invariante de acumulador.

**Notación.** Para una lista de términos $L$:

- $|L|$ es su longitud y $L = t :: r$ significa
  $\text{mas-terminos}(t, r)$; $[\,]$ es $\text{sin-terminos}()$.
- $\mathrm{exps}(L)$ es el conjunto de exponentes que aparecen en $L$.
- $\mathrm{val}(c)$ es el valor concreto (número exacto de Racket) de un
  nodo de coeficiente: $\mathrm{val}(\text{coef-ent}(n)) = n$ y
  $\mathrm{val}(\text{coef-rac}(a, b)) = a/b$.

Un resultado auxiliar usado en todas las demostraciones:

> **(F1)** Para todo racional exacto $q$, $\mathrm{val}(\texttt{a-coeficiente}(q)) = q$
> y el nodo producido cumple $\mathrm{red}$.
>
> *Prueba.* Si $q$ es entero se construye $\text{coef-ent}(q)$ y
> $\mathrm{val} = q$ (denominador implícito $1$). Si no, se construye
> $\text{coef-rac}(\texttt{numerator}(q), \texttt{denominator}(q))$. Racket
> garantiza que los racionales exactos están en forma reducida y con
> denominador positivo, así que $\mathrm{red}$ se cumple, y
> $\mathrm{val} = \texttt{numerator}(q)/\texttt{denominator}(q) = q$. $\square$

### 1.2 El invariante de la representación

Las cuatro condiciones del enunciado se enuncian como una única
propiedad sobre polinomios. Sea $p$ un polinomio con términos
$t_1, t_2, \ldots, t_n$, donde $t_i = (c_i, e_i)$:

$$
\mathrm{Inv}(p) \equiv
\underbrace{\forall i < n : e_i > e_{i+1}}_{\text{orden estricto}}
\ \land\
\underbrace{\forall i : c_i \neq 0}_{\text{sin ceros}}
\ \land\
\underbrace{\forall i : e_i \in \mathbb{N}}_{\text{exponentes naturales}}
\ \land\
\underbrace{\forall i : \mathrm{red}(c_i)}_{\text{racionales reducidos}}
$$

donde $\mathrm{red}\left(\frac{a}{b}\right)$ abrevia
$b > 0 \,\land\, \mathrm{mcd}(|a|, b) = 1$, y un coeficiente entero se
toma como el racional de denominador $1$.

La variable no interviene en el invariante, de modo que
$\mathrm{Inv}(p) \equiv \mathrm{Inv}_L(\text{terms}(p))$, donde
$\mathrm{Inv}_L$ es la misma propiedad aplicada directamente a una lista
de términos. Cada condición se verifica término por término sobre
$L = [t_1, \ldots, t_n]$. Dos consecuencias inmediatas que se usan en las
pruebas:

- **(S)** Si $\mathrm{Inv}_L(t :: r)$, entonces $\mathrm{Inv}_L(r)$ (un sufijo
  hereda las cuatro condiciones) y todo exponente de $r$ es menor que el
  exponente de $t$.
- **(U)** Por el orden estricto, en $L$ hay **a lo sumo un** término con un
  exponente dado. Por eso "el término de exponente $e$" está bien definido.

---

## 2. Funciones analizadas

El código de las tres funciones es idéntico en la representación con
listas y en la de procedimientos (solo usan constructores y observadores
de la gramática), y en datatypes tiene la misma estructura con `cases`.
Las demostraciones se hacen una vez sobre la estructura recursiva.

### 2.1 Corrección de `coeficiente-de`

**Especificación.**

- **Tipo:** `coeficiente-de : polinomio × exponente -> coeficiente`
- **Pre-condición:** $\mathrm{Inv}(p)$ y $e \in \mathbb{N}$ (el exponente
  consultado es un entero no negativo).
- **Post-condición:** $\text{Post}(p, e, r) \equiv \big(\exists c :
  (c, e) \in \text{terms}(p)\big) \Rightarrow r = \mathrm{val}(c)$ cuando
  el exponente $e$ aparece en $p$; y la función levanta
  `eopl:error` cuando no aparece. Por **(U)** el $c$ es único, así que la
  especificación define una función.

**Código.**

```racket
; coeficiente-de : polinomio × exponente -> coeficiente (número exacto)
; Propósito: devuelve el coeficiente concreto del término de exponente e;
;            error si el polinomio no tiene ese exponente.
(define buscar-en-terminos
  (lambda (terms e)
    (if (sin-terminos? terms)
        (eopl:error 'coeficiente-de "El polinomio no tiene termino con ese exponente")
        (let ((primero (mas-terminos->term terms)))
          (if (= e (expo-nat->k (termino->expo primero)))
              (de-coeficiente (termino->coef primero))
              (buscar-en-terminos (mas-terminos->resto terms) e))))))

(define coeficiente-de
  (lambda (polinomio exponente)
    (buscar-en-terminos (poli->terms polinomio) exponente)))
```

Como `coeficiente-de(p, e) = buscar-en-terminos(terms(p), e)`, basta
demostrar la siguiente propiedad sobre listas.

> **Lema 1.** Para toda lista $L$ con $\mathrm{Inv}_L(L)$ y todo $e \in
> \mathbb{N}$: si $(c, e) \in L$ entonces $B(L, e) = \mathrm{val}(c)$; y si
> $e \notin \mathrm{exps}(L)$ entonces $B(L, e)$ levanta el error.
> (Aquí $B$ es `buscar-en-terminos`.)

**Demostración** (inducción estructural sobre $L$).

- **Caso base** ($L = [\,]$): ningún término tiene exponente $e$, así que
  solo hay que probar la segunda cláusula. La rama `sin-terminos?` del
  programa es exactamente la que se toma y ejecuta
  `eopl:error 'coeficiente-de "El polinomio no tiene termino con ese exponente"`.

  $$
  B([\,], e) = \texttt{error} \qquad \text{y} \qquad \mathrm{exps}([\,]) = \emptyset \not\ni e
  $$

- **Caso inductivo** ($L = t :: r$ con $t = (c_0, e_0)$): se toma la rama
  `mas-terminos?`. Por **(S)**, $\mathrm{Inv}_L(r)$, de modo que la
  hipótesis de inducción (HI) aplica a $r$. Hay dos subcasos según la
  comparación $e = e_0$:

  - **$e = e_0$.** El programa devuelve
    `(de-coeficiente (termino->coef primero))`, es decir $\mathrm{val}(c_0)$.
    El término $(c_0, e)$ pertenece a $L$ y, por **(U)**, es el único con ese
    exponente; luego el resultado es el esperado.
  - **$e \neq e_0$.** El programa devuelve $B(r, e)$. Como $t$ no tiene
    exponente $e$, los términos de $L$ con exponente $e$ son exactamente
    los de $r$ con exponente $e$ (y $e \in \mathrm{exps}(L)
    \iff e \in \mathrm{exps}(r)$). Por HI: si $(c, e) \in r$, entonces
    $B(r, e) = \mathrm{val}(c)$; y si $e \notin \mathrm{exps}(r)$, entonces
    $B(r, e)$ levanta el error. En ambos casos se cumple la propiedad para
    $L$.

  $$
  B(t :: r, e) =
  \begin{cases}
  \mathrm{val}(c_0) & \text{si } e = e_0 \\
  B(r, e) & \text{si } e \neq e_0
  \end{cases}
  $$

  **Sobre cortar la búsqueda.** El orden estricto del invariante permite
  una tercera rama: si $e > e_0$, entonces por **(S)** $e$ es mayor que
  todos los exponentes de $r$, de modo que $e \notin \mathrm{exps}(L)$ y
  se podría levantar el error sin recorrer el resto. La implementación del
  grupo no usa ese atajo (compara solo por igualdad y sigue hasta
  `sin-terminos`); es correcta igualmente, porque el caso base cubre la
  ausencia, y recorre la lista a lo sumo una vez. El atajo sería una
  optimización sin cambiar la especificación.

- **Levantamiento del error.** El único lugar del programa que llama a
  `eopl:error` es la rama `sin-terminos?`. Las llamadas recursivas solo
  se hacen cuando la comparación $e = e_0$ falló. Por inducción sobre
  $|L|$: $B(L, e)$ alcanza el caso base si y solo si $e \neq e_i$ para
  todo $i$, es decir, si y solo si $e \notin \mathrm{exps}(L)$. Por lo
  tanto el error se levanta cuando el exponente no está y **solo** en ese
  caso.

- **Terminación.** Medida $\mu(L) = |L| \in \mathbb{N}$. La única llamada
  recursiva es sobre $r$ con $|r| = |L| - 1 < |L|$; cuando $|L| = 0$ no hay
  llamada recursiva. Como $\mu$ decrece estrictamente y tiene cota
  inferior $0$ en un orden bien fundado, la recursión termina en a lo sumo
  $|L| + 1$ llamadas.

**Conclusión:** Por el Lema 1 aplicado a $L = \text{terms}(p)$ (que cumple
$\mathrm{Inv}_L$ por la pre-condición), `coeficiente-de` retorna el
coeficiente buscado cuando el exponente existe y levanta el error exactamente
cuando no existe, y siempre termina. La pre-condición $e \in \mathbb{N}$
garantiza que `=` no falla por un argumento que no sea número. $\blacksquare$

---

### 2.2 Corrección de `eliminar-termino`

**Especificación.**

- **Tipo:** `eliminar-termino : polinomio × exponente -> polinomio`
- **Pre-condición:** $\mathrm{Inv}(p)$ y $e \in \mathbb{N}$.
- **Post-condición:** el resultado contiene **exactamente** los
  términos de $p$ menos el de exponente $e$. Formalmente, si $t_e$ es el
  único término de $p$ con exponente $e$ (por **(U)**):
  $$
  \text{terminos}(r) = \text{terminos}(p) \setminus \{t_e\}
  $$
  con los términos restantes **en el mismo orden relativo**, y la función
  levanta `eopl:error` si $e$ no aparece en $p$. La variable del resultado
  es la de $p$.

**Código.**

```racket
; eliminar-termino : polinomio × exponente -> polinomio
; Propósito: devuelve un polinomio nuevo sin el término de exponente e;
;            error si ese exponente no existe.
(define eliminar-de-terminos
  (lambda (terms e)
    (if (sin-terminos? terms)
        (eopl:error 'eliminar-termino "El polinomio no tiene termino con ese exponente")
        (let ((primero (mas-terminos->term terms))
              (resto (mas-terminos->resto terms)))
          (if (= e (expo-nat->k (termino->expo primero)))
              resto
              (mas-terminos primero (eliminar-de-terminos resto e)))))))

(define eliminar-termino
  (lambda (polinomio exponente)
    (poli (poli->var polinomio)
          (eliminar-de-terminos (poli->terms polinomio) exponente))))
```

> **Lema 2.** Para toda lista $L$ con $\mathrm{Inv}_L(L)$ y $e \in
> \mathbb{N}$, con $E$ = `eliminar-de-terminos`: si $e \in
> \mathrm{exps}(L)$ entonces $E(L, e) = L \setminus \{t_e\}$ (la lista $L$
> sin su único término de exponente $e$, preservando el orden de los
> demás); si $e \notin \mathrm{exps}(L)$, $E(L, e)$ levanta el error.

**Demostración** (inducción estructural sobre $L$).

- **Caso base** ($L = [\,]$): $e \notin \emptyset$ y el programa toma la
  rama `sin-terminos?` y levanta el error. Se cumple la segunda cláusula.
- **Caso inductivo** ($L = t :: r$, $t = (c_0, e_0)$). Por **(S)**
  $\mathrm{Inv}_L(r)$ y vale la HI sobre $r$.
  - **$e = e_0$.** El programa devuelve $r$. Por **(U)**, $t$ es el único
    término de exponente $e$, y $r = L \setminus \{t\}$ con el orden
    intacto.
  - **$e \neq e_0$.** El programa devuelve $t :: E(r, e)$.
    - Si $e \in \mathrm{exps}(r)$, por HI $E(r, e) = r \setminus \{t_e\}$ y
      entonces $t :: E(r, e) = (t :: r) \setminus \{t_e\}$, con el orden
      relativo intacto.
    - Si $e \notin \mathrm{exps}(r)$, como además $e \neq e_0$ se tiene
      $e \notin \mathrm{exps}(L)$, y por HI $E(r, e)$ levanta el error, que
      se propaga (el `mas-terminos` no se alcanza a construir).

  $$
  E(t :: r, e) =
  \begin{cases}
  r & \text{si } e = e_0 \\
  t :: E(r, e) & \text{si } e \neq e_0
  \end{cases}
  $$

- **Levantamiento del error.** Igual que en 2.1: el único `eopl:error` es
  el de la rama `sin-terminos?`, a la que solo se llega tras comprobar que
  ningún exponente de $L$ es $e$. El error se levanta si y solo si
  $e \notin \mathrm{exps}(L)$.
- **Terminación.** Medida $\mu(L) = |L|$: la única llamada recursiva es
  sobre $r$, con $|r| = |L| - 1$, y para $|L| = 0$ no hay llamada.

**El resultado conserva el invariante.** Por el Lema 2 el resultado $R$ es
una subsucesión de $L$ (se eliminó, a lo sumo, un elemento y no se alteró
ningún otro). Entonces: (i) una subsucesión de una sucesión de exponentes
estrictamente decreciente es estrictamente decreciente, así que el orden
estricto se conserva; (ii) no se creó ningún coeficiente, por lo que no
aparece ningún cero; (iii) los exponentes restantes siguen en $\mathbb{N}$;
(iv) los coeficientes restantes siguen reducidos. Luego
$\mathrm{Inv}_L(R)$ y, como la variable no cambia, $\mathrm{Inv}(r)$.

**Conclusión:** `eliminar-termino` devuelve un polinomio con exactamente los
términos de $p$ menos el de exponente $e$, levanta el error si y solo si $e$
no aparece, termina, y su resultado cumple $\mathrm{Inv}$. $\blacksquare$

---

### 2.3 `insertar-termino` preserva el invariante

**Enunciado.** Si $\mathrm{Inv}(p)$ vale antes de la llamada, entonces
$\mathrm{Inv}(\texttt{insertar-termino}(p, c, e))$ vale sobre el
resultado.

**Código.**

```racket
; insertar-termino : polinomio × coeficiente × exponente -> polinomio
; Propósito: inserta c·x^e conservando orden estricto y ausencia de ceros;
;            error si e no es entero no negativo o c no es real exacto.
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

(define insertar-en-terminos
  (lambda (terms c e)
    (if (sin-terminos? terms)
        (if (= c 0)
            (sin-terminos)
            (mas-terminos (termino (a-coeficiente c) (expo-nat e)) (sin-terminos)))
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
                   (mas-terminos (termino (a-coeficiente (+ c co)) (expo-nat e)) resto)))
              (else
               (mas-terminos primero (insertar-en-terminos resto c e)))))))))
```

Las dos primeras cláusulas de `insertar-termino` validan $e \in \mathbb{N}$
y que $c$ sea un real exacto (un racional) **antes** de tocar el polinomio;
por lo tanto, cuando se llega a `insertar-en-terminos`, se cumplen sus
pre-condiciones. Si se levanta el error no hay resultado y no hay nada que
preservar. Notamos $I(L, c, e)$ al resultado de `insertar-en-terminos` y
demostramos una propiedad un poco más fuerte, necesaria para que la
inducción funcione:

> **Lema 3.** Si $\mathrm{Inv}_L(L)$, $c$ es un racional exacto y
> $e \in \mathbb{N}$, entonces para $R = I(L, c, e)$:
> **(a)** $\mathrm{Inv}_L(R)$ y **(b)** $\mathrm{exps}(R) \subseteq
> \mathrm{exps}(L) \cup \{e\}$.

La parte (b) se necesita en el paso recursivo: sin ella no se sabría que
el resultado de la llamada recursiva no introduce exponentes mayores que
el del primer término.

**Demostración por casos.** Se hace inducción estructural sobre $L$. Las
ramas del programa se corresponden con los tres casos del enunciado así:
el caso A ocurre en las ramas $L = [\,]$ y $e > e_0$; el caso B y el caso C
ocurren en la rama $e = e_0$ (según $c + c_0 \neq 0$ o $= 0$); y la rama
$e < e_0$ se limita a delegar en la llamada recursiva, donde se
decide cuál de los tres casos corresponde. Por **(U)**, los tres casos son
mutuamente excluyentes y exhaustivos.

- **Caso A — el exponente es nuevo** ($e \notin \mathrm{exps}(L)$).
  - *Base, $L = [\,]$.* Si $c = 0$, el resultado es $[\,]$, que cumple
    $\mathrm{Inv}_L$ de forma vacía. Si $c \neq 0$, el resultado es
    $[(c, e)]$: un solo término, así que el orden estricto se cumple de
    forma vacía; $c \neq 0$; $e \in \mathbb{N}$; y $\mathrm{red}(c)$ por
    **(F1)**, porque el nodo lo construye `a-coeficiente`.
  - *Paso, rama $e > e_0$.* Como $e \notin \mathrm{exps}(L)$ y $e$ es mayor
    que el primer exponente $e_0$, que por **(S)** es el mayor de $L$,
    el exponente es nuevo y debe ir al **inicio**. Si $c = 0$ el programa
    devuelve $L$ intacta: no se inserta nada, no se crea ningún cero, y
    $\mathrm{Inv}_L(L)$ es la hipótesis. Si $c \neq 0$, el resultado es
    $(c, e) :: L$. Orden estricto: $e > e_0 > e_1 > \cdots$, así que se
    conserva. Sin ceros: $c \neq 0$ y los demás son los de $L$.
    Exponentes naturales: $e \in \mathbb{N}$. Reducidos: $\mathrm{red}(c)$
    por **(F1)** y los demás son los de $L$. Además
    $\mathrm{exps}(R) = \mathrm{exps}(L) \cup \{e\}$, lo que da **(b)**.
  - *Paso, rama $e < e_0$ (el exponente sigue siendo nuevo).* El resultado
    es $t_0 :: R'$ con $R' = I(r, c, e)$. Por **(S)** $\mathrm{Inv}_L(r)$,
    y por HI vale **(a)** y **(b)** para $R'$. Falta ver que $e_0$ es mayor
    que todo exponente de $R'$: por HI(b) cada uno de ellos está en
    $\mathrm{exps}(r)$ (menor que $e_0$ por **(S)**) o es $e$ (menor que
    $e_0$ por la rama). Por lo tanto el orden estricto vale para
    $t_0 :: R'$; $t_0$ mantiene $c_0 \neq 0$, exponente natural y
    $\mathrm{red}(c_0)$; los demás cumplen lo suyo por HI. Para **(b)**:
    $\mathrm{exps}(R) \subseteq \{e_0\} \cup \mathrm{exps}(r) \cup \{e\} =
    \mathrm{exps}(L) \cup \{e\}$.

  *Qué pasa si el coeficiente que llega es cero.* Con $e$ nuevo, nunca se
  inserta un término: el resultado es $L$ (o $[\,]$). Esa es la razón por
  la que la inserción con coeficiente $0$ no altera el polinomio y no puede
  introducir un término de coeficiente cero.

- **Caso B — el exponente ya existía y la suma no es cero.** Se llega a la
  rama $e = e_0$ en la llamada que tiene a $t_0 = (c_0, e)$ como primer
  elemento (única por **(U)**), con $s = c + c_0 \neq 0$. El resultado de
  esa llamada es $(s, e) :: r$, y las llamadas anteriores solo antepusieron
  términos con exponente mayor (rama $e < e_0$, tratada arriba).
  - *Orden estricto:* el exponente $e$ del nuevo primer término es el mismo
    que el del término reemplazado, así que sigue siendo mayor que todos los
    de $r$ (**(S)**) y menor que los anteriores; el orden no cambia.
  - *Sin ceros:* $s \neq 0$ por hipótesis del caso; los demás términos son
    los originales.
  - *Exponentes naturales:* el exponente no cambió.
  - *Racionales reducidos:* $s = c + c_0$ es una suma de racionales exactos,
    luego un racional exacto, y el nodo $(a\text{-}coeficiente\ s)$ cumple
    $\mathrm{red}$ por **(F1)** (Racket normaliza la suma: denominador
    positivo y fracción reducida). Además, $\mathrm{val}$ del nodo es
    exactamente $s$.
  - **(b)** se cumple: $\mathrm{exps}(R) = \mathrm{exps}(L)$.

  Este caso incluye $c = 0$ con $e$ ya existente: entonces $s = c_0 \neq 0$
  y el término se reemplaza por uno idéntico; el polinomio no cambia.

- **Caso C — el exponente ya existía y la suma es cero.** En la rama
  $e = e_0$ con $c + c_0 = 0$ el programa devuelve $r$, es decir, **quita**
  el término $t_0$. Por **(S)**, $\mathrm{Inv}_L(r)$ vale: el orden estricto
  se conserva porque quitar un elemento de una sucesión estrictamente
  decreciente la deja estrictamente decreciente, y los términos que quedan
  son los originales, sin ceros, con exponentes naturales y reducidos. El
  resultado **no contiene ningún cero**: el único término que habría tenido
  coeficiente $0$ (el de suma $0$) es precisamente el que se descarta en
  lugar de construirse, que es lo que exige la segunda condición. Las
  llamadas anteriores solo antepusieron términos de exponente mayor
  (rama $e < e_0$). **(b)** se cumple porque $\mathrm{exps}(R) \subset
  \mathrm{exps}(L)$.

- **Paso para $e < e_0$ en los casos B y C.** Se razona igual que en el paso
  del caso A: el resultado es $t_0 :: R'$, con $R' = I(r, c, e)$ que, por HI,
  cumple (a) y (b); como los exponentes de $R'$ están en
  $\mathrm{exps}(r) \cup \{e\}$ y todos son menores que $e_0$, el orden
  estricto se conserva y $t_0$ no se toca.

**Cierre en `insertar-termino`.** El polinomio resultado es
$\text{poli}(\text{var}(p), R)$ con $R = I(\text{terms}(p), c, e)$. Como
$\mathrm{Inv}(p) \equiv \mathrm{Inv}_L(\text{terms}(p))$ y, por el Lema 3(a),
$\mathrm{Inv}_L(R)$, se obtiene $\mathrm{Inv}$ sobre el resultado.

**Terminación.** Medida $\mu(L) = |L|$: la única llamada recursiva está en
la rama $e < e_0$ y es sobre $r$, con $|r| = |L| - 1$; las demás ramas
(lista vacía, $e > e_0$ y $e = e_0$) no hacen llamadas recursivas. Hay a
lo sumo $|L| + 1$ llamadas, así que la lista se recorre una sola vez.

**Conclusión:** Si $\mathrm{Inv}(p)$ vale antes de la llamada, vale sobre
`insertar-termino(p, c, e)` en los tres casos (A, B y C), y la función
termina. Por eso el invariante **se conserva**: no se necesita ordenar ni
limpiar al final, que es lo que pedía el enunciado. $\blacksquare$

---

## 3. Equivalencia de las dos representaciones

Esta sección es una explicación conceptual apoyada en la sección 2.2 de
EOPL; no es una demostración formal.

- **Qué ve el cliente de un polinomio.** El cliente solo dispone de la
  interfaz: los constructores (`poli`, `nombre-var`, `sin-terminos`,
  `mas-terminos`, `termino`, `coef-ent`, `coef-rac`, `expo-nat`), los
  observadores (predicados y extractores como `poli->terms`,
  `mas-terminos->resto`, `termino->coef`, ...) y las cuatro funciones
  `polinomio-cero`, `insertar-termino`, `coeficiente-de` y
  `eliminar-termino`. No puede asumir que un polinomio sea una lista ni un
  procedimiento: no puede aplicar `car`, `cdr` ni `list-ref`, ni llamar al
  dato como función, sin salirse del contrato. Todo lo que puede saber de
  un dato lo obtiene preguntándole a un observador.

- **Qué cambia entre las dos representaciones y por qué queda adentro.**
  En la representación con listas, cada variante es una lista cuyo primer
  elemento es una etiqueta (`'(termino coef expo)`), y los observadores son
  `car`/`cdr` sobre ella. En la representación con procedimientos, cada
  variante es un procedimiento que recibe un mensaje (`'tipo`,
  `'termino->coef`, ...) y responde; los observadores envían el mensaje
  correspondiente. Lo único que difiere es el cuerpo de los constructores y
  de los observadores. `insertar-termino`, `coeficiente-de`,
  `eliminar-termino` y `polinomio-cero` están escritas **solo** en términos
  de esos constructores y observadores, y por eso su texto es literalmente el
  mismo en ambos archivos (la comparación de los dos archivos solo difiere en
  comentarios). Cambiar de representación es cambiar de archivo en la
  sección de constructores y observadores; las funciones de la interfaz no
  se tocan. Esto es el argumento de EOPL 2.2: quien programa contra la
  interfaz es independiente de cómo se representan los datos, y el
  invariante lo mantienen las funciones, no la representación; por eso las
  pruebas de la Parte 4 se pueden escribir una vez (`suite-interfaz`) y
  aplicar sin cambios a las tres representaciones.

- **Qué habría que hacer para que el cliente notara la diferencia.** El
  cliente tendría que usar algo que **no** es parte de la interfaz:
  imprimir el dato (una lista se ve como lista; un procedimiento se ve como
  `#<procedure>`), compararlo con `equal?` (dos polinomios iguales con
  listas lo son, con procedimientos no), o aplicar `car` o llamar al dato
  como función. Si el cliente hiciera eso, su programa dependería de la
  representación: dejaría de funcionar al cambiar de archivo, y eso
  significaría que **la abstracción se rompió**, ya sea porque el cliente
  violó el contrato o porque la interfaz no ofrecía una operación que le
  hacía falta. Mientras el cliente se limite a la interfaz, las tres
  representaciones (incluida la de datatypes, que es la tercera cara del
  mismo TAD) son indistinguibles. La unicidad de la forma de cada
  polinomio, garantizada por el invariante, hace además que los resultados
  de las operaciones sean los mismos en las tres.

---

## 4. Referencias

- Friedman, D. P., & Wand, M. *Essentials of Programming Languages*,
  3.ª ed., MIT Press, 2008. Sección 2.1 (especificación de datos),
  sección 2.2 (representación basada en listas y basada en
  procedimientos), sección 2.4 (`define-datatype` y `cases`).
- The Racket Reference, *Numbers*: https://docs.racket-lang.org/reference/numbers.html
  (garantía de que los racionales exactos están reducidos y con denominador
  positivo).
- RackUnit, *Unit Testing*: https://docs.racket-lang.org/rackunit/.
