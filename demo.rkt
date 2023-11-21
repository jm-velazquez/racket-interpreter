#lang racket

(newline)
(display "*****************************************************") (newline)
(display "*                    Racket 2023                    *") (newline)
(display "* Demo de definicion y uso de variables y funciones *") (newline)
(display "*****************************************************") (newline)

(newline)
(display "Definicion de variables") (newline)
(display "-----------------------") (newline)

(display "> (define u 'u)") (newline)
(define u 'u)

(display "> (define v 'v)") (newline)
(define v 'v)

(display "> (define w 'w)") (newline)
(define w 'w)

(newline)
(display "Las variables ahora estan en el ambiente.") (newline)
(display "Evaluandolas se obtienen sus valores:") (newline)
(display "> u") (newline)
(display u) (newline)
(display "> v") (newline)
(display v) (newline)
(display "> w") (newline)
(display w) (newline)

(newline)
(display "Una vez definida una variable, con set! se le puede") (newline)
(display "cambiar el valor:") (newline)

(display "> (define n 0)") (newline)
(define n 0)
(display "> n") (newline)
(display n) (newline)

(display "> (set! n 17)") (newline)
(set! n 17)
(display "> n") (newline)
(display n) (newline)

(newline)
(display "Definicion de funciones") (newline)
(display "-----------------------") (newline)

(display "> (define (sumar a b) (+ a b))") (newline)
(define (sumar a b) (+ a b))

(display "> (define (restar a b) (- a b))") (newline)
(define (restar a b) (- a b))

(newline)
(display "Las funciones ahora estan en el ambiente.") (newline)
(display "es posible aplicarlas a valores formando expresiones") (newline)
(display "que evaluadas generan resultados:") (newline)

(display "> (sumar 3 5)") (newline)
(display (sumar 3 5)) (newline)

(display "> (restar 12 5)") (newline)
(display (restar 12 5)) (newline)

(newline)
(display "Racket es un lenguaje de ambito lexico (lexically scoped):") (newline)

(display "> (define x 1)") (newline)
(define x 1)

(display "> (define (g y) (+ x y))") (newline)
(define (g y) (+ x y))

(display "> (define (f x) (g 2))") (newline)
(define (f x) (g 2))

(display "> (f 5)") (newline)
(display (f 5)) (newline)
(display "[En TLC-Lisp -dynamically scoped- daria 7 en lugar de 3]") (newline)

(newline)
(display "Aplicacion de funciones anonimas [lambdas]") (newline)
(display "------------------------------------------") (newline)

(newline)
(display "Lambda con cuerpo simple:") (newline)
(display "> ((lambda (y) (+ 1 y)) 15)") (newline)
(display ((lambda (y) (+ 1 y)) 15)) (newline)

(newline)
(display "Lambda con cuerpo multiple:") (newline)
(display "> ((lambda (y) (display \"hola!\") (newline) (+ 1 y)) 5)") (newline)
(display ((lambda (y) (display "hola!") (newline) (+ 1 y)) 5)) (newline)

(newline)
(display "Lambda con cuerpo multiple y efectos colaterales [side effects]:") (newline)
(display "> ((lambda (a b c) (set! u a) (set! v b) (set! w c)) 1 2 3)") (newline)
(display ((lambda (a b c) (set! u a) (set! v b) (set! w c)) 1 2 3)) (newline)
(newline)

(display "Los nuevos valores de las variables modificadas:") (newline)
(display "> u") (newline)
(display u) (newline)
(display "> v") (newline)
(display v) (newline)
(display "> w") (newline)
(display w) (newline)

(newline)
(display "Aplicacion parcial:") (newline)
(display "> (((lambda (x) (lambda (y) (- x y))) 8) 3)") (newline)
(display (((lambda (x) (lambda (y) (- x y))) 8) 3)) (newline)

(newline)
(display "El mismo ejemplo anterior, ahora definiendo una funcion:") (newline)
(display "> (define p (lambda (x) (lambda (y) (- x y))))") (newline)
(define p (lambda (x) (lambda (y) (- x y))))

(display "> (p 8)") (newline)
(display (p 8)) (newline)

(display "> ((p 8) 3)") (newline)
(display ((p 8) 3)) (newline)

(newline)
(display "Definicion de funciones recursivas [recorrido lineal]") (newline)
(display "-----------------------------------------------------") (newline)

(newline)
(display "Funcion recursiva con efecto colateral") (newline)
(display "[deja en la variable d la cantidad de pares]:") (newline)

(display "> (define (recorrer l)") (newline)
(display "    (recorrer2 l 0))") (newline)

(define (recorrer l)
  (recorrer2 l 0))
  
(display "> (define d 0)") (newline)
(define d 0)

(display "> (define (recorrer2 l i)") (newline)
(display "    (cond") (newline)
(display "      ((null? (cdr l)) (set! d (+ 1 d)) (list (car l) i))") (newline)
(display "      (#t (display (list (car l) i)) (set! d (+ i 1)) (newline) (recorrer2 (cdr l) d))))") (newline)

(define (recorrer2 l i)
  (cond
    ((null? (cdr l)) (set! d (+ 1 d)) (list (car l) i))
    (#t (display (list (car l) i)) (set! d (+ i 1)) (newline) (recorrer2 (cdr l) d))))

(display "> (recorrer '(a b c d e f))") (newline)
(display (recorrer '(a b c d e f))) (newline)

(display "> d") (newline)
(display d) (newline)

(newline)
(display "Definicion de funciones recursivas [recorrido \"a todo nivel\"]") (newline)
(display "-------------------------------------------------------------") (newline)

(newline)
(display "Existencia de un elemento escalar en una lista:") (newline)

(display "> (define (existe? a l)") (newline)
(display "    (cond") (newline)
(display "      ((null? l) #f)") (newline)
(display "      ((not (list? (car l))) (or (equal? a (car l)) (existe? a (cdr l))))") (newline)
(display "      (else (or (existe? a (car l)) (existe? a (cdr l))))))") (newline)

(define (existe? a l)
  (cond
    ((null? l) #f)
    ((not (list? (car l))) (or (equal? a (car l)) (existe? a (cdr l))))
    (else (or (existe? a (car l)) (existe? a (cdr l))))))

(display "> (existe? 'c '(a ((b) ((d c) a) e f)))") (newline)
(display (existe? 'c '(a ((b) ((d c) a) e f)))) (newline)

(display "> (existe? 'g '(a ((b) ((d c) a) e f)))") (newline)
(display (existe? 'g '(a ((b) ((d c) a) e f)))) (newline)

(newline)
(display "Eliminacion de un elemento de una lista:") (newline)

(display "> (define (eliminar dat li)") (newline)
(display "    (cond") (newline)
(display "      ((null? li) li)") (newline)
(display "      ((equal? dat (car li)) (eliminar dat (cdr li)))") (newline)
(display "      ((list? (car li)) (cons (eliminar dat (car li)) (eliminar dat (cdr li))))") (newline)
(display "      (else (cons (car li) (eliminar dat (cdr li))))))") (newline)

(define (eliminar dat li)
  (cond
    ((null? li) li)
    ((equal? dat (car li)) (eliminar dat (cdr li)))
    ((list? (car li)) (cons (eliminar dat (car li)) (eliminar dat (cdr li))))
    (else (cons (car li) (eliminar dat (cdr li))))))

(display "> (eliminar 'c '(a ((b) ((d c) a) c f)))") (newline)
(display (eliminar 'c '(a ((b) ((d c) a) c f)))) (newline)

(display "> (eliminar '(1 2 3) '(a ((b) (((1 2 3) c) a) c f)))") (newline)
(display (eliminar '(1 2 3) '(a ((b) (((1 2 3) c) a) c f)))) (newline)

(newline)
(display "Profundidad de una lista:") (newline)

(display "> (define (profundidad lista)") (newline)
(display "    (if (or (not (list? lista)) (null? lista)) 0") (newline)
(display "        (if (> (+ 1 (profundidad (car lista))) (profundidad (cdr lista)))") (newline)
(display "            (+ 1 (profundidad (car lista)))") (newline)
(display "            (profundidad (cdr lista)))))") (newline)

(define (profundidad lista)
  (if (or (not (list? lista)) (null? lista)) 0
      (if (> (+ 1 (profundidad (car lista))) (profundidad (cdr lista)))
          (+ 1 (profundidad (car lista)))
          (profundidad (cdr lista)))))

(display "> (profundidad '((2 3)(3 ((7))) 5))") (newline)
(display (profundidad '((2 3)(3 ((7))) 5))) (newline)
(display "[el valor esperado es 4]") (newline)

(newline)
(display "\"Planchado\" de una lista:") (newline)

(display "> (define (planchar li)") (newline)
(display "    (cond") (newline)
(display "      ((null? li) '())") (newline)
(display "      ((list? (car li)) (append (planchar (car li)) (planchar (cdr li))))") (newline)
(display "      (else (cons (car li) (planchar (cdr li))))))") (newline)

(define (planchar li)
  (cond
    ((null? li) '())
    ((list? (car li)) (append (planchar (car li)) (planchar (cdr li))))
    (else (cons (car li) (planchar (cdr li))))))

(display "> (planchar '((2 3)(3 ((7))) 5))") (newline)
(display (planchar '((2 3)(3 ((7))) 5))) (newline)

(newline)
(display "Definicion de funciones para \"ocultar\" la recursividad en la programacion funcional") (newline)
(display "-----------------------------------------------------------------------------------") (newline)

(newline)
(display "FILTRAR [selecciona de una lista los elementos que cumplan con una condicion dada]:") (newline)

(display "> (define (filtrar f l)") (newline)
(display "    (cond") (newline)
(display "      ((null? l) '())") (newline)
(display "      ((f (car l)) (cons (car l) (filtrar f (cdr l))))") (newline)
(display "      (else (filtrar f (cdr l)))))") (newline)

(define (filtrar f l)
  (cond
    ((null? l) '())
    ((f (car l)) (cons (car l) (filtrar f (cdr l))))
    (else (filtrar f (cdr l)))))

(display "> (filtrar (lambda (x) (> x 0)) '(5 0 2 -1 4 6 0 8))") (newline)
(display (filtrar (lambda (x) (> x 0)) '(5 0 2 -1 4 6 0 8))) (newline)

(newline)
(display "REDUCIR [reduce una lista aplicando de a pares una funcion dada]:") (newline)

(display "> (define (reducir f l)") (newline)
(display "    (if (null? (cdr l))") (newline)
(display "        (car l)") (newline)
(display "        (f (car l) (reducir f (cdr l)))))") (newline)

(define (reducir f l)
  (if (null? (cdr l))
      (car l)
      (f (car l) (reducir f (cdr l)))))

(display "> (reducir (lambda (x y) (if (> x 0) (cons x y) y)) '(5 0 2 -1 4 6 0 8 ()))") (newline)
(display (reducir (lambda (x y) (if (> x 0) (cons x y) y)) '(5 0 2 -1 4 6 0 8 ()))) (newline)

(newline)
(display "MAPEAR [aplica a cada elemento de una lista una funcion dada]:") (newline)

(display "> (define (mapear op l)") (newline)
(display "    (if (null? l)") (newline)
(display "        '()") (newline)
(display "        (cons (op (car l)) (mapear op (cdr l)))))") (newline)

(define (mapear op l)
  (if (null? l)
      '()
      (cons (op (car l)) (mapear op (cdr l)))))

(display "> (mapear (lambda (x) (if (equal? x 0) 'z x)) '(5 0 2 -1 4 6 0 8))") (newline)
(display (mapear (lambda (x) (if (equal? x 0) 'z x)) '(5 0 2 -1 4 6 0 8))) (newline)

(newline)
(display "TRANSPONER [transpone una lista de listas]:") (newline)

(display "> (define (transponer m)") (newline)
(display "    (if (null? (car m))") (newline)
(display "        '()") (newline)
(display "        (cons (mapear car m) (transponer (mapear cdr m)))))") (newline)

(define (transponer m)
  (if (null? (car m))
    '()
    (cons (mapear car m) (transponer (mapear cdr m)))))

(display "> (transponer '((a b c) (d e f) (g h i)))") (newline)
(display (transponer '((a b c) (d e f) (g h i)))) (newline)

(newline)
(display "IOTA [retorna una lista con los primeros n numeros naturales]:") (newline)

(display "> (define (iota n)") (newline)
(display "    (if (< n 1)") (newline)
(display "         '()") (newline)
(display "         (auxiota 1 n)))") (newline)

(define (iota n)
    (if (< n 1)
     '()
     (auxiota 1 n)))

(display "> (define (auxiota i n)") (newline)
(display "    (if (equal? i n)") (newline)
(display "        (list n)") (newline)
(display "        (cons i (auxiota (+ i 1) n))))") (newline)

(define (auxiota i n)
  (if (equal? i n)
    (list n)
    (cons i (auxiota (+ i 1) n))))

(display "> (iota 10)") (newline)
(display (iota 10)) (newline)

(newline)
(display "Funciones implementadas usando las funciones anteriores") (newline)
(display "-------------------------------------------------------") (newline)

(newline)
(display "Sumatoria de los primeros n numeros naturales:") (newline)

(display "> (define (sumatoria n) (reducir + (iota n)))") (newline)

(define (sumatoria n) (reducir + (iota n)))

(display "> (sumatoria 100)") (newline)
(display (sumatoria 100)) (newline)
(display "[El valor esperado es 5050]") (newline)

(newline)
(display "Eliminacion de los elementos repetidos en una lista simple:") (newline)

(display "> (define (eliminar-repetidos li)") (newline)
(display "    (reverse (reducir (lambda (x y) (if (existe? x y) y (cons x y))) (reverse (cons '() li)))))") (newline)

(define (eliminar-repetidos li)
  (reverse (reducir (lambda (x y) (if (existe? x y) y (cons x y))) (reverse (cons '() li)))))

(display "> (eliminar-repetidos '(a b c d e f g d c h b i j))") (newline)
(display (eliminar-repetidos '(a b c d e f g d c h b i j))) (newline)

(newline)
(display "Seleccion del enesimo elemento de una lista dada:") (newline)

(display "> (define (seleccionar n li)") (newline)
(display "    (if (or (< n 1) (> n (length li)))") (newline)
(display "        '()") (newline)
(display "        (car (car (filtrar (lambda (x) (equal? n (car (cdr x)))) (transponer (list li (iota (length li)))))))))") (newline)

(define (seleccionar n li)
  (if (or (< n 1) (> n (length li)))
      '()
      (car (car (filtrar (lambda (x) (equal? n (car (cdr x)))) (transponer (list li (iota (length li)))))))))

(display "> (seleccionar 5 '(a b c d e f g h i j))") (newline)
(display (seleccionar 5 '(a b c d e f g h i j))) (newline)

(newline)
(display "Aplicacion de todas las funciones de una lista a un elemento dado:") (newline)

(display "> (define (aplicar-todas lf x)") (newline)
(display "    (mapear (lambda (f) (f x)) lf))") (newline)

(define (aplicar-todas lf x)
  (mapear (lambda (f) (f x)) lf))

(display "> (aplicar-todas (list length cdr car) '((3 2 1)(9 8)(7 6)(5 4)))") (newline)
(display (aplicar-todas (list length cdr car) '((3 2 1)(9 8)(7 6)(5 4)))) (newline)

(newline)
(display "Entrada de datos y salida del interprete") (newline)
(display "----------------------------------------") (newline)

(newline)
(display "Carga de datos desde la terminal/consola:") (newline)
(display "> (define r 0)") (newline)
(display "> (define (cargar-r)") (newline)
(display "    (display \"->r: \")(set! r (read))(display \"r*2: \")(display (+ r r))(newline))") (newline)
(display "> (cargar-r)") (newline)

(define r 0)
(define (cargar-r)
  (display "->r: ")(set! r (read))(display "r*2: ")(display (+ r r))(newline))
(cargar-r)

(newline)
(display "Para ver el ambiente [no funciona en Racket v8.10]: (env)") (newline)
(display "Para salir del interprete: (exit)") (newline)
