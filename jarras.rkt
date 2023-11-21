#lang racket

;;;;;;;;;;;;;;;;;;;; DEFINIR LA BASE DE CONOCIMIENTO ;;;;;;;;;;;;;;;;;;;;

(define (jarra5 x) (car x))

(define (jarra8 x) (car (cdr x)))

(define bc '(
       (lambda (x) (if (< (jarra5 x) 5) (list 5 (jarra8 x)) x))
       (lambda (x) (if (> (jarra5 x) 0) (list 0 (jarra8 x)) x))
       (lambda (x) (if (>= (- 5 (jarra5 x)) (jarra8 x)) (list (+ (jarra5 x) (jarra8 x)) 0) x))
       (lambda (x) (if (< (- 5 (jarra5 x)) (jarra8 x)) (list 5 (- (jarra8 x) (- 5 (jarra5 x)))) x))
       (lambda (x) (if (< (jarra8 x) 8) (list (jarra5 x) 8) x))
       (lambda (x) (if (> (jarra8 x) 0) (list (jarra5 x) 0) x))
       (lambda (x) (if (>= (- 8 (jarra8 x)) (jarra5 x)) (list 0 (+ (jarra8 x) (jarra5 x))) x))
       (lambda (x) (if (< (- 8 (jarra8 x)) (jarra5 x)) (list (- (jarra5 x) (- 8 (jarra8 x))) 8) x))
    )
)

;;;;;;;;;;; DEFINIR EL ALGORITMO DE BUSQUEDA DE LA SOLUCION ;;;;;;;;;;;;;

(define inicial '())

(define final '())

(define (breadth-first bc)
    (display "Ingrese el estado inicial: ") (set! inicial (read))
    (display "Ingrese el estado   final: ") (set! final (read))
    (cond ((equal? inicial final) (display "El problema ya esta resuelto !!!") (newline) (breadth-first bc))
          (#t (buscar bc final (list (list inicial)) '()))))

(define (buscar bc fin grafobusq estexp)
     (cond ((null? grafobusq) (fracaso))
          ((pertenece fin (car grafobusq)) (exito grafobusq))
          (#t (buscar bc fin (append (cdr grafobusq) (expandir (car grafobusq) bc estexp))
		              (if (pertenece (car (car grafobusq)) estexp) estexp (cons (car (car grafobusq)) estexp))))))

(define (expandir linea basecon estexp)
    (if (or (null? basecon) (pertenece (car linea) estexp))
        '()
	      (if (not (equal? ((eval (car basecon)) (car linea)) (car linea)))
	          (cons (cons ((eval (car basecon)) (car linea)) linea) (expandir linea (cdr basecon) estexp))
            (expandir linea (cdr basecon) estexp))))

(define (pertenece x lista)
    (cond ((null? lista) #f)
          ((equal? x (car lista)) #t)
          (else (pertenece x (cdr lista)))))

(define (fracaso)
    (display "No existe solucion") (newline) #t)

(define (exito grafobusq)
    (display "Exito !!!") (newline)
    (display "Prof ....... ") (display (- (length (car grafobusq)) 1)) (newline)
    (display "Solucion ... ") (display (reverse (car grafobusq))) (newline) #t)

