(ns racket-interpreter.core-test
  (:require [clojure.test :refer :all]
            [racket-interpreter.core :refer :all]))

(deftest error?-test
  (testing "Error? test"
    (is (error? (list (symbol ";ERROR:") 'mal 'hecho)))
    (is (not (error? (list 'mal 'hecho))))
    (is (error? (list (symbol ";WARNING:") 'mal 'hecho)))
  ))

(deftest verificar-parentesis-test
  (testing "Verificar parentesis test"
    (is (= 1 (verificar-parentesis "(hola 'mundo")))
    (is (= -1 (verificar-parentesis "(hola '(mundo)))")))
    (is (= -1 (verificar-parentesis "(hola '(mundo) () 6) 7)")))
    (is (= -1 (verificar-parentesis "(hola '(mundo) () 6) 7) 9)")))
    (is (= 0 (verificar-parentesis "(hola '(mundo) )")))))

(deftest actualizar-amb-test
  (testing "Actualizar ambiente test"
    (is (= '(a 1 b 2 c 3 d 4) (actualizar-amb '(a 1 b 2 c 3) 'd 4)))
    (is (= '(a 1 b 4 c 3) (actualizar-amb '(a 1 b 2 c 3) 'b 4)))
    (is (= '(a b b 4 c 3) (actualizar-amb '(a b b 2 c 3) 'b 4)))
    (is (= '(a d b 2 c 3 d 4) (actualizar-amb '(a d b 2 c 3) 'd 4)))
    (is (= '(a 1 b 2 c 3) (actualizar-amb '(a 1 b 2 c 3) 'b (list (symbol ";ERROR:") 'mal 'hecho))))
    (is (= '(b 7) (actualizar-amb () 'b 7)))))

(deftest buscar-test
  (testing "Buscar test"
    (is (= 3 (buscar 'c '(a 1 b 2 c 3 d 4 e 5))))
    (is (= 'a (buscar 'a '(a a b 2 c 3 d 4 e 5))))
    (is (error? (buscar 'f '(a 1 b 2 c 3 d 4 e 5))))
    (is (error? (buscar 'f '(a f b 2 c 3 d 4 e 5))))))

(deftest proteger-bool-en-str-test
  (testing "Proteger bool en str test"
    (is (= "(or %f %t)" (proteger-bool-en-str "(or #f #t)")))
    (is (= "(and (or %f %t) %t)" (proteger-bool-en-str "(and (or #f #t) #t)")))
    (is (= "" (proteger-bool-en-str "")))))

(deftest restaurar-bool-test
  (testing "Restaurar bool test"
    (is (= "(and (or #F #f #t #T) #T)" (str (restaurar-bool (read-string (proteger-bool-en-str "(and (or #F #f #t #T) #T)"))))))
    (is (= "(and (or #F #f #t #T) #T)" (str (restaurar-bool (read-string "(and (or %F %f %t %T) %T)")))))))

(deftest fnc-append-test
  (testing "Append test"
    (is (= '(1 2 3 4 5 6 7) (fnc-append '( (1 2) (3) (4 5) (6 7)))))
    (is (error? (fnc-append '( (1 2) 3 (4 5) (6 7)))))
    (is (error? (fnc-append '( (1 2) A (4 5) (6 7)))))))

(deftest fnc-equal?-test
  (testing "Equal? test"
    (is (= (symbol "#t") (fnc-equal? '())))
    (is (= (symbol "#t") (fnc-equal? '(A))))
    (is (= (symbol "#f") (fnc-equal? '(A a))))
    (is (= (symbol "#f") (fnc-equal? '(A a A))))
    (is (= (symbol "#f") (fnc-equal? '(A a A a))))
    (is (= (symbol "#f") (fnc-equal? '(A a A B))))
    (is (= (symbol "#t") (fnc-equal? '(1 1 1 1))))
    (is (= (symbol "#f") (fnc-equal? '(1 1 2 1))))))

(deftest fnc-sumar-test
  (testing "Sumar test"
    (is (= 0 (fnc-sumar '())))
    (is (= 3 (fnc-sumar '(3))))
    (is (= 7 (fnc-sumar '(3 4))))
    (is (= 12 (fnc-sumar '(3 4 5))))
    (is (= 18 (fnc-sumar '(3 4 5 6))))
    (is (error? (fnc-sumar '(A 4 5 6))))
    (is (error? (fnc-sumar '(3 A 5 6))))
    (is (error? (fnc-sumar '(3 4 A 6))))))

(deftest fnc-restar-test
  (testing "Restar test"
    (is (error? (fnc-restar '())))
    (is (= '-3 (fnc-restar '(3))))
    (is (= '-1 (fnc-restar '(3 4))))
    (is (= '-6 (fnc-restar '(3 4 5))))
    (is (= '-12 (fnc-restar '(3 4 5 6))))
    (is (error? (fnc-restar '(A 4 5 6))))
    (is (error? (fnc-restar '(3 A 5 6))))
    (is (error? (fnc-restar '(3 4 A 6))))))

(deftest fnc-menor-test
  (testing "Menor test"
    (is (= (symbol "#t") (fnc-menor '())))
    (is (= (symbol "#t") (fnc-menor '(1))))
    (is (= (symbol "#t") (fnc-menor '(1 2))))
    (is (= (symbol "#t") (fnc-menor '(1 2 3))))
    (is (= (symbol "#t") (fnc-menor '(1 2 3 4))))
    (is (= (symbol "#f") (fnc-menor '(1 2 2 4))))
    (is (= (symbol "#f") (fnc-menor '(1 2 1 4))))
    (is (error? (fnc-menor '(A 1 2 4))))
    (is (error? (fnc-menor '(1 A 1 4))))
    (is (error? (fnc-menor '(1 2 A 4))))))

(deftest fnc-mayor-test
  (testing "Mayor test"
    (is (= (symbol "#t") (fnc-mayor '())))
    (is (= (symbol "#t") (fnc-mayor '(1))))
    (is (= (symbol "#t") (fnc-mayor '(2 1))))
    (is (= (symbol "#t") (fnc-mayor '(3 2 1))))
    (is (= (symbol "#t") (fnc-mayor '(4 3 2 1))))
    (is (= (symbol "#f") (fnc-mayor '(4 2 2 1))))
    (is (= (symbol "#f") (fnc-mayor '(4 2 1 4))))
    (is (error? (fnc-mayor '(A 3 2 1))))
    (is (error? (fnc-mayor '(3 A 2 1))))
    (is (error? (fnc-mayor '(3 2 A 1))))))

(deftest fnc-mayor-o-igual-test
  (testing "Mayor o igual test"
    (is (= (symbol "#t") (fnc-mayor-o-igual '())))
    (is (= (symbol "#t") (fnc-mayor-o-igual '(1))))
    (is (= (symbol "#t") (fnc-mayor-o-igual '(2 1))))
    (is (= (symbol "#t") (fnc-mayor-o-igual '(3 2 1))))
    (is (= (symbol "#t") (fnc-mayor-o-igual '(4 3 2 1))))
    (is (= (symbol "#t") (fnc-mayor-o-igual '(4 2 2 1))))
    (is (= (symbol "#f") (fnc-mayor-o-igual '(4 2 1 4))))
    (is (error? (fnc-mayor-o-igual '(A 3 2 1))))
    (is (error? (fnc-mayor-o-igual '(3 A 2 1))))
    (is (error? (fnc-mayor-o-igual '(3 2 A 1))))))

(deftest evaluar-escalar-test
  (testing "Evaluar escalar test"
    (is (= "(32 (x 6 y 11 z \"hola\"))" (str (evaluar-escalar 32 '(x 6 y 11 z "hola")))))
    (is (= "(\"chau\" (x 6 y 11 z \"hola\"))" (str (evaluar-escalar "chau" '(x 6 y 11 z "hola")))))
    (is (= "(11 (x 6 y 11 z \"hola\"))" (str (evaluar-escalar 'y '(x 6 y 11 z "hola")))))
    (is (= "(\"hola\" (x 6 y 11 z \"hola\"))" (str (evaluar-escalar 'z '(x 6 y 11 z "hola")))))
    (is (error? (first (evaluar-escalar 'a '(x a y 11 z "hola")))))
    (is (error? (first (evaluar-escalar 'n '(x 6 y 11 z "hola")))))))

(deftest evaluar-define-test
  (testing "Evaluar define test"
    (is (= "(#<void> (x 2))" (str (evaluar-define '(define x 2) '(x 1)))))
    (is (= "(#<void> (x 1 f (lambda (x) (+ x 1))))" (str (evaluar-define '(define (f x) (+ x 1)) '(x 1)))))
    (is (= "(#<void> (x 1 f (lambda (x) (+ x 1) (- x 1))))" (str (evaluar-define '(define (f x) (+ x 1) (- x 1)) '(x 1)))))
    (is (error? (first (evaluar-define '(define) '(x 1)))))
    (is (error? (first (evaluar-define '(define x) '(x 1)))))
    (is (error? (first (evaluar-define '(define x 2 3) '(x 1)))))
    (is (error? (first (evaluar-define '(define ()) '(x 1)))))
    (is (error? (first (evaluar-define '(define () 2) '(x 1)))))
    (is (error? (first (evaluar-define '(define 2 x) '(x 1)))))))

(deftest evaluar-if-test
  (testing "Evaluar if test"
    (is (= "(2 (n 7))" (str (evaluar-if '(if 1 2) '(n 7)))))
    (is (= "(7 (n 7))" (str (evaluar-if '(if 1 n) '(n 7)))))
    (is (= "(7 (n 7))" (str (evaluar-if '(if 1 n 8) '(n 7)))))
    (is (= "(#<void> (n 7 #f #f))" (str (evaluar-if (list 'if (symbol "#f") 'n) (list 'n 7 (symbol "#f") (symbol "#f"))))))
    (is (= "(8 (n 7 #f #f))" (str (evaluar-if (list 'if (symbol "#f") 'n 8) (list 'n 7 (symbol "#f") (symbol "#f"))))))
    (is (= "(#<void> (n 9 #f #f))" (str (evaluar-if (list 'if (symbol "#f") 'n '(set! n 9)) (list 'n 7 (symbol "#f") (symbol "#f"))))))
    (is (error? (first (evaluar-if '(if) '(n 7)))))
    (is (error? (first (evaluar-if '(if 1) '(n 7)))))))

(deftest evaluar-or-test
  (testing "Evaluar or test"
    (is (= "(#f (#f #f #t #t))" (str (evaluar-or (list 'or) (list (symbol "#f") (symbol "#f") (symbol "#t") (symbol "#t"))))))
    (is (= "(#t (#f #f #t #t))" (str (evaluar-or (list 'or (symbol "#t")) (list (symbol "#f") (symbol "#f") (symbol "#t") (symbol "#t"))))))
    (is (= "(7 (#f #f #t #t))" (str (evaluar-or (list 'or 7) (list (symbol "#f") (symbol "#f") (symbol "#t") (symbol "#t"))))))
    (is (= "(5 (#f #f #t #t))" (str (evaluar-or (list 'or (symbol "#f") 5) (list (symbol "#f") (symbol "#f") (symbol "#t") (symbol "#t"))))))
    (is (= "(#f (#f #f #t #t))" (str (evaluar-or (list 'or (symbol "#f")) (list (symbol "#f") (symbol "#f") (symbol "#t") (symbol "#t"))))))))

(deftest evaluar-set!-test
  (testing "Evaluar set! test"
    (is (= "(#<void> (x 1))" (str (evaluar-set! '(set! x 1) '(x 0)))))
    (is (error? (first (evaluar-set! '(set! x 1) '(y x)))))
    (is (error? (first (evaluar-set! '(set! x 1) '()))))
    (is (error? (first (evaluar-set! '(set! x) '(x 0)))))
    (is (error? (first (evaluar-set! '(set! x 1 2) '(x 0)))))
    (is (error? (first (evaluar-set! '(set! 1 2) '(x 0)))))
  )
)
