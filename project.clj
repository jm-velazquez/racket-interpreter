(defproject racket-interpreter "0.1.0-SNAPSHOT"
  :description "Racket Interpreter para Clojure, TP Lenguajes Formales 2C2023 - FIUBA"
  :url "https://github.com/jm-velazquez/racket-interpreter"
  :license {:name ""
            :url ""}
  :dependencies [[org.clojure/clojure "1.8.0"]]
  :main ^:skip-aot racket-interpreter.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all
                       :jvm-opts ["-Dclojure.compiler.direct-linking=true"]}})
