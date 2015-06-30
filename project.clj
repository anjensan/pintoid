(defproject pintoid "0.1.0-SNAPSHOT"

  :description "Multiplayer asteroid-like game"

  :dependencies [[org.clojure/clojure "1.7.0-RC2"]
                 [org.clojure/core.async "0.1.346.0-17112a-alpha"]
                 [org.clojure/clojurescript "0.0-3308"]
                 [org.clojure/data.int-map "0.2.0"]
                 [org.clojure/tools.logging "0.3.1"]
                 [ring/ring-core "1.3.2"]
                 [jarohen/chord "0.6.0"]
                 [compojure/compojure "1.3.4"]
                 [hiccup/hiccup "1.0.5"]
                 [prismatic/dommy "1.1.0"]
                 [ch.qos.logback/logback-classic "1.1.3"]
                 [net.mikera/vectorz-clj "0.30.1"]
                 [net.mikera/core.matrix "0.36.1"]
                 [criterium "0.4.3"]
                 ]

  :plugins [[lein-pdo "0.1.1"]
            [lein-cljsbuild "1.0.6"]
            [lein-shell "0.4.0"]
            [jarohen/lein-frodo "0.4.2"]]

  ;; config
  :frodo/config-resource "conf/development.edn"
  ;; :frodo/config-resource "conf/production.edn"

  :jvm-opts ^:replace []
  
  ;; commands
  :aliases {"dev" ["do"
                   ["shell" "mkdir" "-p" "target/resources"]
                   ["pdo" ["cljsbuild" "auto"] "frodo"]]}

  :source-paths ["src"]
  :resource-paths ["resources" "target/resources"]

  ;; clojurescript
  :cljsbuild
  {
   :builds
   [{:source-paths ["src"]
     :compiler {:output-to "target/resources/js/pintoid.js"
                :optimizations :whitespace
                :pretty-print true
                :externs ["resources/js/pixi.js"]
                }}]}
  )
