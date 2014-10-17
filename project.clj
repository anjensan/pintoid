(defproject pintoid "0.1.0-SNAPSHOT"

  :description "Multiplayer asteroid-like game"

  :dependencies [[org.clojure/clojure "1.6.0"]
                 [org.clojure/core.async "0.1.346.0-17112a-alpha"]
                 [org.clojure/clojurescript "0.0-2371"]
                 [ring/ring-core "1.3.1"]
                 [jarohen/chord "0.4.2"]
                 [compojure/compojure "1.2.0"]
                 [hiccup/hiccup "1.0.5"]
                 [prismatic/dommy "1.0.0"]]

  :plugins [[lein-pdo "0.1.1"]
            [lein-cljsbuild "1.0.3"]
            [lein-shell "0.4.0"]
            [jarohen/lein-frodo "0.4.1"]]

  ;; config
  :frodo/config-resource "conf/development.edn"

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
