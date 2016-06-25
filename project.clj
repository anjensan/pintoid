(defproject pintoid "0.1.0-SNAPSHOT"

  :description "Multiplayer asteroid-like game"

  :dependencies [[org.clojure/clojure "1.8.0"]
                 [org.clojure/core.async "0.2.385"]
                 [org.clojure/clojurescript "1.9.76"]
                 [org.clojure/data.int-map "0.2.2"]
                 [org.clojure/tools.logging "0.3.1"]
                 [ring/ring-core "1.5.0"]
                 [jarohen/chord "0.7.0"]
                 [compojure/compojure "1.5.1"]
                 [hiccup/hiccup "1.0.5"]
                 [prismatic/dommy "1.1.0"]
                 [cljsjs/pixi "3.0.10-0"]
                 [ch.qos.logback/logback-classic "1.1.7"]
                 ]

  :plugins [[lein-pdo "0.1.1"]
            [lein-cljsbuild "1.1.3"]
            [lein-shell "0.5.0"]
            [jarohen/lein-frodo "0.4.2"]]

  :profiles
  {:bench {:dependencies [[criterium/criterium "0.4.4"]]}}

  :hooks [leiningen.cljsbuild]

  ;; config
  :frodo/config-resource "conf/development.edn"
  ;; :frodo/config-resource "conf/production.edn"

  :resource-paths ["resources" "target/resources"]

  ;; commands
  :aliases {"dev" ["pdo" ["cljsbuild" "auto"] "frodo"]}

  ;; clojurescript
  :cljsbuild
  {:builds {:main {:source-paths ["src"]
                   :compiler {:output-to "target/resources/js/pintoid.js"
                              :optimizations :whitespace
                              :main pintoid.client.core}}}}
  )
