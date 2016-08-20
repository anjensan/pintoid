(defproject pintoid "0.1.0-SNAPSHOT"

  :description "Multiplayer asteroid-like game"

  :dependencies [[org.clojure/clojure "1.8.0"]
                 [org.clojure/clojurescript "1.9.225"]
                 [org.clojure/core.async "0.2.385"]
                 [org.clojure/data.int-map "0.2.3"]
                 [aero/aero "1.0.0"]
                 [cljsjs/pixi "3.0.10-0"]
                 [com.taoensso/timbre "4.7.3"]
                 [compojure/compojure "1.5.1"]
                 [hiccup/hiccup "1.0.5"]
                 [jarohen/chord "0.7.0"]
                 [mount/mount "0.1.10"]
                 [prismatic/dommy "1.1.0"]
                 [ring/ring-core "1.5.0"]
                 [org.clojure/tools.nrepl "0.2.12"]
                 [com.cemerick/piggieback "0.2.1"]
                 [cljsbuild/cljsbuild "1.1.3"]
                 [weasel/weasel "0.7.0"]]

  :plugins [[lein-cljsbuild "1.1.3"]
            [lein-shell "0.5.0"]
            [lein-bin "0.3.4"]]

  :main pintoid.main
  :prep-tasks ["clean" "compile" ["cljsbuild" "once"]]
  :release-tasks [["vcs" "assert-committed"]
                  ["change" "version" "leiningen.release/bump-version" "release"]
                  ["vcs" "commit"]
                  ["vcs" "tag"]
                  ["uberjar"]]
  :jar-name "pintoid-onlyclj-%s.jar"
  :uberjar-name "pintoid-%s.jar"
  :bin {:name "../pintoid" :bootclasspath true}
  :resource-paths ["resources" "target/resources"]
  :aliases {"jar" "uberjar"
            "run-prod" ["do" "uberjar," "shell" "java" "-jar" "target/pintoid-${:version}.jar"]}

  :cljsbuild
  {:builds
   {:main {:source-paths ["src"]
           :compiler {:output-to "target/resources/js/pintoid.js"
                      :language-in :ecmascript5
                      :language-out :ecmascript5
                      :main pintoid.client.core}}}}

  :profiles
  {
   :dev-cljs
   {:cljsbuild
    {:builds {:main {:compiler {:optimizations :whitespace
                                :source-map "target/resources/js/pintoid.js.map"
                                :output-dir "target/resources/js"
                                :pretty-print true
                                }}}}
    :repl-options {:nrepl-middleware [cemerick.piggieback/wrap-cljs-repl]}}

   :prod-cljs
   {:cljsbuild
    {:builds {:main {:jar true
                     :compiler {:optimizations :advanced
                                :elide-asserts true}}}}}

   :dev [:dev-cljs {:dependencies [[criterium/criterium "0.4.4"]]}]
   :prod [:prod-cljs]
   :uberjar [:prod {:aot :all :auto-clean true}]
   }
  )
