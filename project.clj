(defproject pintoid "0.1.0-SNAPSHOT"

  :description "Multiplayer asteroid-like game"

  :dependencies [[org.clojure/clojure "1.10.0-RC1"]
                 [org.clojure/algo.monads "0.1.6"]
                 [org.clojure/clojurescript "1.10.439"]
                 [org.clojure/core.async "0.4.474"]
                 [org.clojure/data.int-map "0.2.4"]
                 [org.clojure/data.xml "0.2.0-alpha5"]
                 [org.clojure/tools.nrepl "0.2.13"]
                 [cljsbuild/cljsbuild "1.1.7"]
                 [cljsjs/pixi "4.7.0-0"]
                 [cljsjs/howler "2.0.5-0"]
                 [com.cognitect/transit-clj "0.8.313"]
                 [com.cognitect/transit-cljs "0.8.256"]
                 [com.taoensso/timbre "4.10.0"]
                 [com.taoensso/tufte "2.0.1"]
                 [compojure/compojure "1.6.1"]
                 [hiccup/hiccup "2.0.0-alpha1"]
                 [http-kit/http-kit "2.3.0"]
                 [jarohen/chord "0.8.1"]
                 [mount/mount "0.1.14"]
                 [prismatic/dommy "1.1.0"]
                 [ring/ring-core "1.7.1"]
                 [cider/piggieback "0.3.10"]
                 [aero/aero "1.1.3"]
                 [weasel/weasel "0.7.0"]
                 ]

  :plugins [[lein-cljsbuild "1.1.7"]
            [lein-shell "0.5.0"]
            [lein-with-env-vars "0.2.0"]
            [lein-binplus "0.6.4"]]

  :hooks [leiningen.cljsbuild
          leiningen.with-env-vars/auto-inject]

  :main pintoid.main
  :jar-name "pintoid-onlyclj-%s.jar"
  :uberjar-name "pintoid-%s.jar"

  :bin {:name "../pintoid"
        :jvm-opts
        ["-server"
         "-Xms64m"
         "-Xmx1024m"
         "-XX:MaxGCPauseMillis=10"
         ]}

  :resource-paths ["resources" "target/resources"]
  :aliases {"jar" "uberjar"
            "run-prod" ["do" "uberjar," "shell" "java" "-jar" "target/pintoid-${:version}.jar"]}

  :cljsbuild
  {:builds
   {:main {:source-paths ["src/pintoid/client"]
           :compiler {:output-to "target/resources/js/pintoid.js"
                      :language-in :ecmascript5
                      :language-out :ecmascript5
                      :optimizations :whitespace
                      :main pintoid.client.core}}}}

  :profiles
  {:dev
   {:cljsbuild {:builds {:main {:compiler {:optimizations :whitespace
                                           :source-map "target/resources/js/pintoid.js.map"
                                           :output-dir "target/resources/js"
                                           :pretty-print true
                                           }}}}
    :repl-options {:nrepl-middleware [cider.piggieback/wrap-cljs-repl]}}

   :prod
   {:env-vars {:TIMBRE_LEVEL :info}
    :cljsbuild {:builds {:main {:jar true
                                :compiler {:optimizations :advanced
                                           :elide-asserts true}}}}}

   :repl [:dev {:dependencies [[criterium/criterium "0.4.4"]]}]

   :uberjar [:prod {:aot :all :auto-clean true}]
   }
  )
