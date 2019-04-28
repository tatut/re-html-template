(defproject re-html-template-todomvc "0.1-SNAPSHOT"
  :dependencies [[org.clojure/clojure "1.10.0"]
                 [org.clojure/clojurescript "1.10.520"]
                 [http-kit "2.3.0"]
                 [reagent "0.8.1"]
                 [hiccup "1.0.5"]
                 [compojure "1.6.1"]

                 [webjure/tuck "20181204"]

                 ;; these are only needed because we aren't depending on the lib but
                 ;; actually adding its source paths
                 [org.jsoup/jsoup "1.11.3"]
                 [org.clojure/data.xml "0.0.8"]]
  :plugins [[lein-cljsbuild "1.1.7"]
            [lein-figwheel "0.5.13"]]

  :source-paths ["src/clj" "src/cljc" "../src" "src/html"]
  :cljsbuild {:builds
              [{:id "dev"
                :source-paths ["src/cljs" "src/cljc" "../src" "src/html"]
                :figwheel {:on-jsload "todomvc.core/reload-hook"}
                :compiler {:optimizations :none
                           :output-to "dev-resources/public/js/todomvc.js"
                           :output-dir "dev-resources/public/js/out"}}]}
  :main todomvc.server
  )
