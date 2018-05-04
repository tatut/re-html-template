(defproject re-html-template "20180504"
  :dependencies [[org.clojure/clojure "1.9.0"]

                 [org.jsoup/jsoup "1.11.3"]
                 [org.clojure/data.xml "0.0.8"]]
  :profiles {:dev {:dependencies [[org.clojure/core.match "0.3.0-alpha5"]]}}
  :source-paths ["src"]
  :test-paths ["test"])
