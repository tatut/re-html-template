(ns todomvc.server
  "TodoMVC server"
  (:require [org.httpkit.server :as server]
            [todomvc.view :as view]
            [hiccup.core :refer [html]]
            [compojure.core :refer [defroutes GET]]
            [compojure.route :as route]))

(defonce todomvc nil)


(defn main-handler [req]
  {:status 200
   :headers {"Content-Type" "text/html"}
   :body (html (view/page {:filter "all"
                           :todos (into [{:label "teeppä tämä" :completed? true}
                                         {:label "tee myös cljs toteutus" :completed? false}]
                                        (for [i (range 10)]
                                          {:label (str "todo " i)
                                           :completed? (rand-nth [true false])}))}))})

(defroutes app
  (GET "/" req (main-handler req))
  (route/resources "/"))

(defn start []
  (alter-var-root #'todomvc
                  (fn [_]
                    (server/run-server app {:port 3000}))))

(defn stop []
  (todomvc)
  (alter-var-root #'todomvc (constantly nil)))

(defn restart []
  (stop)
  (start))

(defn -main [& args]
  (start)
  (println "TodoMVC running: http://localhost:3000/"))
