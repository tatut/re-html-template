(ns todomvc.core
  (:require [reagent.core :as r]
            [todomvc.view :as view]
            [cljs.reader :as reader]
            [tuck.core :as tuck]))


(defonce app-state (r/atom nil))


(defn main* []
  [tuck/tuck app-state view/main-view])

(defn ^:export main []
  (.log js/console "TodoMVC frontend starting")
  (reset! app-state (reader/read-string
                     (.-innerText (.getElementById js/document "todomvc-initial-state"))))
  (js/ReactDOM.hydrate (r/as-element [main*]) (.querySelector js/document "section.todoapp")))
