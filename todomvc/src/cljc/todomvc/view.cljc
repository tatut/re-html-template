(ns todomvc.view
  (:require #?(:clj [re-html-template.core :refer [define-html-template]])
            #?(:clj [clojure.java.io :as io])
            #?(:cljs [todomvc.events :as events]))
  #?(:cljs (:require-macros [re-html-template.core :refer [define-html-template]])))

;; Extract the template body
(define-html-template main-view [e! app]
  {:file "todomvc.html" :selector "section.todoapp"}

  ;; Add handlers to add new todo
  [:input.new-todo]
  {:set-attributes (merge
                    {:value (or (:new-todo app) "")}
                    #?(:clj {}
                       :cljs {:on-change #(e! (events/->UpdateNewTodo (-> % .-target .-value)))
                              :on-key-down #(when (= 13 (.-keyCode %))
                                              (e! (events/->AddTodo)))}))}

  ;; Loop through todos
  [:ul.todo-list :li]
  {:for {:items (filter (case (:filter app)
                          "all" (constantly true)
                          "active" (complement :completed?)
                          "completed" :completed?)
                        (:todos app))
         :item todo
         :index idx}
   :set-attributes {:class (when (:completed? todo)
                             "completed")}}

  [:div.view :label] {:replace-children (:label todo)}
  [:div.view :input.toggle]
  {:set-attributes
   (merge {:checked (if (:completed? todo)
                      ;; Subtle render vs react difference in checkbox value
                      "checked" #?(:clj nil :cljs ""))}
          #?(:cljs {:on-change #(e! (events/->ToggleCompleted idx))}))}

  ;; Set footer item count
  [:footer :span.todo-count]
  {:replace-children
   (let [items-left (count (filter (complement :completed?)
                                   (:todos app)))]
     [:span
      [:strong items-left]
      (if (not= 1 items-left) " items left" " item left")])}


  ;; Handle filters
  [:footer :ul.filters :li :a]
  {:let-attrs {:keys [data-filter]}
   :set-attributes {:class (if (= data-filter (:filter app))
                             "selected"
                             "")
                    :on-click #?(:clj nil
                                 :cljs #(do (.preventDefault %)
                                            (e! (events/->SetFilter data-filter))))}})

#?(:clj
   ;; This renders the full HTML file on the backend
   (define-html-template page [app]
     {:file "todomvc.html" :selector "html"}

     ;; For simplicity, just include the CSS in the html header
     ;; Also add serialized app state
     [:html :head] {:append-children (list [:style (slurp (io/resource "css/todomvc.css"))]
                                           [:script#todomvc-initial-state (pr-str app)])}

     ;; Add frontend build js scripts
     [:html :body] {:append-children (list
                                      [:script {:src "js/out/goog/base.js"}]
                                      [:script {:src "js/todomvc.js"}])
                    :set-attributes {:onload "todomvc.core.main()"}}

     ;; Render the view
     [:section.todoapp] {:replace-children (main-view nil app)}))
