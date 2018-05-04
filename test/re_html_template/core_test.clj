(ns re-html-template.core-test
  (:require [re-html-template.core :refer [define-html-template]]
            [clojure.test :refer [deftest is testing]]))

(defmacro eval-test-fn [app-data & transforms]
  `(do
     (define-html-template test-fn# [~'app]
       {:file "test.html" :selector "html"}
       ~@transforms)
     (test-fn# ~app-data)))

(def links [{:url "http://www.google.com" :label "Google"}
            {:url "http://webjure.org" :label "Webjure"}])

(deftest for-transform
  (let [hiccup (eval-test-fn
                {:links links}
                [:ul :li] {:for {:items (:links app)
                                 :item link}
                           :transforms
                           [:a {:replace-children (:label link)
                                :set-attributes {:href (:url link)}}]})]
    (println hiccup)
    (is hiccup)))
