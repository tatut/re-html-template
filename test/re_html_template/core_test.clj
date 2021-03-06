(ns re-html-template.core-test
  (:require [re-html-template.core :refer [html-template define-html-template]]
            [clojure.test :refer [deftest is testing]]
            [clojure.core.match :refer [match]]))

(defmacro eval-test-fn [file app-data & transforms]
  `(do
     (define-html-template test-fn# [~'app]
       {:file ~file :selector "html"}
       ~@transforms)
     (test-fn# ~app-data)))

(def links [{:url "http://www.google.com" :label "Google"}
            {:url "http://webjure.org" :label "Webjure"}])

(deftest for-transform
  (let [hiccup (eval-test-fn
                "test.html" {:links links}
                [:ul :li] {:for {:items (:links app)
                                 :item link}}
                [:li :a] {:replace-children (:label link)
                          :set-attributes {:href (:url link)}})]
    (is (match hiccup
               [:html
                [:head [:script _ _]]
                [:body [_ [_ [:ul ([[:li [:a {:href "http://www.google.com"} "Google"]]
                                    [:li [:a {:href "http://webjure.org"} "Webjure"]]] :seq)]]]]] true))))

(deftest translate
  (let [hiccup (eval-test-fn
                "translate.html" :good
                :body {:translate (fn [key]
                                    (if (= key "daytype")
                                      (list 'name 'app)
                                      "World"))}

                :.daytype {:when (not= :awful app)})]
    (is (match hiccup
               [:html
                [:head]
                [:body " Hello World! " [:div.daytype " It is a good day! "]]] true)))

  (let [hiccup (eval-test-fn
                "translate.html" :awful
                :body {:translate (fn [key]
                                    (if (= key "daytype")
                                      (list 'name 'app)
                                      "World"))}

                :.daytype {:when (not= :awful app)})]
    (is (match hiccup
               [:html
                [:head]
                [:body " Hello World! " _]] true))))

(deftest wrap-hiccup
  (let [tpl (html-template []
                           {:file "test.html" :selector ".links"
                            :wrap-hiccup (first %)})]
    (is (= (tpl) :div.links))))

(deftest wrap-transform
  (let [tpl (html-template
             []
             {:file "test.html" :selector ".main-content"}
             :.main-content {:wrap [::wrapped %]}
             :.links {:replace [:span "FOO"]})]
    (is (= (tpl) [::wrapped [:div.main-content [:span "FOO"]]]))))
