(ns re-html-template.spec
  "Spec for transformations"
  (:require [clojure.spec.alpha :as s]))

(s/def ::rule
  (s/or :tag-rule keyword?
        :attrs-rule (s/and map? #(every? keyword? (keys %)))
        :custom-rule :re-html-template.core/supported-custom-type))

(s/def ::rules
  (s/or :single-rule ::rule
        :rule-vector (s/and (s/coll-of ::rule) vector?)))

(s/def ::transforms-map
  (s/keys :opt-un [::replace ::when ::omit ::for ::wrap
                   ::prepend-children ::append-children ::replace-children
                   ::set-attributes ::remove-attributes

                   ::translate

                   ::transforms]))

(s/def ::transforms
  (s/*
   (s/cat :rules ::rules
          :transforms-map ::transforms-map)))

(s/def ::file string?)
(s/def ::selector string?)

(s/def ::options
  (s/keys :req-un []
          :opt-un [::file ::selector]))

(s/def ::args vector?)

(s/def ::html-template
  (s/cat :args ::args
         :options ::options
         :transforms ::transforms))

(s/def ::define-html-template
  (s/cat :name symbol?
         :args ::args
         :options ::options
         :transforms ::transforms))

(s/def ::html
  (s/cat :options ::options
         :transforms ::transforms))
