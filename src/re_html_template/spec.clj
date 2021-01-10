(ns re-html-template.spec
  "Spec for transformations"
  (:require [clojure.spec.alpha :as s]))

(s/def ::rule
  (s/or :kw keyword?
        :kw-path (s/and (s/coll-of keyword?) vector?)))

(s/def ::transforms-map
  (s/keys :opt-un [::replace ::when ::omit ::for
                   ::prepend-children ::append-children ::replace-children
                   ::set-attributes

                   ::translate

                   ::transforms]))

(s/def ::transforms
  (s/*
   (s/cat :rule ::rule
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
