(ns re-html-template.parse
  "Load HTML document and return it as hiccup"
  (:require [clojure.java.io :as io]
            [clojure.string :as str])
  (:import (org.jsoup Jsoup)
           (org.jsoup.nodes Element Comment DataNode DocumentType TextNode)))

(defn- children [node]
  (.childNodes node))

(defn parse [file]
  (Jsoup/parse (slurp (io/resource file))))

(defn- attributes->map [attributes]
  (into {}
        (map (juxt #(keyword (.getKey %)) #(.getValue %)))
        (.asList attributes)))

(defmulti node->hiccup type)

(defmethod node->hiccup TextNode [text]
  (.text text))

(defmethod node->hiccup DataNode [data]
  (.getWholeData data))

(defmethod node->hiccup Comment [_])
(defmethod node->hiccup DocumentType [_])

(defn- empty-node? [node]
  (or (nil? node)
      (and (string? node)
           (str/blank? node))))

(defmethod node->hiccup Element [element]
  (let [{:keys [id class] :as attrs} (attributes->map (.attributes element))
        hiccup [(keyword (str (.tagName element)
                    (when-not (str/blank? id)
                      (str "#" id))
                    (when-not (str/blank? class)
                      (str "." (str/join "." (str/split class #"\s+"))))))]
        attrs (dissoc attrs :id :class)
        hiccup (if (empty? attrs)
                 hiccup
                 (conj hiccup attrs))]
    (into hiccup
          (comp (map node->hiccup)
                (remove empty-node?))
          (children element))))
