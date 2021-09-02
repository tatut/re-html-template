(ns re-html-template.parse
  "Load HTML document and return it as hiccup"
  (:require [clojure.java.io :as io]
            [clojure.string :as str])
  (:import (org.jsoup Jsoup)
           (org.jsoup.nodes Element Comment DataNode DocumentType TextNode)))

(defn- children [node]
  (.childNodes node))

(defn template-last-modified [template-file-name]
  (if-let [url (io/resource template-file-name)]
    (.lastModified (java.io.File. (.toURI url)))
    (.lastModified (io/file template-file-name))))

(defn parse [template-file-name]
  (let [classpath-resource (io/resource template-file-name)
        file (when-not classpath-resource
               (io/file template-file-name))]
    (if-not (or classpath-resource (.canRead file))
      (throw (ex-info "No such template. Can't open specified template as classpath resource or file."
                      {:template-file-name template-file-name}))
      (Jsoup/parse (slurp (or classpath-resource file))))))

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

;; Some CSS class names may have / in them and they won't be
;; representable as keyword.
(def ^:private illegal-kw-chars #{\/ \space})

(defn valid-in-keyword? [class]
  (every? (complement illegal-kw-chars) class))

(defmethod node->hiccup Element [element]
  (let [{:keys [id class] :as attrs} (attributes->map (.attributes element))
        classes (when-not (str/blank? class)
                  (remove str/blank? (str/split class #"\s+")))

        hiccup [(keyword
                 (str (.tagName element)
                      (when (and (not (str/blank? id))
                                 (valid-in-keyword? id))
                        (str "#" id))
                      (when (and (seq classes)
                                 (every? valid-in-keyword? classes))
                        (str "." (str/join "." classes)))))]
        attrs
        (merge (dissoc attrs :id :class)
               (when (and (not (str/blank? id))
                          (not (valid-in-keyword? id)))
                 {:id id})
               (when (and (seq classes)
                          (not (every? valid-in-keyword? classes)))
                 {:class (str/join " " classes)}))
        hiccup (if (empty? attrs)
                 hiccup
                 (conj hiccup attrs))]
    (into hiccup
          (comp (map node->hiccup)
                (remove empty-node?))
          (children element))))
