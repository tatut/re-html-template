(ns re-html-template.core
  "Macro that generates Reagent components from HTML files."
  (:require [clojure.data.xml :as xml]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.walk :as walk])
  (:import (org.jsoup Jsoup)
           (org.jsoup.nodes Element Comment DataNode DocumentType TextNode XmlDeclaration)))

(defn- children [node]
  (.childNodes node))

(defn- parse [file]
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
          (map node->hiccup)
          (children element))))

(def transformation-type #{:replace

                           ;; Remove element
                           :when :omit

                           ;; Loop
                           :for

                           ;; Manipulate children
                           :prepend-children :append-children :replace-children
                           :set-attributes

                           ;; Translate text
                           :translate
                           })

(defmulti transform (fn [transformation element]
                      (first transformation)))

(defn- normalize
  "Normalize element so that it is [:tag {...attrs...} ...children...].
  The input may or may not have attributes, normalize returns an empty
  map in that case."
  [elt]
  (let [tag (first elt)
        attrs (if (map? (second elt))
                (second elt))
        children (drop (if attrs 2 1) elt)]
    (into [tag (or attrs {})] children)))

(declare walk)

(defmethod transform :omit [_ _]
  nil)

(defmethod transform :when [[_ & forms] element]
  `(when (do ~@forms)
     ~element))

(defmethod transform :for [[_ {:keys [item items index key transforms]}] element]
  (let [idx-sym (or index (gensym "index"))
        item-sym (or item (symbol (str *ns* "item")))]
    `(doall
      (map-indexed
       (fn [~idx-sym ~item-sym]
         ^{:key ~(or key idx-sym)}
         ~(let [[tag attrs & children] (normalize element)]
            (into [tag attrs]
                  (map #(walk [] (map vec (partition 2 transforms)) %))
                  children)))
       ~items))))

(defmethod transform :replace [[_ & forms] _]
  `(do ~@forms))

(def handlebar-pattern #"\{\{[^}]+\}\}")

(defn- translate [translation-expander-fn text-to-translate]
  (let [string-parts (str/split text-to-translate handlebar-pattern)
        translated-parts (re-seq handlebar-pattern text-to-translate)]
    `(str ~@(interleave string-parts
                        (map translation-expander-fn translated-parts)))))

(defmethod transform :translate [[_ translation-expander-form] element]
  (let [translate (eval translation-expander-form)
        [tag attrs & children] (normalize element)]
    (assert (fn? translate)
            "Translate requires a compile time form that yields a function.")
    `[~tag ~@(when attrs [attrs])
      ~@(for [c children]
          (if (string? c)
            (translate c)
            c))]))

(defmethod transform :prepend-children [[_ & forms] element]
  (let [[tag attrs & children] (normalize element)]
    `[~tag ~@(when attrs [attrs]) ~@forms ~@children]))

(defmethod transform :append-children [[_ & forms] element]
  (let [[tag attrs & children] (normalize element)]
    `[~tag ~@(when attrs [attrs]) ~@children ~@forms]))

(defmethod transform :replace-children [[_ & forms] element]
  (let [tag (first element)
        attrs (when (map? (second element))
                (second element))]
    `[~tag ~@(when attrs [attrs])
      ~@forms]))

(defmethod transform :set-attributes [[_ & forms] element]
  (let [[tag attrs & children] (normalize element)]
    `[~tag (merge ~attrs
                  (do ~@forms)) ~@children]))

(defn- html-tag
  "Get the HTML tag name of the hiccup tag keyword"
  [tag]
  (second (re-find #"^([^.#]+)" (name tag))))

(defn- classes
  "Get classes of hiccup tag keyword"
  [tag]
  (into #{} (map second (re-seq #"\.([^.#]+)" (name tag)))))

(defn- id
  "Get ID of hiccup tag keyword"
  [tag]
  (second (re-find #"\#([^.]+)" (name tag))))

(defn- matching-tag?
  "Check if candidate tag should match the given rule tag.
  Tags match if the tags have the same id or the same HTML tag and the candidate has
  the same classes as the rule. The candidate may have extra classes."
  [rule-tag candidate-tag]
  (let [rule-id (id rule-tag)
        rule-html-tag (html-tag rule-tag)]
    (or (and rule-id (= rule-id (id candidate-tag)))
        (and (or (nil? rule-html-tag)
                 (= rule-html-tag (html-tag candidate-tag)))
             (every? (classes candidate-tag) (classes rule-tag))))))

(defn- match?
  "Check if the given rule matches the current path"
  [rule path]
  (or
   ;; Rule is a keyword and the last path element matches it
   (and (keyword? rule)
        (matching-tag? rule (last path)))

   ;; Rule is a vector and the tail of the path matches
   (and (vector? rule)
        (>= (count path) (count rule))
        (every? (fn [[r c]]
                  (matching-tag? r c))
                (map vector rule (subvec path (- (count path) (count rule))))))))

(def wrapping-transformation? #{:when :for})
(defn transformation-order [t]
  (case t
    :when 1
    :for 2
    32))

(def ordered-transformation-types (sort-by transformation-order transformation-type))

;; TODO: always do children transformations in rule
(defn- apply-transformations [transformations element]
  (let [type (some #(when (contains? transformations %) %) ordered-transformation-types)]
    (if-not type
      element
      (transform [type (transformations type)]
                 (apply-transformations (dissoc transformations type) element)))))

(defn- walk [path transformations element]
  (if (vector? element)
    (let [tag (first element)
          path (conj path tag)]
      (if-let [transformation (some (fn [[rule xf]]
                                      (when (match? rule path)
                                        xf))
                                    transformations)]
        ;; Transformation found at this path, run it
        (do
          (assert (and (map? transformation)
                       (every? transformation-type (keys transformation)))
                  (str "Transformation must be a map from transformation type to forms. Got: " (pr-str transformation)))
          (apply-transformations transformation element))

        ;; No transformation, recurse into children
        (let [tag (first element)
              attrs (when (map? (second element))
                      (second element))
              children (drop (if attrs 2 1) element)
              element-out [tag]
              element-out (if attrs
                            (conj element-out attrs)
                            element-out)]
          (into element-out
                (map (partial walk path transformations))
                children))))
    element))

(defmacro define-html-template
  "Define HTML template component with the given name and arguments.
  Expands to a function with the specified name and arguments that returns
  the HTML as hiccup.

  Options map supports to following keys:
  :file        a file (in the classpath) to load HTML from
  :selector    a CSS selector string to take as the root element of the component

  Selectors and transformations are an alternating list of selectors and transformation
  maps.
  Each selector is either a keyword (matching an element with a class and id, like: :div.main-content)
  or a vector of keywords (path of matching elements)
  A transformation is a map of supported transformation types to forms. The forms are spliced to the
  function body and may refer to the arguments.

  Supported transformation types are:
  :when               Only show element if form yields truthy value
  :omit               Unconditionally remove this element
  :replace            Replace the whole element with the form.
  :prepend-children   Prepend children to the beginning of the element
  :append-children    Append children to the end of the element
  :replace-children   Replace all children
  :set-attributes     Set attributes generated by for (must yield an attribute map)
  :for                Repeat this node for a each element of a given collection

  "
  [name args {:keys [file selector] :as options} & selectors-and-transformations]
  (assert (symbol? name) "Name must be a symbol naming the function")
  (assert (and (vector? args)
               (every? symbol? args))
          "Args must be a (possibly empty) vector of symbols naming the function arguments")
  (let [doc (parse file)
        element-node (if selector
                       (.selectFirst doc selector)
                       (.root doc))]
    (assert element-node "Can't find component element, check CSS selector.")
    `(defn ~name [~@args]
       ~(walk []
              (map vec (partition 2 selectors-and-transformations))
              (node->hiccup element-node)))))


#_(define-html-template base-template [body-content]
    {:file "test.html" :selector "html"}
    :body {:replace-children body-content})


#_(define-html-template foo [app]
    {:file "templates/napote.html" :selector "body"}
  :div.main-actors-info {:append-children
                           (if (:foo? app)
                             "THIS IS FOO"
                             "THIS IS NOT FOO")
                         :set-attributes {:title (:title app)}}
  :ul.header-navi {:for {:items ["hep" "jipujei" "foobar"]
                         :item page
                         :transforms [[:li :div] {:replace-children page}]}})
