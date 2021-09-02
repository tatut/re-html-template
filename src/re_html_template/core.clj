(ns re-html-template.core
  "Macro that generates Reagent components from HTML files."
  (:require [clojure.string :as str]
            [re-html-template.parse :refer [parse node->hiccup] :as parse]
            [clojure.spec.alpha :as s]
            [re-html-template.spec :as spec]
            [clojure.walk :as walk]))

(defonce ^{:private true
           :doc "Global options for all HTML template processing."}
  global-options (atom {}))

(def transformation-type
  #{:replace

    ;; Remove element
    :when :omit

    ;; wrap element with arbitrary form
    :wrap

    ;; Loop
    :for

    ;; Manipulate children
    :prepend-children :append-children :replace-children
    :set-attributes :remove-attributes

    ;; Translate text
    :translate

    ;; Use tag attributes at compile time
    :let-attrs})

(def ^{:dynamic true :private true} *wrap-hiccup-form* nil)

(defn- hiccup
  "Wrap hiccup with form bound to *wrap-hiccup-form*.
  Otherwise returns form as is.

  Replaces % in wrapper with the form.
  For example if *wrap-hiccup-form* is (my.html/render %)
  and form is [:div.foo \"bar\"].
  Returns (my.html/render [:div.foo \"bar\"])"
  [form]
  (if-let [wrap *wrap-hiccup-form*]
    (for [w wrap]
      (if (= w '%)
        form
        w))
    form))

(defmulti transform (fn [transformation _element _orig-element _transform-options]
                      (first transformation)))

(defn- normalize
  "Normalize element so that it is [:tag {...attrs...} ...children...].
  The input may or may not have attributes, normalize returns an empty
  map in that case."
  [elt]
  (let [tag (first elt)
        attrs (when (map? (second elt))
                (second elt))
        children (drop (if attrs 2 1) elt)]
    (into [tag (or attrs {})] children)))

(declare walk)

(defmethod transform :omit [_ _ _ _]
  nil)

(defmethod transform :when [[_ & forms] element _ _]
  `(when (do ~@forms)
     ~(hiccup element)))

(defmethod transform :wrap [[_ form] element _orig-element _options]
  (walk/prewalk
   (fn [x]
     (if (= x '%)
       element
       x))
   form))

(defmethod transform :let-attrs [[_ bindings] element orig-element _options]
  (let [[_ attrs & _] (normalize orig-element)]
    `(let [~bindings ~attrs]
       ~(hiccup element))))

(defmethod transform :for [[_ {:keys [item items index key]}] element _ _options]
  (let [idx-sym (or index (gensym "index"))
        item-sym (or item (symbol (str *ns* "item")))]
    `(doall
      (map-indexed
       (fn [~idx-sym ~item-sym]
         (with-meta
           ;; Wrap in or to prevent NPE if hiccup wrapper returns nil
           (or ~(hiccup element) {})
           {:key ~(or key idx-sym)}))
       ~items))))

(defmethod transform :replace [[_ form] _ _ _options]
  form)

(def handlebar-pattern #"\{\{[^}]+\}\}")

(defn- translate
  ([translation-expander-fn text-to-translate]
   (translate translation-expander-fn text-to-translate []))
  ([translation-expander-fn text-to-translate acc]
   (let [idx (str/index-of text-to-translate "{{")]
     (if (nil? idx)
       ;; Nothing more to translate
       (if (seq acc)
         `(str ~@acc ~text-to-translate)
         ~text-to-translate)

       (let [end-idx (str/index-of text-to-translate "}}" (+ 2 idx))
             _ (assert end-idx (str "Closing translation braces not found, starting at " idx " in string: " text-to-translate))
             key (subs text-to-translate (+ idx 2) end-idx)
             acc (if (pos? idx)
                   (conj acc (subs text-to-translate 0 idx))
                   acc)
             rest-of-text (subs text-to-translate (+ end-idx 2))]
         (translate translation-expander-fn rest-of-text
                    (conj acc (translation-expander-fn key))))))))

(defmethod transform :translate [[_ translation-expander-form] element _ _options]
  (let [tr (eval translation-expander-form)]
    (assert (fn? tr)
            "Translate requires a compile time form that yields a function.")
    (walk/postwalk
     (fn [form]
       (if (string? form)
         (translate tr form)
         form))
     element)))

(defmethod transform :prepend-children [[_ & forms] element _ _options]
  (let [[tag attrs & children] (normalize element)]
    `[~tag ~@(when attrs [attrs]) ~@forms ~@children]))

(defmethod transform :append-children [[_ & forms] element _ _options]
  (let [[tag attrs & children] (normalize element)]
    `[~tag ~@(when attrs [attrs]) ~@children ~@forms]))

(defmethod transform :replace-children [[_ & forms] element _ _options]
  (let [tag (first element)
        attrs (when (map? (second element))
                (second element))]
    `[~tag ~@(when attrs [attrs])
      ~@forms]))

(defmethod transform :set-attributes [[_ & forms] element _ _options]
  (let [[tag attrs & children] (normalize element)
        attrs (if (and (map? attrs)
                       (= 1 (count forms))
                       (map? (first forms)))
                ;; Both are literal maps, merge them as is
                (merge attrs (first forms))

                ;; Some code that yields a map, output code for merge
                `(merge ~attrs
                        (do ~@forms)))]
    `[~tag ~attrs ~@children]))

(defmethod transform :remove-attributes [[_ attrs-to-remove] element _ _options]
  (let [[tag attrs & children] (normalize element)]
    (when-not (map? attrs)
      (ex-info ":remove-attributes can only work on static map of attributes"
               {:attrs attrs}))
    `[~tag ~(apply dissoc attrs attrs-to-remove) ~@children]))

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

(defmulti custom-match-element
  "Match by custom rule type, dispatches on rule type.
  Implementations must return truthy value if rule matches
  given element."
  (fn [rule _candidate-element] (type rule)))

(defmethod custom-match-element :default
  [rule candidate-element]
  (throw
   (ex-info "Unsupported matching rule, expected tag keyword, attr map or supported custom type."
            {:rule rule
             :candidate-element candidate-element
             :custom-match-element-methods (methods custom-match-element)})))

(s/def ::supported-custom-type
  (fn [val]
    (contains? (into #{} (keys (methods custom-match-element)))
                        (type val))))

(defn- matching-element?
  "Check if candidate element matches the given rule.

  Rule can either be a keyword like :div or :span.some-class that
  describes the element's tag and class/id or a map that describes
  the element's attributes like {:data-test \"foo\"}.

  Tags match if the tags have the same id or the same HTML tag and the candidate has
  the same classes as the rule. The candidate may have extra classes.

  Attributes match if candidate has the same values for all the attributes
  defined in the map. The candidate may have extra attributes."
  [rule candidate-element]
  (cond
    ;; Tag rule
    (keyword? rule)
    (let [rule-tag rule
          candidate-tag (first candidate-element)
          candidate-attrs (when (map? (second candidate-element))
                            (second candidate-element))
          rule-id (id rule-tag)
          rule-html-tag (html-tag rule-tag)

          candidate-id (or (id candidate-tag)
                           (:id candidate-attrs))
          candidate-classes (into (classes candidate-tag)
                                  (some-> candidate-attrs :class
                                          (str/split #"\s+")))]
      (and (or (nil? rule-id)
               (= rule-id candidate-id))
           (or (nil? rule-html-tag)
               (= rule-html-tag (html-tag candidate-tag)))
           (every? candidate-classes
                   (classes rule-tag))))

    ;; Attributes rules
    (map? rule)
    (let [candidate-attrs (when (map? (second candidate-element))
                            (second candidate-element))]
      (every? (fn [[rule-key rule-value]]
                (= (get candidate-attrs rule-key) rule-value))
              rule))

    :else
    (custom-match-element rule candidate-element)))

(defn- match?
  "Check if the given rule matches the current path"
  [rule path]
  (cond
    ;; Rule is a vector and the tail of the path matches
    (vector? rule)
    (and (>= (count path) (count rule))
         (every? (fn [[r c]]
                   (matching-element? r c))
                 (map vector rule (subvec path (- (count path) (count rule))))))

    ;; Rule is a single rule and the last path element matches it
    :else
    (matching-element? rule (last path))))

(def wrapping-transformation? #{:when :for :let-attrs :wrap})
(defn transformation-order [t]
  (case t
    (:replace :wrap) 0
    :let-attrs 1
    :translate 2
    :when 3
    :for 4
    :remove-attributes 5
    :set-attributes 6
    :replace-children 7
    32))

(def ordered-transformation-types (sort-by transformation-order transformation-type))

(defn- apply-transformations [options transformations element]
  (let [type (some #(when (contains? transformations %) %) ordered-transformation-types)]
    (if-not type
      element
      (transform [type (transformations type)]
                 (apply-transformations options (dissoc transformations type) element)
                 element
                 (get options type)))))

(defn- conformed-rule
  "Given a clojure.spec conformed rule, return [rule transforms-map]"
  [{:keys [rules transforms-map]}]
  [(case (first rules)
     :single-rule (second (second rules))
     :rule-vector (mapv second (second rules)))
   transforms-map])

(defn- transform-element [options path all-transformations transformations element]
  (let [[tag attrs & children] (normalize element)
        new-element [tag]
        new-element (if (empty? attrs)
                      new-element
                      (conj new-element attrs))]
    (apply-transformations
     options
      (merge (:default-transformations options) transformations)
     (into new-element
           (map #(walk options path all-transformations %))
           children))))

(defn- walk [options path transformations element]
  (if (vector? element)
    (let [path (conj path element)]
      (if-let [transformation (some (fn [[rule xf]]
                                      (when (match? rule path)
                                        xf))
                                    transformations)]
        ;; Transformation found at this path, run it
        (transform-element options path
                           (remove (comp #(= % transformation) second) transformations)
                           transformation element)

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
                (map (partial walk options path transformations))
                children))))
    element))

(defn set-global-options!
  "Set global options to use for HTML template, see [[html]]
  for documentation on supported option keys."
  [options]
  {:pre [(map? options)]}
  (reset! global-options options))

(defn merge-global-options!
  "Merge global options, useful for settings from REPL.
  See [[html]] for documentation on supported option keys.

  Example: Reload all templates in dev, evaluate:
  (merge-global-options! {:reload? true})"
  [options]
  (swap! global-options merge options))

(defn- macroexpand-throw [form]
  (try
    (macroexpand-1 form)
    (catch Throwable t
      (throw (ex-info "Exception trying to evaluate rules"
                      {:form form
                       :thrown t})))))

(defn- expand-transforms [transforms]
  (mapcat
   (fn [[type val]]
     (case type
       :rule-and-transforms [val]
       :seq (let [evaluated (macroexpand-throw val)
                  tfs (s/conform ::spec/transforms evaluated)]
              (if (= ::s/invalid tfs)
                (throw (ex-info "Evaluated form result didn't conform to transforms"
                                {:result evaluated
                                 :explain (s/explain-data ::spec/transforms evaluated)}))
                (expand-transforms tfs)))))
   transforms))

;; Contains reloads for
;; {file {:last-modified 1232321412
;;        :templates {reload-key
;;                   {;; current template fn
;;                    :template-fn (fn...)
;;                    :ns #namespace[...] ; ns to eval in
;;                    ;; form to evaluate to get new template fn
;;                    :form (fn [foo bar] (html {:file ..}))}}}
(defonce reloads (atom {}))

(defn- reload! [reloads]
  (into {}
        (map (fn [[file-name {:keys [last-modified templates] :as file}]]
               (let [tpl-last-mod (parse/template-last-modified file-name)]
                 (if (> tpl-last-mod last-modified)
                   (do
                     (println "\nReloading templates:" file-name)
                     [file-name
                      {:last-modified tpl-last-mod
                       :templates
                       (into {}
                             (map (fn [[key {:keys [form ns error?] :as entry}]]
                                    (try
                                      [key
                                       (merge entry
                                              {:template-fn (binding [*ns* ns]
                                                              (eval form))
                                               :error? false})]
                                      (catch Throwable t
                                        (when-not error?
                                          ;; Only show error? once in a row
                                          (println "Error evaluating template from file " file-name ", source: \n" key "\nexception: " t))
                                        [key (assoc entry
                                                    :error? true
                                                    :exception t)]))))
                             templates)}])
                   [file-name file]))))
        reloads))

(def ^:private
  reloader-thread
  (delay
    (.start
     (java.lang.Thread.
      (fn []
        (while true
          (try
            (Thread/sleep 1000)
            (swap! reloads reload!)
            (catch Throwable t
              (println "Error in template reload thread:" (.getMessage t))))))))))

(defn- wrap-reload [options environment form compiled-form]
  (if (and (:reload? options)
           ;; ::reloading? is set when html is called within reload
           ;; to prevent recursive reloading
           (not (::reloading? options)))
    (let [key (str form)
          file (:file options)
          args (keys environment)
          initial-fn-form `(fn [~@args]
                             ~compiled-form)
          fn-form `(fn [~@args]
                     ~(let [[html opts & forms] form]
                        (concat (list html (assoc opts ::reloading? true))
                                forms)))]
      (swap! reloads update file
             (fn [{t :templates}]
               {:last-modified (System/currentTimeMillis)
                :templates (assoc t key
                                  {:template-fn (eval initial-fn-form)
                                   :ns *ns*
                                   :form fn-form})}))
      @reloader-thread
      `((get-in @reloads [~file :templates ~key :template-fn])
        ~@args))
    compiled-form))

(defmacro html
  "Expands to code that yields the HTML as hiccup.

  Options map supports to following keys:
  :file        a file (in the classpath) to load HTML from
  :selector    a CSS selector string to take as the root element of the component
  :wrap-hiccup a form to wrap hiccup output at all levels including
               toplevel function body.
               Replaces % with the generated hiccup,
               for example: (my.html/render %)
  :default-transformations
               default transformations done on all elements matched by any rule
               (merged with transformations from the rule)
  :reload?     watch template :file for modifications and automatically
               reload re-evaluate the template (experimental)
               should only be used in development mode

  Options can also be given by calling set-global-options!.


  Selectors and transformations are an alternating list of selectors and transformation
  maps.
  Each selector is either a keyword (matching an element with a class and id, like: :div.main-content)
  or a vector of keywords (path of matching elements)
  A transformation is a map of supported transformation types to forms. The forms are spliced to the
  function body and may refer to the arguments.

  If a sequence is encountered as a selector, it is macroexpanded at compile time
  and must produce a list of alternating selectors and transformations.
  The generated list is spliced in as if it were written directly.
  This allows creating reusable and parameterized transformations.

  Supported transformation types are:
  :when               Only show element if form yields truthy value
  :omit               Unconditionally remove this element
  :replace            Replace the whole element with the form.
  :prepend-children   Prepend children to the beginning of the element
  :append-children    Append children to the end of the element
  :replace-children   Replace all children
  :set-attributes     Set attributes generated by for (must yield an attribute map)
  :for                Repeat this node for a each element of a given collection
  :let-attrs          Bind element attributes at compile time
  :wrap               wrap element with arbitrary code (use % for the body)
  "
  [& args]
  (let [{:keys [options transforms] :as conformed}
        (s/conform ::spec/html args)
        _ (when (= ::s/invalid conformed)
            (throw (ex-info (s/explain-str ::spec/html args)
                            (s/explain-data ::spec/html conformed))))
        transforms (expand-transforms transforms)
        ;; transforms pitaa olla lista {:rules [...] :transforms-map {...}}
        _ (def *tfs transforms)
        options (merge @global-options options)
        {:keys [file selector wrap-hiccup]} options
        doc (parse file)
        element-node (if selector
                       (.selectFirst doc selector)
                       (.root doc))]
    (assert element-node "Can't find component element, check CSS selector.")
    (wrap-reload
     options &env &form
     (binding [*wrap-hiccup-form* wrap-hiccup]
       (hiccup (walk options []
                     (map conformed-rule transforms)
                     (node->hiccup element-node)))))))

(defmacro html-template
  "Define an anonymous HTML template function with the given arguments.

  See html macro for option details
  "
  [& html-template-args]
  (let [{:keys [args options transforms] :as conformed}
        (s/conform ::spec/html-template html-template-args)
        _ (when (= ::s/invalid conformed)
            (throw (ex-info (s/explain-str ::spec/define-html-template args)
                            (s/explain-data ::spec/define-html-template conformed))))
        options (merge @global-options options)
        {:keys [file selector wrap-hiccup]} options
        doc (parse file)
        element-node (if selector
                       (.selectFirst doc selector)
                       (.root doc))
        fn-name (gensym "html-template")]
    (assert element-node "Can't find component element, check CSS selector.")
    (binding [*wrap-hiccup-form* wrap-hiccup]
      `(fn ~fn-name [~@args]
         (html ~@(rest html-template-args))))))

(defmacro define-html-template
  "Define HTML template component with the given name and arguments.
  See html-template for options.

  Same as html-template but the first argument is the symbol name."
  [& args]
  (let [{:keys [name] :as conformed} (s/conform ::spec/define-html-template args)]
    (when (= conformed :clojure.spec.alpha/invalid)
      (throw (ex-info (str "Invalid define-html-template call: "
                           (s/explain-str ::spec/define-html-template args))
                      (s/explain-data ::spec/define-html-template args))))
    `(def ~name (html-template ~@(rest args)))))
