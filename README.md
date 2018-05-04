# re-html-template

Re-html-template can create Reagent components (functions that return hiccup markup) from HTML
templates at compile time.

See the macro `define-html-template`.

## Goals

- Simple macro that outputs a function
- Both Clojure and ClojureScript support
- Zero runtime dependencies (outputs plain clojure code that doesn't need any libraries)

## Usage

If you have the following HMTL file in your classpath `template.html`:

```html
<!DOCTYPE html>
<html
  <head>
    <title>My template</title>
  </head>
  <body>
    <div id="myapp">
      Links:
      <ul class="links">
        <li><a href="http://example.com/">Example.com</a></li>
      </ul>
    </div>
  </body>
</html>
```

And you define it as such:

```clojure
(define-html-template links-page [links]
  {:file "template.html" :selector "div.myapp"}
  [:ul.links :li] {:for {:items links
                         :item link}
                         :transforms
                         [:a {:replace-children (:label link)
                              :set-attributes {:href (:url link)}}]})
```

You will get (roughly) the following function:

```clojure
(defn links-page [links]
  [:div.myapp
    "Links:"
    [:ul.links
      (doall
       (map-indexed
        (fn [index15434 link]
          [:li
           [:a (merge
                 {:href "http://example.com"}
                 (do {:href (:url link)}))
             (:label link)]]))
        (:links app))]])
```

You can call the function as normal Reagent UI like `[links-page some-links-list]`.
