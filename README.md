# re-html-template

Re-html-template can create Reagent components (functions that return hiccup markup) from HTML
templates at compile time.

See the macros `html-template` and `define-html-template`.

## Changes

### 2021-09-05
- Support :translate option to globally supply a translation form

### 2021-08-31
- Support (experimental) for automatic reloading (and re-eval of code) when template file changes (`:reload? true`)
- Support merging global options as REPL convenience with `merge-global-options!`

### 2021-08-15
- Support reusable transforms via macros

### 2021-08-14
- Filter out empty classes
- Support custom matcher types (see `custom-match-element`)
- Support :remove-attributes transform (eg. to remove data- attribute used for matching purpose from output)
- Support :default-transformations that are run for all elements matched by any rule
- Support illegal (in clj keyword) chars in classes (represented in :class attribute instead)

### 2021-01-12
- Add :wrap transform support
- Add `html` macro that does just expansion without generating a function

### 2021-01-11
- Remove project.clj (last clojars version is 20180504)

### 2021-01-10
- Support attr maps as rules
- Support global options that are merged with options given at call site
- New `html-template` that expands to an anonymous function and refactor
  `define-html-template` to use that
- Usage as deps.edn git dependency
- `:wrap-hiccup` option to support different other approaches than returning hiccup

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
                         :item link}}

  :a {:replace-children (:label link)
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
        links)]])
```

You can call the function as normal Reagent UI like `[links-page some-links-list]`.

You can also use it on the backend to create hiccup markup. The library has no runtime dependencies
but the code you use in transformations may still depend on the environment.

## Transformations

Transformations bring user code to selected sections of the generated code.
The rule part is either a single rule or vector of rules that matches the
current document structure.

A single rule can be a keyword that matches the tag (like `:div.foo`)
or map that matches the attributes `{:data-id "foo"}`.

If the rules is a vector, the tail of the path at the candidate element must
match the vector of rules. For example if the current element is
`html > div > ul > li` the rule `[:ul :li]` would match but `[:div :li]` would not.


Transformations are always tried in order. The first rule that matches the current element
is used and only one transformation will be done for a single element. Make sure your
rules are in priority order or suitably unambiguous.

The transformation is a map where the keys are supported transformations.
The supported transformations are  described here.

| Key | Description |
| --- | --- |
| `:for` | Repeat the given element for each element in `:items` binding it to symbol named by `:item` |
| `:when` | Remove element if value (user code) is falsy |
| `:omit` | Unconditionally remove this element |
| `:replace` | Replace this element with user code |
| `:prepend-children` | Add children before the other children |
| `:append-children` | Add children after the other children |
| `:replace-children` | Replace children with the value (user code) |
| `:set-attributes` | Merge attributes from user code |
| `:remove-attributes` | Remove static attributes. Useful for removing data- attribute used for matching from the output. |

### Looping with `:for`

Repeating an element is often necessary to make a list of rows in a table or links in a header.
The `:for` transformation repeats the element as many times as there are elements in the given
items. If no `:key` form is specified, the elements will have metadata key based on the index.

Keys supported by `:for` transformation:

| Key | Description |
| --- | --- |
| :items | Code that yields the sequence of items (usually getting something from parameters) |
| :item | Name (symbol) to bind the item to so that it may be used in child transforms |
| :key | Optional form to generate key for the element (recommended to be some unique id of item) |
| :index | Optional symbol to bind the current loop index to (autogenerated by default) |


### Translating static text

The `:translate` option takes code that is evaluated at compile time.
 The specified code must yield a function that takes in a translation key (string) and returns
 the expansion to output. This makes it easy to integrate any existing translation function to HTML
 templates.

The translation is run last and will recursively transform all generated code. It will replace all
strings with handlebar references with translation expansions.

Example:
The element `[:div "Hello, {{user}}!"]`  and translation function `(fn [key] (list 'translate key))`
will output `[:div [:<> "Hello, " (translate "user") "!"]]`.
