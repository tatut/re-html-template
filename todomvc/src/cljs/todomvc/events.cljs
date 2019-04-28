(ns todomvc.events
  (:require [tuck.core :as t]))

(defrecord UpdateNewTodo [text])
(defrecord AddTodo [])

(defrecord ToggleCompleted [idx])
(defrecord Remove [idx])
(defrecord SetFilter [filter])

(defrecord ClearCompleted [])

;; Events to edit
(defrecord EditTodo [idx])
(defrecord UpdateTodo [text])
(defrecord SaveTodo [])
(defrecord CancelEdit [])

(extend-protocol t/Event
  UpdateNewTodo
  (process-event [{text :text} app]
    (assoc app :new-todo text))

  AddTodo
  (process-event [_ {new-todo :new-todo :as app}]
    (-> app
        (update :todos conj {:label new-todo :completed? false})
        (dissoc :new-todo)))

  ToggleCompleted
  (process-event [{idx :idx} app]
    (update-in app [:todos idx :completed?] not))

  Remove
  (process-event [{idx :idx} {todos :todos :as app}]
    (assoc app :todos
           (if (zero? idx)
             (subvec todos 1)
             (into (subvec todos 0 idx)
                   (subvec todos (inc idx))))))

  SetFilter
  (process-event [{filter :filter} app]
    (assoc app :filter filter))

  ClearCompleted
  (process-event [_ app]
    (update app :todos #(filterv (complement :completed?) %)))


  EditTodo
  (process-event [{idx :idx} app]
    (assoc app :edit {:idx idx
                      :text (get-in app [:todos idx :label])}))

  UpdateTodo
  (process-event [{text :text} app]
    (assoc-in app [:edit :text] text))

  SaveTodo
  (process-event [_ {edit :edit :as app}]
    (-> app
        (assoc-in [:todos (:idx edit) :label] (:text edit))
        (dissoc :edit)))

  CancelEdit
  (process-event [_ app]
    (dissoc app :edit)))
