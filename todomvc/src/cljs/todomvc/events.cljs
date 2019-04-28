(ns todomvc.events
  (:require [tuck.core :as t]))

(defrecord UpdateNewTodo [text])
(defrecord AddTodo [])

(defrecord ToggleCompleted [idx])
(defrecord SetFilter [filter])

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

  SetFilter
  (process-event [{filter :filter} app]
    (assoc app :filter filter)))
