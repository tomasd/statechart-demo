(ns sc-demo-page.routes
  (:require-macros [secretary.core :refer [defroute]])
  (:import goog.History)
  (:require [secretary.core :as secretary]
            [goog.events :as events]
            [goog.history.EventType :as EventType]
            [re-frame.core :as re-frame]))

(defn hook-browser-navigation! []
  (doto (History.)
    (events/listen
      EventType/NAVIGATE
      (fn [event]
        (secretary/dispatch! (.-token event))))
    (.setEnabled true)))

(defn app-routes []
  (secretary/set-config! :prefix "#")
  ;; --------------------
  ;; define routes here
  (defroute "/" []
            (re-frame/dispatch [:dispatch [:goto-page :page/home]]))

  (defroute "/tipovanie" []
            (re-frame/dispatch [:dispatch [:goto-page :page/tipovanie]]))

  (defroute "/superkurzy" []
            (re-frame/dispatch [:dispatch [:goto-page :page/superkurzy]]))

  (defroute "/moje" []
            (re-frame/dispatch [:dispatch [:goto-page :page/moje]]))


  ;; --------------------
  (hook-browser-navigation!))
