(ns nike-sk.core
  (:require [reagent.core :as reagent]
            [re-frame.core :as re-frame]
            [nike-sk.events]
            [nike-sk.subs]
            [nike-sk.views :as views]
            [nike-sk.config :as config]))


(defn dev-setup []
  (when config/debug?
    (enable-console-print!)
    (println "dev mode")))

(defn mount-root []
  (re-frame/clear-subscription-cache!)
  (reagent/render [views/application]
                  (.getElementById js/document "app")))

(defn ^:export init []
  (re-frame/dispatch-sync [:initialize-db])
  (dev-setup)
  (mount-root))
