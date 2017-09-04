(ns nike-sk.events
  (:require [re-frame.core :as re-frame]
            [statechart.core :as statechart]
            [nike-sk.statechart :refer [statechart]]))

(defn initialize []
  (let [{:keys [fx configuration] :as x} (statechart/initialize {:db {}} statechart)]
    (assoc-in fx [:db :configuration] configuration)))
(defn process-event [ctx event]
  (let [configuration (get-in ctx [:db :configuration])
        {:keys [fx configuration] :as x} (statechart/process-event {:configuration configuration
                                                                    :fx            (select-keys ctx [:db])} statechart event)]
    (assoc-in fx [:db :configuration] configuration)))


(re-frame/reg-event-fx :initialize-db
                       [re-frame/debug]
                       (fn [_ _]
                         (initialize)))

(re-frame/reg-event-fx :goto-page
  [re-frame/debug]
  (fn [ctx event]
    (process-event ctx event)))