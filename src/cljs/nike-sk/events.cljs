(ns nike-sk.events
  (:require [re-frame.core :as re-frame]
            [statechart.core :as statechart]
            [nike-sk.statechart :refer [statechart]]
            [nike-sk.api :as api]))

(defn initialize []
  (let [{:keys [fx configuration] :as x} (statechart/initialize {:db {}} statechart)]
    (assoc-in fx [:db :configuration] configuration)))
(defn process-event [ctx event]
  (let [configuration (get-in ctx [:db :configuration])
        {:keys [fx configuration] :as x} (statechart/process-event {:configuration configuration
                                                                    :fx            (select-keys ctx [:db])} statechart event)]
    (assoc-in fx [:db :configuration] configuration)))

(defn reg-event [event]
  (re-frame/reg-event-fx event
    ;[re-frame/debug]
    (fn [ctx event]
      (process-event ctx event))))

(re-frame/reg-event-fx :initialize-db
  ;[re-frame/debug]
  (fn [_ _]
    (initialize)))

(reg-event :goto-page)
(reg-event :set-tab)
(reg-event :toggle-layout)
(reg-event :toggle-prematch)
(reg-event :toggle-live)
(reg-event :toggle-results)
(reg-event :set-menu)
(reg-event :set-date)
(reg-event :clear-date)
(reg-event :clear-menu)

(re-frame/reg-event-db :reset-boxes
  ;[re-frame/debug]
  (fn [db [_ path boxes]]
    (assoc-in db path {:boxes   boxes
                       :box-ids (->> boxes
                                     vals
                                     (map :box)
                                     (remove nil?)
                                     (sort-by :order)
                                     (mapv :boxId))})))
(re-frame/reg-event-db :reset-menu
  (fn [db [_ path menu-data]]
    (assoc-in db path menu-data)))

(re-frame/reg-fx :fx/reset-boxes
                 (fn [requests]
                   (doseq [{:keys [path filter]} (distinct requests)]
                     (js/console.log "Load boxes" path filter)
                     (api/load-boxes filter (fn [success? data]
                                              (when success?
                                                (re-frame/dispatch [:reset-boxes path data])))))))

(re-frame/reg-fx :fx/reset-sport-menu
                 (fn [requests]
                   (doseq [{:keys [path filter]} (distinct requests)]
                     (js/console.log "Load menu" path filter)
                     (api/load-menu (fn [success? data]
                                      (when success?
                                        (re-frame/dispatch [:reset-menu path data])))))))