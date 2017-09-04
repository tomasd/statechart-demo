(ns sc-demo-page.events
  (:require [re-frame.core :as re-frame]
            [sc-demo-page.db :as db]
            [reagent.ratom :refer-macros [reaction]]
            [reagent.core :as reagent]
            [statechart.core :as statechart]
            [ajax.core :refer [GET POST]]
            [clojure.string :as str]
            [sc-demo-page.view-utils :as wu]
            [sc-demo-page.demo :as demo]))



(re-frame/reg-event-fx :initialize-db
  [re-frame/debug]
  (fn [_ _]
    (demo/initialize)))

(re-frame/reg-event-fx :goto-page
  [re-frame/debug]
  (fn [ctx event]
    (demo/process-event ctx event)))

(re-frame/reg-event-fx :set-tab
  [re-frame/debug]
  (fn [ctx event]
    (demo/process-event ctx event)))

(re-frame/reg-event-fx :set-menu
  [re-frame/debug]
  (fn [ctx event]
    (demo/process-event ctx event)))

(re-frame/reg-event-fx :toggle-prematch
  [re-frame/debug]
  (fn [ctx event]
    (demo/process-event ctx event)))

(re-frame/reg-event-fx :toggle-live
  [re-frame/debug]
  (fn [ctx event]
    (demo/process-event ctx event)))

(re-frame/reg-event-fx :toggle-results
  [re-frame/debug]
  (fn [ctx event]
    (demo/process-event ctx event)))





(re-frame/reg-fx :load-boxes
                 (fn [requests]
                   (doseq [{:keys [path filter]} requests]
                     #_(js/console.log "Loading boxes for" path "with filter" filter)
                     (GET "https://live.nike.sk/api/prematch/boxes/portal"
                          {:params          filter
                           :response-format (ajax.core/json-response-format {:raw true})
                           :handler         (fn [response]
                                              (re-frame/dispatch [:boxes-loaded path response]))
                           :error-handler   (fn [response]
                                              (js/console.log "error" response)
                                              )}))))

(re-frame/reg-fx :load-menu (fn [requests]
                              (doseq [{:keys [path filter]} requests]
                                (GET "https://live.nike.sk/api/prematch/menu" {:params          {}
                                                                               :response-format (ajax.core/json-response-format {:raw true})
                                                                               :handler         (fn [response]
                                                                                                  (re-frame/dispatch [:menu-loaded path response]))
                                                                               :error-handler   (fn [response]
                                                                                                  (js/console.log "error" response))}))))

(re-frame/reg-event-db :menu-loaded
  (fn [db [_ path data]]
    (assoc-in db path data)))

(re-frame/reg-event-db :boxes-loaded
  (fn [db [_ path data]]
    (let [db (-> db
                 (update-in path dissoc :boxes)
                 (update-in path dissoc :box-ids))
          db (reduce
               (fn [db box]
                 (assoc-in db (into path [:boxes (:boxId box) :box]) box))
               db
               (:boxes data))
          db (reduce
               (fn [db sport-event]
                 (reduce (fn [db box-id]
                           (assoc-in db (into path [:boxes box-id :sport-events (:sportEventId sport-event) :sport-event]) sport-event))
                         db
                         (:boxIds sport-event)))
               db
               (:sportEvents data))
          db (reduce
               (fn [db market]
                 (reduce (fn [db box-id]
                           (assoc-in db (into path [:boxes box-id :markets (:marketId market) :market]) market))
                         db
                         (:boxIds market)))
               db
               (:markets data))
          db (reduce
               (fn [db bet]
                 (reduce (fn [db box-id]
                           (assoc-in db (into path [:boxes box-id :sport-events (:sportEventId bet) :bets (:betId bet) :bet]) bet))
                         db
                         (into #{} (concat (:boxIds bet)
                                           (get-in data [:betBoxIds (:betId bet)] [])))))
               db
               (:bets data))
          db (assoc-in db (conj path :box-ids)
                       (->> (get-in db (conj path :boxes))
                            vals
                            (map :box)
                            (remove nil?)
                            (sort-by :order)
                            (mapv :boxId)))]
      db)))

(re-frame/reg-event-fx :login
  (fn [ctx event]
    (demo/process-event ctx event)))

(re-frame/reg-event-fx :logout
  (fn [ctx event]
    (demo/process-event ctx event)))