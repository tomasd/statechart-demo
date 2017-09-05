(ns nike-sk.api
  (:require
    [re-frame.core :as re-frame]
    [ajax.core :refer [GET POST]]
    [cljs-time.coerce :as date-coerce]
    [cljs-time.format :as date-format]))



(defn process-boxes [data]
  (let [db   {}
        path []
        db   (reduce
               (fn [db box]
                 (assoc-in db [(:boxId box) :box] box))
               db
               (:boxes data))
        db   (reduce
               (fn [db sport-event]
                 (reduce (fn [db box-id]
                           (assoc-in db [box-id :sport-events (:sportEventId sport-event) :sport-event] sport-event))
                         db
                         (:boxIds sport-event)))
               db
               (:sportEvents data))
        db   (reduce
               (fn [db market]
                 (reduce (fn [db box-id]
                           (assoc-in db [box-id :markets (:marketId market) :market] market))
                         db
                         (:boxIds market)))
               db
               (:markets data))
        db   (reduce
               (fn [db bet]
                 (reduce (fn [db box-id]
                           (assoc-in db [box-id :sport-events (:sportEventId bet) :bets (:betId bet) :bet] bet))
                         db
                         (into #{} (concat (:boxIds bet)
                                           (get-in data [:betBoxIds (:betId bet)] [])))))
               db
               (:bets data))
        ]
    db))

(defn load-boxes [filter handle]
  (GET "https://live.nike.sk/api/prematch/boxes/portal"
       {:params          (cond-> filter
                                 (:date filter) (update :date #(date-format/unparse (:date cljs-time.format/formatters) %))
                                 (not (:date filter)) (dissoc :date)
                                 (not (:menu filter)) (dissoc :menu))
        :response-format (ajax.core/json-response-format {:raw true})
        :handler         (fn [response]
                           (let [boxes (process-boxes response)]
                             (handle true boxes)))
        :error-handler   (fn [response]
                           (js/console.log "error" response)
                           (handle false response)
                           )}))

(defn load-menu [handle]
  (GET "https://live.nike.sk/api/prematch/menu"
       {:params          {}
        :response-format (ajax.core/json-response-format {:raw true})
        :handler         (fn [response]
                           (handle true response))
        :error-handler   (fn [response]
                           (js/console.log "error" response)
                           (handle false response))}))