(ns sc-demo-page.context
  (:require [sc-demo-page.state :as state]))

(defn queue []
  #?(:clj  clojure.lang.PersistentQueue/EMPTY
     :cljs #queue []))

(defn current-event [ctx]
  (:current-event ctx))

(defn current-event-id [ctx]
  (first (current-event ctx)))

(defn pop-event [ctx]
  (let [event (-> ctx :internal-queue peek)]
    (-> ctx
        (assoc :current-event event)
        (update :internal-queue pop))))

(defn make-ctx
  ([statechart configuration fx]
   {:configuration  (->> configuration
                         (map #(state/get-state statechart %))
                         (filter state/atomic?)
                         (into #{}))
    :internal-queue (queue)
    :fx             fx})
  ([statechart configuration fx event]
   (-> (make-ctx statechart configuration fx)
       (update :internal-queue conj event))))
