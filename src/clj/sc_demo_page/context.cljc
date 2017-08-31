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
  ([statechart {:keys [configuration history]} fx]
   {:configuration  (->> (or configuration #{})
                         (map #(state/get-state statechart %))
                         (filter state/atomic?)
                         (into #{}))
    :internal-queue (queue)
    :fx             fx
    :history        (or history {})})
  ([statechart configuration fx event]
   (-> (make-ctx statechart configuration fx)
       (update :internal-queue conj event))))

(defn current-configuration [ctx]
  (get-in ctx [:configuration]))

(defn full-configuration [ctx]
  (->> (current-configuration ctx)
       (mapcat #(cons % (state/proper-ancestors % nil)))
       (distinct)))

(defn state-history [state ctx]
  (let [value (->> (get-in ctx [:history (state/state-id state)])
                   (map #(state/get-state state %)))]
    (if-not (seq value)
      [state]
      value)))

(defn save-history [ctx exit-states]
  (reduce (fn [ctx state]
            (cond-> ctx
              (state/shallow-history? state)
              (assoc-in [:history (state/state-id state)]
                        (->> (full-configuration ctx)
                             (filter #(state/child? % state))
                             (map :id)))

              (state/deep-history? state)
              (assoc-in [:history (state/state-id state)]
                        (->> (full-configuration ctx)
                             (filter #(state/descendant? % state))
                             (filter state/atomic?)
                             (map :id)))))
          ctx
          exit-states))