(ns statechart.core
  (:require
    [statechart.state :as state]
    [statechart.context :as ctx]
    [statechart.transition :as transition]
    [statechart.runtime :refer [run enter-states]]
    [statechart.make :as make]))

(defn make [statechart-definition]
  (make/make-statechart statechart-definition))

(defn- make-result [ctx]
  {:fx            (:fx ctx)
   :configuration {:configuration (->> (ctx/full-configuration ctx)
                                       (sort-by state/entry-order)
                                       (map :id)
                                       (into []))
                   :history       (:history ctx)}})

(defn process-event [statechart event {:keys [configuration fx]}]
  (let [ctx (loop [ctx (ctx/make-ctx statechart configuration fx event)]
              (if (seq (:internal-queue ctx))
                (let [ctx (run ctx)]
                  (recur ctx))
                ctx))]
    (make-result ctx)))


(defn initialize [statechart fx]
  (let [states (->> (state/initialize-statechart statechart)
                    (mapcat #(cons % (state/proper-ancestors % nil)))
                    (distinct))
        ctx    (enter-states (ctx/make-ctx statechart #{} fx) states)]
    (make-result ctx)))