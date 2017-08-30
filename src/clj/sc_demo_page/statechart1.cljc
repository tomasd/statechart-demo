(ns sc-demo-page.statechart1
  (:require [clojure.zip :as zip]))

(defprotocol State
  (initialize-state [this])
  (substates [this])
  (state-id [this]))

(extend-protocol State
  clojure.lang.PersistentArrayMap
  (initialize-state [this]
    (case (:type this)
      :and
      (mapcat initialize-state (vals (:states this)))

      :xor
      [(get-in this [:states (:init this)])]

      [this]))
  (substates [this]
    (vals (:states this)))
  (state-id [this]
    (:id this)))

(defn parent-id [state]
  (let [id (state-id state)]
    (cond
      (= id []) nil
      (= (count id) 1) []

      :else (->> id
                 (butlast)
                 vec))))

(defn parent [state]
  (let [{:keys [state-index]} state]
    (get state-index (parent-id state))))

(defn state-zip [state]
  (zip/zipper
    (constantly true)
    substates
    (fn [state] (prn "edit"))
    state
    ))

(defn make-transitions [state]
  (let [{:keys [id]} state]
    (update state :transitions
            (fn [transitions]
              (->> transitions
                   (map (fn [transition]
                          (let [{:keys [target]} transition]
                            (cond-> transition
                              (keyword? target)
                              (assoc :target (conj id target)))))))))))



(defn index-states [state]
  (loop [loc (state-zip state)
         idx {}]
    (if (zip/end? loc)
      idx
      (let [value (zip/node loc)]
        (recur (zip/next loc)
               (assoc idx (:id value) value))))))

(defprotocol IStatechartIndex
  (set-index [this idx]))
(deftype StatechartIndex [^:volatile-mutable idx]
  IStatechartIndex
  (set-index [this index]
    (set! idx index))
  clojure.lang.ILookup
  (valAt [o k]
    (get idx k))
  (valAt [o k not-found]
    (get idx k not-found)))

(defn make-statechart
  ([state]
   (let [state-index (StatechartIndex. {})
         state       (make-statechart [] (assoc state :id []
                                                      :state-index state-index)
                                      state-index)]
     (set-index state-index (index-states state))
     state))
  ([path state state-index]
   (-> (reduce-kv (fn [state id substate]
                    (assoc-in state [:states id] (make-statechart (conj path id) substate state-index)))
                  state
                  (:states state))
       (assoc :id path
              :state-index state-index)
       (make-transitions))))

(comment
  (def statechart
    {:type :and
     :states
           {:push {}
            :user {:type   :xor
                   :init   :anonymous
                   :states {:anonymous     {}
                            :authenticated {}}}
            :page {:type        :xor
                   :init        :home
                   :states      {:home    {}
                                 :betting {}}
                   :transitions [{:event  :goto-home
                                  :target :home}
                                 {:event  :goto-betting
                                  :target :betting}]
                   }}}))