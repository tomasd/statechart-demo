(ns statechart-test
  (:require [clojure.test :refer :all]
            [statechart.core :as statechart]
            [statechart.state :as state]
            [statechart.runtime :as runtime]
            [statechart.context :as ctx]
            [statechart.transition :as transition]
            [clojure.set :as set]))

(deftest x1
  (let [sc (statechart/make
             {:type        :and
              :states      {:field {:type   :xor
                                    :init   :static
                                    :states {:static {}
                                             :active {}}}}
              :transitions [{:event    :focus
                             :internal true
                             :target   [:field :active]}
                            ]})]
    (is (= [[] [:field] [:field :active]]
           (-> (statechart/initialize {} sc)
               (statechart/process-event sc [:focus])
               (get-in [:configuration :configuration]))))))

(deftest x2
  (let [sc (statechart/make
             {:type   :and
              :states {:form {:type        :and
                              :states      {:field {:type   :xor
                                                    :init   :static
                                                    :states {:static {}
                                                             :active {}}}}
                              :transitions [{:event    :focus
                                             :internal true
                                             :target   [:form :field :active]}
                                            ]}}})]
    (is (= [[] [:form] [:form :field] [:form :field :active]]
           (-> (statechart/initialize {} sc)
               (statechart/process-event sc [:focus])
               (get-in [:configuration :configuration]))))))

(deftest x3
  (let [sc (statechart/make
             {:type   :and
              :states {:form {:type        :and
                              :states      {:field {:type   :xor
                                                    :init   :static
                                                    :states {:static {}
                                                             :active {}}}}
                              :transitions [{:event    :focus
                                             :target   [:form :field :active]}
                                            ]}}})]
    (is (= [[] [:form] [:form :field] [:form :field :active]]
           (-> (statechart/initialize {} sc)
               (statechart/process-event sc [:focus])
               (get-in [:configuration :configuration]))))))

(let [sc    (statechart/make
              {:type   :and
               :states {:form {:type        :and
                               :states      {:field {:type   :xor
                                                     :init   :static
                                                     :states {:static {}
                                                              :active {}}}}
                               :transitions [{:event  :focus
                                              :target [:form :field :active]}
                                             ]}}})
      ctx   (-> (statechart/initialize {} sc)
                (ctx/init-ctx sc [:focus])
                (ctx/pop-event)
                #_(statechart/process-event sc [:focus])
                #_(get-in [:configuration :configuration]))
      transitions (runtime/event-transitions ctx)]
  #_[(->> (runtime/transitions-exit-set ctx transitions)
        (map :id))
   (->> (runtime/transitions-entry-set ctx transitions)
        (map :id))]

  (->> #_(runtime/transitions-entry-set ctx transitions)
    transitions
    (mapcat (fn [t]
              (let [ancestor               (runtime/transition-domain t ctx)
                    immediate-anc-children (->> (runtime/effective-target-states t ctx)
                                                (map #(last (state/proper-ancestors % ancestor)))
                                                (remove nil?))
                    other-children         (if (seq immediate-anc-children)
                                             (set/difference (into #{} (state/substates ancestor))
                                                             immediate-anc-children)
                                             [])
                    states                 (concat (runtime/add-descendants (transition/target-state t) ctx)
                                                   (->> (runtime/effective-target-states t ctx)
                                                        (mapcat #(runtime/add-ancestors % ancestor ctx)))
                                                   ; this is different than scxml, we need to visit other children
                                                   ; from transition domain, as they are exited in trasitions-exit-set
                                                   (->> other-children
                                                        (mapcat #(runtime/add-descendants % ctx))))]
                states)))
       (map :id)))