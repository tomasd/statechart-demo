(ns statechart-test
  (:require [clojure.test :refer :all]
            [statechart.core :as statechart]
            [statechart.state :as state]
            [statechart.runtime :as runtime]
            [statechart.context :as ctx]
            [statechart.transition :as transition]
            [clojure.set :as set]))

(deftest test-internal-1
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

(deftest test-internal-2
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

(deftest test-3
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

