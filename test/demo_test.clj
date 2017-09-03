(ns .demo-test
  (:require [clojure.test :refer :all]
            [statechart.core :as statechart]
            [statechart.context :as ctx]
            [statechart.runtime :as runtime]))


(defn event-pred? [predicate]
  (fn [ctx]
    (predicate (ctx/current-event ctx))))

(defn goto-substate [event target & opts]
  {:event     event
   :target    target
   :internal  (contains? (into #{} opts) :internal)
   :condition (event-pred? (fn [[_ arg]]
                             (= arg target)))})

(def home-page
  {:type   :and
   :states {:tabs {:init        :top-5
                   :type        :xor
                   :history     :shallow
                   :states      {:top-5      {}
                                 :10-naj     {}
                                 :superkurzy {}
                                 :top-ponuka {}}
                   :transitions [(goto-substate :set-tab :top-5 :internal)
                                 (goto-substate :set-tab :10-naj :internal)
                                 (goto-substate :set-tab :superkurzy :internal)
                                 (goto-substate :set-tab :top-ponuka :internal)]}}})

(defn filter-flag-sc [{:keys [event flag init]}]
  {:type    :xor
   :init    init
   :history :shallow
   :states  {:on  {:transitions [{:event  event
                                  :target [:page :betting :page :page/betting flag :off]}]}
             :off {:transitions [{:event  event
                                  :target [:page :betting :page :page/betting flag :on]}]}}})

(def betting-page
  {:type        :and
   :states      {:prematch (filter-flag-sc {:event :toggle-prematch
                                            :flag  :prematch
                                            :init  :on})
                 :live     (filter-flag-sc {:event :toggle-live
                                            :flag  :live
                                            :init  :on})
                 :results  (filter-flag-sc {:event :toggle-results
                                            :flag  :results
                                            :init  :off})}


   :transitions [{:event :set-menu}]})

(def sc (statechart/make
          {:type   :and
           :states {:push {}
                    :user {:type        :xor
                           :init        :anonymous
                           :states      {:authenticated {}
                                         :anonymous     {}}
                           :transitions [{:event    :logout
                                          :internal true
                                          :target   :anonymous}
                                         {:event    :login
                                          :internal true
                                          :target   :authenticated}]}
                    :page {:type   :xor
                           :init   :betting
                           :states {:betting {:type   :and
                                              :states {:page {:type        :xor
                                                              :init        :page/loading
                                                              :states      {:page/loading    {}
                                                                            :page/home       home-page
                                                                            :page/betting    betting-page
                                                                            :page/my-matches {}}
                                                              :transitions [(goto-substate :goto-page :page/home)
                                                                            (goto-substate :goto-page :page/betting)
                                                                            (goto-substate :goto-page :page/my-matches)
                                                                            {:event  :set-menu
                                                                             :target :page/betting}
                                                                            {:event  :toggle-prematch
                                                                             :target [:page :betting :page :page/betting :prematch :on]}
                                                                            {:event  :toggle-live
                                                                             :target [:page :betting :page :page/betting :live :on]}
                                                                            {:event  :toggle-results
                                                                             :target [:page :betting :page :page/betting :results :on]}]}
                                                       :menu {}
                                                       }}}}}}))

(defn prepare-event [ctx event]
  (-> ctx
      (ctx/init-ctx sc event)
      (ctx/pop-event)))

(deftest test-initialize
  (is (= (-> (statechart/initialize {} sc)
             (get-in [:configuration :configuration]))
         [[:push] [:user :anonymous] [:page :betting :page :page/loading] [:page :betting :menu]])))

(deftest test-page-handlers
  (is (= (-> (statechart/initialize {} sc)
             (statechart/process-event sc [:goto-page :page/home])
             (get-in [:configuration :configuration]))
         [[:push]
          [:user :anonymous]
          [:page :betting :page :page/home :tabs :top-5]
          [:page :betting :menu]]))

  (is (= (-> (statechart/initialize {} sc)
             (statechart/process-event sc [:goto-page :page/betting])
             (get-in [:configuration :configuration]))
         [[:push]
          [:user :anonymous]
          [:page :betting :page :page/betting :prematch :on]
          [:page :betting :page :page/betting :live :on]
          [:page :betting :page :page/betting :results :off]
          [:page :betting :menu]]))

  (is (= (-> (statechart/initialize {} sc)
             (statechart/process-event sc [:goto-page :page/my-matches])
             (get-in [:configuration :configuration]))
         [[:push]
          [:user :anonymous]
          [:page :betting :page :page/my-matches]
          [:page :betting :menu]]))

  (is (= (-> (statechart/initialize {} sc)
             (statechart/process-event sc [:goto-page :page/home])
             (statechart/process-event sc [:goto-page :page/betting])
             (get-in [:configuration :configuration]))
         [[:push]
          [:user :anonymous]
          [:page :betting :page :page/betting :prematch :on]
          [:page :betting :page :page/betting :live :on]
          [:page :betting :page :page/betting :results :off]
          [:page :betting :menu]]))

  (is (= (-> (statechart/initialize {} sc)
             (statechart/process-event sc [:goto-page :page/home])
             (statechart/process-event sc [:goto-page :page/betting])
             (statechart/process-event sc [:goto-page :page/home])
             (get-in [:configuration :configuration]))
         [[:push]
          [:user :anonymous]
          [:page :betting :page :page/home :tabs :top-5]
          [:page :betting :menu]])))

(def ctx (-> (statechart/initialize {} sc)
             (statechart/process-event sc [:goto-page :page/home])
             (statechart/process-event sc [:toggle-prematch])
             (statechart/process-event sc [:toggle-prematch])
             (statechart/process-event sc [:goto-page :page/home])
             #_(statechart/process-event sc [:toggle-prematch])
             (prepare-event [:toggle-prematch])

             #_(get-in [:configuration :configuration])))
(let [
      transitions (runtime/event-transitions ctx)]
  (->> (runtime/transitions-exit-set ctx transitions)
       (map :id))
  (->> (runtime/transitions-entry-set ctx transitions)
       (map :id)))

(deftest test-toggle-prematch
  (is (= (-> (statechart/initialize {} sc)
             (statechart/process-event sc [:goto-page :page/home])
             (statechart/process-event sc [:toggle-prematch])
             (statechart/process-event sc [:toggle-prematch])
             (statechart/process-event sc [:goto-page :page/home])
             (statechart/process-event sc [:toggle-prematch])

             (get-in [:configuration :configuration]))
         [[:push]
          [:user :anonymous]
          [:page :betting :page :page/betting :prematch :on]
          [:page :betting :page :page/betting :live :on]
          [:page :betting :page :page/betting :results :off]
          [:page :betting :menu]]))
  )


(run-tests)