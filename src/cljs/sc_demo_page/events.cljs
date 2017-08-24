(ns sc-demo-page.events

  (:require [re-frame.core :as re-frame]
            [sc-demo-page.db :as db]
            [reagent.ratom :refer-macros [reaction]]
            [reagent.core :as reagent]
            [sc-demo-page.statechart :as sc]))

(declare set-page)
(defn set-page [db page]
  (assoc db :page page))
(declare dispatch)
(defn state-tab [label state cmd]
  (let [active? (reaction (contains? @(re-frame/subscribe [:configuration]) state))]
    (fn [label state cmd]
      [:li [:a {:href     "#"
                :on-click (fn [e]
                            (dispatch cmd))}
            (str (if @active? "* " "") label)]])))

(def sm (let [filter-offer {:id     :filter-offer
                            :type   :and
                            :states [{:id     :prematch
                                      :type   :xor
                                      :states [{:id          :on
                                                :type        :xor
                                                :states      [{:id :enabled}
                                                              {:id :disabled}]
                                                :transitions [{:event     :toggle-section
                                                               :target    :off
                                                               :condition (sc/event-pred? (fn [[_ section]]
                                                                                            (= section :prematch)))}]}
                                               {:id          :off
                                                :type        :xor
                                                :states      [{:id :enabled}
                                                              {:id :disabled}]
                                                :transitions [{:event     :toggle-section
                                                               :target    :on
                                                               :condition (sc/event-pred? (fn [[_ section]]
                                                                                            (= section :prematch)))}]}]}
                                     {:id     :live
                                      :type   :xor
                                      :states [{:id          :on
                                                :type        :xor
                                                :states      [{:id :enabled}
                                                              {:id :disabled}]
                                                :transitions [{:event     :toggle-section
                                                               :target    :off
                                                               :condition (sc/event-pred? (fn [[_ section]]
                                                                                            (= section :live)))}]}
                                               {:id          :off
                                                :type        :xor
                                                :states      [{:id :enabled}
                                                              {:id :disabled}]
                                                :transitions [{:event     :toggle-section
                                                               :target    :on
                                                               :condition (sc/event-pred? (fn [[_ section]]
                                                                                            (= section :live)))}]}]}
                                     {:id     :result
                                      :type   :xor
                                      :states [{:id          :on
                                                :type        :xor
                                                :states      [{:id :enabled}
                                                              {:id :disabled}]
                                                :transitions [{:event     :toggle-section
                                                               :target    :off
                                                               :condition (sc/event-pred? (fn [[_ section]]
                                                                                            (= section :result)))}]}
                                               {:id          :off
                                                :type        :xor
                                                :states      [{:id :enabled}
                                                              {:id :disabled}]
                                                :transitions [{:event     :toggle-section
                                                               :target    :on
                                                               :condition (sc/event-pred? (fn [[_ section]]
                                                                                            (= section :result)))}]}]}
                                     {:id     :video
                                      :type   :xor
                                      :states [{:id          :on
                                                :type        :xor
                                                :states      [{:id :enabled}
                                                              {:id :disabled}]
                                                :transitions [{:event     :toggle-section
                                                               :target    :off
                                                               :condition (sc/event-pred? (fn [[_ section]]
                                                                                            (= section :video)))}]}
                                               {:id          :off
                                                :type        :xor
                                                :enter       [(sc/raise [:load-boxes])]
                                                :states      [{:id :enabled}
                                                              {:id :disabled}]
                                                :transitions [{:event     :toggle-section
                                                               :target    :on
                                                               :condition (sc/event-pred? (fn [[_ section]]
                                                                                            (= section :video)))}]}]}]}
              dataset      {:id     :dataset
                            :type   :and
                            :states [{:id     :data
                                      :type   :xor
                                      :init   :init
                                      :states [{:id          :init
                                                :transitions [{:event     :load-data-success
                                                               :target    :empty
                                                               :condition (sc/event-pred? (fn [[_ data]]
                                                                                            (empty? data)))}
                                                              {:event     :load-data-success
                                                               :target    :paged
                                                               :condition (sc/event-pred? (fn [[_ data]]
                                                                                            (not (:full? data))))}
                                                              {:event     :load-data-success
                                                               :target    :full
                                                               :condition (sc/event-pred? (fn [[_ data]]
                                                                                            (:full? data)))}]}
                                               {:id :empty}
                                               {:id          :paged
                                                :transitions [{:event     :load-data-success
                                                               :target    :paged
                                                               :condition (sc/event-pred? (fn [[_ data]]
                                                                                            (not (:full? data))))}
                                                              {:event     :load-data-success
                                                               :target    :full
                                                               :condition (sc/event-pred? (fn [[_ data]]
                                                                                            (:full? data)))}]}
                                               {:id :full}]}
                                     {:id     :loader
                                      :type   :xor
                                      :init   :init
                                      :states [{:id          :init
                                                :transitions [{:event  :load-data
                                                               :target :loading}]}
                                               {:id          :loading
                                                :transitions [{:event  :load-data-success
                                                               :target :loaded}
                                                              {:event  :load-data-error
                                                               :target :error}]}
                                               {:id          :loaded
                                                :transitions [{:event :load-data :target :loading}]}
                                               {:id          :error
                                                :transitions [{:event :load-data :target :loading}]}]}]}]
          {:id          :page
           :type        :xor
           :init        :home
           :states      [{:id :init}
                         {:id     :tipovanie
                          :type   :and
                          :states [{:id          :filter
                                    :type        :xor
                                    :init        :all
                                    :states      [{:id          :all
                                                   :transitions [{:event   :set-date
                                                                  :target  :date
                                                                  :execute [(sc/raise [:dummy])]}]
                                                   :enter       [(sc/raise [:load-boxes :all])]
                                                   :exit        [(sc/raise [:clear-all-boxes])]}
                                                  {:id          :date
                                                   :transitions [{:event  :set-menu-item
                                                                  :target :date-menu-item}
                                                                 {:event  :clear-date
                                                                  :target :all}]
                                                   :enter       [(sc/raise [:load-boxes :date])
                                                                 (fn [ctx]
                                                                   (sc/update-db ctx update :value (fnil inc 0)))]}
                                                  {:id          :menu-item
                                                   :transitions [{:event  :set-date
                                                                  :target :date-menu-item}]
                                                   :enter       [(sc/raise [:load-boxes :menu-item])]}
                                                  {:id          :date-menu-item
                                                   :transitions [{:event  :clear-date
                                                                  :target :date}]
                                                   :enter       [(sc/raise [:load-boxes :date-menu-item])]}]
                                    :transitions [{:event  :show-all
                                                   :target [:page :tipovanie :filter :all]}]}
                                   filter-offer
                                   dataset]}
                         {:id     :home
                          :type   :and
                          :enter  [(fn [ctx]
                                     (js/console.log "Enter home")
                                     ctx)
                                   (fn [ctx]
                                     (sc/update-db ctx set-page {:name   :page/home
                                                                 :layout {:layout/top  [:div "top"]
                                                                          :layout/main [:div "home"
                                                                                        [:ul
                                                                                         [state-tab "Top 5" [:page :home :filter :top-5] [:set-tab :top5]]
                                                                                         [state-tab "10 Naj" [:page :home :filter :10-naj] [:set-tab :10-naj]]
                                                                                         [state-tab "Superkurzy" [:page :home :filter :superkurzy] [:set-tab :superkurzy]]
                                                                                         [state-tab "Top ponuka" [:page :home :filter :top-ponuka] [:set-tab :top-ponuka]]]
                                                                                        ]}}))]
                          :exit   [(fn [ctx]
                                     (js/console.log "Leave home"))]
                          :states [{:id          :filter
                                    :type        :xor
                                    :states      [{:id    :top-5
                                                   :enter [(sc/raise [:load-boxes :top5])]}
                                                  {:id    :10-naj
                                                   :enter [(sc/raise [:load-boxes :10naj])]}
                                                  {:id    :superkurzy
                                                   :enter [(sc/raise [:load-boxes :superkurzy])]}
                                                  {:id    :top-ponuka
                                                   :enter [(sc/raise [:load-boxes :top-ponuka])]}]
                                    :transitions [{:event     :set-tab
                                                   :target    :top-5
                                                   :condition (sc/event-pred? (fn [[_ tab]]
                                                                                (= tab :top-5)))}
                                                  {:event     :set-tab
                                                   :target    :10-naj
                                                   :condition (sc/event-pred? (fn [[_ tab]]
                                                                                (= tab :10-naj)))}
                                                  {:event     :set-tab
                                                   :target    :superkurzy
                                                   :condition (sc/event-pred? (fn [[_ tab]]
                                                                                (= tab :superkurzy)))}
                                                  {:event     :set-tab
                                                   :target    :top-ponuka
                                                   :condition (sc/event-pred? (fn [[_ tab]]
                                                                                (= tab :top-ponuka)))}]}
                                   filter-offer
                                   dataset]}
                         {:id     :superkurzy
                          :type   :and
                          :states [{:id    :filter
                                    :enter [(sc/raise [:load-boxes :superkurzy])]}
                                   filter-offer
                                   dataset]}
                         {:id     :moje
                          :type   :and
                          :states [{:id          :filter
                                    :type        :xor
                                    :states      [{:id    :all
                                                   :enter [(sc/raise [:load-boxes :moje/all])]}
                                                  {:id    :menu-item
                                                   :enter [(sc/raise [:load-boxes :moje/menu-item])]}
                                                  {:id    :my-matches
                                                   :enter [(sc/raise [:load-boxes :moje/matches])]}]
                                    :transitions [{:event  :set-menu-item
                                                   :target :menu-item}
                                                  {:event  :set-my-matches
                                                   :target :mymatches}
                                                  {:event  :set-all
                                                   :target :all}]}
                                   filter-offer
                                   dataset]}]
           :transitions [{:event  :show-all
                          :target [:page :tipovanie :filter :all]
                          ;:condition (not-in-state? [:page :tipovanie])
                          }
                         {:event  :set-date
                          :target [:page :tipovanie :filter :date]
                          ;:condition (not-in-state? [:page :tipovanie])
                          }
                         {:event  :set-menu-item
                          :target [:page :tipovanie :filter :set-menu-item]
                          ;:condition (not-in-state? [:page :tipovanie])
                          }
                         {:event     :goto-page
                          :target    [:page :tipovanie]
                          :condition (sc/event-pred? (fn [[_ page]]
                                                       (= page :page/tipovanie)))}
                         {:event     :goto-page
                          :target    [:page :home]
                          :condition (sc/event-pred? (fn [[_ page]]
                                                       (= page :page/home)))}
                         {:event     :goto-page
                          :target    [:page :superkurzy]
                          :condition (sc/event-pred? (fn [[_ page]]
                                                       (= page :page/superkurzy)))}
                         {:event     :goto-page
                          :target    [:page :moje]
                          :condition (sc/event-pred? (fn [[_ page]]
                                                       (= page :page/moje)))}]}))
(def idx
  (sc/make-machine sm))

(defprotocol IPage
  (page-layout [this]))

(re-frame/reg-event-db :load-boxes
  [re-frame/debug]
  (fn [db _]
    db))
(re-frame/reg-event-fx :dispatch
  [re-frame/debug]
  (fn [ctx [_ event]]
    (sc/process-event idx ctx event)))

(re-frame/reg-event-fx :initialize-db
  [re-frame/debug]
  (fn [_ _]
    (:ctx (sc/initialize idx {:db db/default-db}))))
(defn dispatch [event]
  (re-frame/dispatch [:dispatch event]))

