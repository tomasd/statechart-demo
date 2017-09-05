(ns nike-sk.statechart
  (:require [statechart.core :as sc]
            [statechart.context :as ctx]
            [statechart.path :as path]
            [statechart.core :as statechart]
            [statechart.runtime :as runtime]
            [cljs-time.core :as time]))
(defn ctx-log [message]
  (fn [ctx]
    #?(:cljs (js/console.log message)
       :clj  (prn message))
    ctx))

(defn update-db [f & args]
  (fn [ctx]
    (apply update-in ctx [:fx :db] f args)))

(defn db-event [f & args]
  (fn [ctx]
    (apply update-in ctx [:fx :db] f (cons (ctx/current-event ctx) args))))

(defn assoc-event-arg [path]
  (fn [ctx]
    (let [[_ event-arg] (ctx/current-event ctx)]
      (update-in ctx [:fx :db] assoc-in path event-arg))))

(defn event-pred? [predicate]
  (fn [ctx]
    (predicate (ctx/current-event ctx))))

(defn goto-substate [event target & opts]
  {:event     event
   :target    target
   :internal  (contains? (into #{} opts) :internal)
   :condition (event-pred? (fn [[_ arg]]
                             (= arg target)))})

(def default-filter
  {:limit     50
   :live      false
   :prematch  false
   :results   false
   :videoOnly false})

(defn reset-boxes [path filter]
  (fn [ctx]
    (update-in ctx [:fx :fx/reset-boxes] (fnil conj []) {:path   path
                                                         :filter filter})))

(defn push-event [event]
  (fn [ctx]
    (ctx/push-event ctx event)))

(defn request-reset-boxes [path]
  (push-event [:request-reset-boxes path]))

(defn request-sport-menu [path]
  (fn [ctx]
    (update-in ctx [:fx :fx/reset-sport-menu] (fnil conj []) {:path path})))

(defn boxfilter-section [event path section init]
  {:type    :xor
   :init    init
   :history :shallow
   :states  {:on  {:enter       [(ctx-log (str "Enter " section " :on"))
                                 (update-db assoc-in (into path [:filter section]) true)
                                 (request-reset-boxes path)]
                   :transitions [{:event  event
                                  :target (path/parent [:off])}]}
             :off {:enter       [(ctx-log (str "Enter " section " :off"))
                                 (update-db assoc-in (into path [:filter section]) false)
                                 (request-reset-boxes path)]
                   :transitions [{:event  event
                                  :target (path/parent [:on])}]}}})

(def statechart
  (sc/make
    {:type        :and
     :states      {:push    {}
                   :layout  {:type   :xor
                             :init   :3-col
                             :states {:3-col {:enter       [(update-db assoc-in [:page :layout] :layout/column-3)]
                                              :transitions [{:event  :toggle-layout
                                                             :target (path/sibling :4-col)}]}
                                      :4-col {:enter       [(update-db assoc-in [:page :layout] :layout/column-4)]
                                              :transitions [{:event  :toggle-layout
                                                             :target (path/sibling :3-col)}]}}}
                   :content {:type   :and
                             :states {:page       {:type        :xor
                                                   :init        :page/home
                                                   :history     :shallow
                                                   :states      {:page/home    {:type   :and
                                                                                :enter  [(update-db assoc-in [:page :name] :page/home)]
                                                                                :states {:tabs {:type        :xor
                                                                                                :history     :shallow
                                                                                                :init        :top-5
                                                                                                :states      {:top-5      {:enter [(update-db assoc-in [:home-page :current-tab] :top-5)
                                                                                                                                   (reset-boxes [:home-page :box-store] (assoc default-filter
                                                                                                                                                                          :prematch true
                                                                                                                                                                          :boxId ["top5"]))]}
                                                                                                              :10-naj     {:enter [(update-db assoc-in [:home-page :current-tab] :10-naj)
                                                                                                                                   (reset-boxes [:home-page :box-store] (assoc default-filter
                                                                                                                                                                          :prematch true
                                                                                                                                                                          :boxId ["naj10"]))]}
                                                                                                              :superkurzy {:enter [(update-db assoc-in [:home-page :current-tab] :superkurzy)
                                                                                                                                   (reset-boxes [:home-page :box-store] (assoc default-filter
                                                                                                                                                                          :prematch true
                                                                                                                                                                          :boxId ["superoffer" "superchance" "duel"]))]}
                                                                                                              :top-ponuka {:enter [(update-db assoc-in [:home-page :current-tab] :top-ponuka)
                                                                                                                                   (reset-boxes [:home-page :box-store] (assoc default-filter
                                                                                                                                                                          :prematch true
                                                                                                                                                                          :filter ["topponuka"]))]}}
                                                                                                :transitions [(goto-substate :set-tab :top-5)
                                                                                                              (goto-substate :set-tab :10-naj)
                                                                                                              (goto-substate :set-tab :superkurzy)
                                                                                                              (goto-substate :set-tab :top-ponuka)]}}}
                                                                 :page/betting {:type   :and
                                                                                :enter  [(ctx-log "Enter :page/betting")
                                                                                         (update-db assoc-in [:page :name] :page/betting)
                                                                                         (update-db assoc-in [:betting-page :filter] (assoc default-filter
                                                                                                                                       :prematch true
                                                                                                                                       :live true))
                                                                                         (request-reset-boxes [:betting-page])
                                                                                         ]
                                                                                :states {:filter {:type   :and
                                                                                                  :states {:prematch (boxfilter-section :toggle-prematch [:betting-page] :prematch :on)
                                                                                                           :live     (boxfilter-section :toggle-live [:betting-page] :live :on)
                                                                                                           :results  (boxfilter-section :toggle-results [:betting-page] :results :off)
                                                                                                           :filter   {:type   :xor
                                                                                                                      :enter  [(ctx-log "Enter filter ")
                                                                                                                               (request-reset-boxes [:betting-page])]
                                                                                                                      :init   :all
                                                                                                                      :states {:all       {:enter       [(request-reset-boxes [:betting-page])]
                                                                                                                                           :transitions [{:event  :set-date
                                                                                                                                                          :target (path/sibling :date)}
                                                                                                                                                         {:event   :set-menu
                                                                                                                                                          :execute [(assoc-event-arg [:betting-page :filter :menu])]
                                                                                                                                                          :target  (path/sibling :menu)}]}
                                                                                                                               :date      {:enter       [(request-reset-boxes [:betting-page])]
                                                                                                                                           :transitions [{:event  :clear-date
                                                                                                                                                          :target (path/sibling :all)}
                                                                                                                                                         {:event  :set-date
                                                                                                                                                          :target (path/this)}
                                                                                                                                                         {:event   :set-menu
                                                                                                                                                          :execute [(assoc-event-arg [:betting-page :filter :menu])]
                                                                                                                                                          :target  (path/sibling :menu-date)}]}
                                                                                                                               :menu      {:enter       [(request-reset-boxes [:betting-page])]
                                                                                                                                           :transitions [{:event  :set-date
                                                                                                                                                          :target (path/sibling :menu-date)}
                                                                                                                                                         {:event   :set-menu
                                                                                                                                                          :execute [(assoc-event-arg [:betting-page :filter :menu])]
                                                                                                                                                          :target  (path/this)}
                                                                                                                                                         {:event  :clear-menu
                                                                                                                                                          :target (path/sibling :all)}]}
                                                                                                                               :menu-date {:enter       [(request-reset-boxes [:betting-page])]
                                                                                                                                           :transitions [{:event  :clear-date
                                                                                                                                                          :target (path/sibling :menu)}
                                                                                                                                                         {:event  :set-date
                                                                                                                                                          :target (path/this)}
                                                                                                                                                         {:event   :set-menu
                                                                                                                                                          :execute [(assoc-event-arg [:betting-page :filter :menu])]
                                                                                                                                                          :target  (path/this)}
                                                                                                                                                         {:event  :clear-menu
                                                                                                                                                          :target (path/sibling :date)}]}}}}}}}}
                                                   :transitions [(goto-substate :goto-page :page/home)
                                                                 (goto-substate :goto-page :page/betting)
                                                                 {:event  :toggle-prematch
                                                                  :target (path/child [:page/betting :filter :prematch :on])}
                                                                 {:event  :toggle-live
                                                                  :target (path/child [:page/betting :filter :live :on])}
                                                                 {:event  :toggle-results
                                                                  :target (path/child [:page/betting :filter :results :on])}
                                                                 {:event  :toggle-layout
                                                                  :target (path/this)}
                                                                 {:event   :set-menu
                                                                  :execute [(assoc-event-arg [:betting-page :filter :menu])]
                                                                  :target  (path/child [:page/betting :filter :filter :menu])}
                                                                 {:event   :set-date
                                                                  :execute [(assoc-event-arg [:betting-page :filter :date])]
                                                                  :target  (path/child [:page/betting :filter :filter :date])}]}
                                      :sport-menu {:enter [(ctx-log "Enter sport menu")
                                                           (request-sport-menu [:menu])
                                                           (update-db assoc-in [:betting-days] (vec (for [i (range -7 10)]
                                                                                                      (-> (time/now)
                                                                                                          (time/at-midnight)
                                                                                                          (time/plus (time/days i))))))]}

                                      :betslip    {}}}}
     :transitions [{:event   :request-reset-boxes
                    :execute [(fn [ctx]
                                (let [[_ path] (ctx/current-event ctx)
                                      filter (get-in ctx (into [:fx :db] (conj path :filter)))]
                                  (update-in ctx [:fx :fx/reset-boxes] (fnil conj []) {:path   (conj path :box-store)
                                                                                       :filter filter})))]}]}))

(comment
  (let [ctx         (-> (statechart/initialize {} statechart)
                        (ctx/init-ctx statechart [:set-menu "/futbal"])
                        (ctx/pop-event)
                        #_(runtime/event-transitions)
                        #_(statechart/process-event statechart [:set-menu "/futbal"]))
        transitions (runtime/event-transitions ctx)]
    (map :id (:configuration (runtime/enter-transition-states ctx transitions)))
    #_(-> ctx
          (runtime/microstep transitions)
          (ctx/pop-event)
          #_(runtime/microstep transitions)
          #_(update :configuration #(->> %
                                         (sort-by :order)
                                         (map :id)))
          (update :internal-queue #(into [] %))
          )
    (-> ctx
        (runtime/run)
        (update :configuration #(->> %
                                     (sort-by :order)
                                     (map :id))))

    (-> (statechart/initialize {} statechart)
        (statechart/process-event statechart [:set-menu "/futbal"]))))