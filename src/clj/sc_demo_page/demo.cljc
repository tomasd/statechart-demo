(ns sc-demo-page.demo
  (:require [statechart.core :as statechart]
            [statechart.context :as ctx]
            [statechart.state :as state]))

(defn event-pred? [predicate]
  (fn [ctx]
    (predicate (ctx/current-event ctx))))

(defn update-db [ctx f & args]
  (apply update-in ctx [:fx :db] f args))

(defn goto-substate [event target & opts]
  {:event     event
   :target    target
   :internal  (contains? (into #{} opts) :internal)
   :condition (event-pred? (fn [[_ arg]]
                             (= arg target)))})

(defn goto-substate' [event arg target & opts]
  {:event     event
   :target    target
   :internal  (contains? (into #{} opts) :internal)
   :condition (event-pred? (fn [[_ arg']]
                             (= arg arg')))})

(defn assoc-page-layout [page-name layout]
  (fn [ctx]
    (update-db ctx assoc :page {:name   page-name
                                :layout layout})))

(defn ctx-log [message]
  (fn [ctx]
    #?(:cljs (js/console.log message)
       :clj  (prn message))
    ctx))

(def loading-page
  {:enter [(assoc-page-layout :page/loading :layout/blank)]})

(defn assoc-db-value [path value]
  (fn [ctx]
    (update-db ctx assoc-in path value)))

(defn make-filter [filter]
  (merge {:limit     "50"
          :live      false
          :prematch  false
          :results   false
          :videoOnly false}
         filter))
(defn update-filter [path f & args]
  (fn [ctx]
    (apply update-db ctx update-in (conj path :filter) f args)))

(defn assoc-filter
  ([path filter]
   (fn [ctx]
     (assoc-filter ctx path filter)))
  ([ctx path filter]
   (update-db ctx assoc-in (conj path :filter) (make-filter filter))))

(defn reset-boxes [path]
  (fn [ctx]
    (let [filter (get-in ctx (into [:fx :db] (conj path :filter)))
          ctx    (-> ctx
                     (update-in [:fx :load-boxes] (fnil conj [])
                                {:path   (conj path :data)
                                 :filter filter}))]
      ctx)))

(defn load-menu [path]
  (fn [ctx]
    (-> ctx
        (update-in [:fx :load-menu] (fnil conj [])
                   {:path (conj path :data)}))))

(defn dissoc-db-value [key]
  (fn [ctx]
    (update-db ctx dissoc key)))

(def home-page
  {:type   :and
   :enter  [(ctx-log "Entering home page")
            (assoc-page-layout :page/home :layout/column
                               )]
   :exit   [(dissoc-db-value :home-page)
            (ctx-log "Leaving home page")]
   :states {
            :tabs {:init        :top-5
                   :type        :xor
                   :history     :shallow
                   :states      {:top-5      {:enter [(assoc-db-value [:home-page :current-tab] :top-5)
                                                      (assoc-filter [:home-page] {:boxId    ["top5"]
                                                                                  :prematch true})
                                                      (reset-boxes [:home-page])]}
                                 :10-naj     {:enter [(assoc-db-value [:home-page :current-tab] :10-naj)
                                                      (assoc-filter [:home-page] {:boxId    ["naj10"]
                                                                                  :prematch true})
                                                      (reset-boxes [:home-page])]}
                                 :superkurzy {:enter [(assoc-db-value [:home-page :current-tab] :superkurzy)
                                                      (assoc-filter [:home-page] {:boxId    ["superoffer" "superchance" "duel"]
                                                                                  :prematch true})
                                                      (reset-boxes [:home-page])]}
                                 :top-ponuka {:enter [(assoc-db-value [:home-page :current-tab] :top-ponuka)
                                                      (assoc-filter [:home-page] {:filter   ["topponuka"]
                                                                                  :prematch true})
                                                      (reset-boxes [:home-page])]}}
                   :transitions [(goto-substate :set-tab :top-5 :internal)
                                 (goto-substate :set-tab :10-naj :internal)
                                 (goto-substate :set-tab :superkurzy :internal)
                                 (goto-substate :set-tab :top-ponuka :internal)]}}})

(defn filter-flag-sc [{:keys [event flag init]}]
  {:type    :xor
   :init    init
   :history :shallow
   :states  {:on  {:enter       [(update-filter [:betting-page] assoc flag true)
                                 (reset-boxes [:betting-page])]
                   :transitions [{:event  event
                                  :target [:page :betting :page :page/betting flag :off]}]}
             :off {:enter       [(update-filter [:betting-page] assoc flag false)
                                 (reset-boxes [:betting-page])]
                   :transitions [{:event  event
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
   :enter       [(ctx-log "Entering betting page")
                 (assoc-page-layout :page/betting :layout/column)
                 (update-filter [:betting-page]
                                (fn [old]
                                  (if-not old
                                    (make-filter {:live     true
                                                  :prematch true})
                                    old)))
                 (reset-boxes [:betting-page])]
   :exit        [(dissoc-db-value :betting-page)
                 (ctx-log "Leaving betting page")]
   :transitions [{:event   :set-menu
                  :execute [(fn [ctx]
                              (let [[_ slug] (ctx/current-event ctx)
                                    ctx ((update-filter [:betting-page] assoc :menu slug) ctx)]
                                ctx))
                            (reset-boxes [:betting-page])]}]})

(def mymatches-page
  {:enter [(assoc-page-layout :page/my-matches :layout/column)]})

(def idx (statechart/make
           {:type   :and
            :states {:push {:enter [(ctx-log "Starting push")]
                            :exit  [(ctx-log "Stopping push")]}
                     :user {:type        :xor
                            :init        :anonymous
                            :enter       [(ctx-log "Starting user")]
                            :exit        [(ctx-log "Stopping user")]
                            :states      {:authenticated {:enter [(ctx-log "User authenticated")
                                                                  (assoc-db-value [:user :authenticated?] true)]}
                                          :anonymous     {:enter [(ctx-log "Anonymous user")
                                                                  (assoc-db-value [:user :authenticated?] false)]
                                                          }}
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
                                                               :states      {:page/loading    loading-page
                                                                             :page/home       home-page
                                                                             :page/betting    betting-page
                                                                             :page/my-matches mymatches-page}
                                                               :transitions [(goto-substate :goto-page :page/home)
                                                                             (goto-substate :goto-page :page/betting)
                                                                             (goto-substate :goto-page :page/my-matches)
                                                                             {:event   :set-menu
                                                                              :execute [(fn [ctx]
                                                                                          (let [[_ slug] (ctx/current-event ctx)
                                                                                                ctx (assoc-filter ctx [:betting-page] {:menu     slug
                                                                                                                                       :prematch true
                                                                                                                                       :live     true})]
                                                                                            ctx))]
                                                                              :target  :page/betting}
                                                                             {:event   :toggle-prematch
                                                                              :target  [:page :betting :page :page/betting :prematch :on]}
                                                                             {:event   :toggle-live
                                                                              :target  [:page :betting :page :page/betting :live :on]}
                                                                             {:event   :toggle-results
                                                                              :target  [:page :betting :page :page/betting :results :on]}]}
                                                        :menu {:enter [(load-menu [:betting :menu])]}
                                                        }}}}}}))

(defn initialize []
  (let [{:keys [fx configuration] :as x} (statechart/initialize {:db {}} idx)]
    (assoc-in fx [:db :configuration] configuration)))
(defn process-event [ctx event]
  (let [configuration (get-in ctx [:db :configuration])
        {:keys [fx configuration] :as x} (statechart/process-event {:configuration configuration
                                                                    :fx            (select-keys ctx [:db])} idx event)]
    (assoc-in fx [:db :configuration] configuration)))
