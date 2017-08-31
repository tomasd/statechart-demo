(ns sc-demo-page.events
  (:require [re-frame.core :as re-frame]
            [sc-demo-page.db :as db]
            [reagent.ratom :refer-macros [reaction]]
            [reagent.core :as reagent]
            [sc-demo-page.statechart1 :as sc]
            [ajax.core :refer [GET POST]]
            [clojure.string :as str]
            [sc-demo-page.view-utils :as wu]
            [sc-demo-page.context :as ctx]))

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

(defn assoc-page-layout [page-name layout]
  (fn [ctx]
    (update-db ctx assoc :page {:name   page-name
                                :layout layout})))

(defn ctx-log [message]
  (fn [ctx]
    (js/console.log message)
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
  {:type    :and
   :enter   [(ctx-log "Entering home page")
             (assoc-page-layout :page/home :layout/column
                                )]
   :history :deep
   :exit    [(dissoc-db-value :home-page)
             (ctx-log "Leaving home page")]
   :states  {:tabs {:init        :top-5
                    :type        :xor
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

(def betting-page
  {:type        :and
   :history     :deep
   :states      {:prematch {:type   :xor
                            :enter  [(ctx-log "Entering prematch state")]
                            :init   :on
                            :states {:on  {:enter       [(update-filter [:betting-page] assoc :prematch true)
                                                         (reset-boxes [:betting-page])]
                                           :transitions [{:event  :toggle-prematch
                                                          :target [:page :betting :page :page/betting :prematch :off]}]}
                                     :off {:enter       [(update-filter [:betting-page] assoc :prematch false)
                                                         (reset-boxes [:betting-page])]
                                           :transitions [{:event  :toggle-prematch
                                                          :target [:page :betting :page :page/betting :prematch :on]}]}}}}
   :enter       [(ctx-log "Entering betting page")
                 (assoc-page-layout :page/betting :layout/column)
                 (update-filter [:betting-page]
                                (fn [old]
                                  (js/console.log "betting page filter" old)
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
                            (reset-boxes [:betting-page])]}
                 {:event   :toggle-live
                  :execute [(update-filter [:betting-page] update :live not)
                            (reset-boxes [:betting-page])]}
                 {:event   :toggle-results
                  :execute [(update-filter [:betting-page] update :results not)
                            (reset-boxes [:betting-page])]}]})

(def mymatches-page
  {:enter [(assoc-page-layout :page/my-matches :layout/column)]})

(def idx (sc/make-statechart
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
                                                                              :execute [(assoc-filter [:betting-page] {:prematch true})]
                                                                              :target  :page/betting}
                                                                             {:event   :toggle-live
                                                                              :execute [(assoc-filter [:betting-page] {:live true})]
                                                                              :target  :page/betting}
                                                                             {:event   :toggle-results
                                                                              :execute [(assoc-filter [:betting-page] {:results true})]
                                                                              :target  :page/betting}]}
                                                        :menu {:enter [(load-menu [:betting :menu])]}
                                                        }}}}}}))

(defn process-event [ctx event]
  (let [configuration (get-in ctx [:db :configuration])
        {:keys [fx configuration] :as x} (sc/process-event idx configuration (select-keys ctx [:db]) event)]
    (assoc-in fx [:db :configuration] configuration)))

(re-frame/reg-event-fx :initialize-db
  #_[re-frame/debug]
  (fn [_ _]
    (let [{:keys [fx configuration] :as x} (sc/initialize idx {:db db/default-db})]
      (assoc-in fx [:db :configuration] configuration))))

(re-frame/reg-event-fx :goto-page
  #_[re-frame/debug]
  (fn [ctx event]
    (process-event ctx event)))

(re-frame/reg-event-fx :set-tab
  #_[re-frame/debug]
  (fn [ctx event]
    (process-event ctx event)))

(re-frame/reg-event-fx :set-menu
  #_[re-frame/debug]
  (fn [ctx event]
    (process-event ctx event)))

(re-frame/reg-event-fx :toggle-prematch
  [re-frame/debug]
  (fn [ctx event]
    (process-event ctx event)))

(re-frame/reg-event-fx :toggle-live
  #_[re-frame/debug]
  (fn [ctx event]
    (process-event ctx event)))

(re-frame/reg-event-fx :toggle-results
  #_[re-frame/debug]
  (fn [ctx event]
    (process-event ctx event)))





(re-frame/reg-fx :load-boxes
                 (fn [requests]
                   (doseq [{:keys [path filter]} requests]
                     (js/console.log "Loading boxes for" path "with filter" filter)
                     (GET "https://live.nike.sk/api/prematch/boxes/portal"
                          {:params          filter
                           :response-format (ajax.core/json-response-format {:raw true})
                           :handler         (fn [response]
                                              (re-frame/dispatch [:boxes-loaded path response]))
                           :error-handler   (fn [response]
                                              (js/console.log "error" response)
                                              )}))))

(re-frame/reg-fx :load-menu (fn [requests]
                              (doseq [{:keys [path filter]} requests]
                                (GET "https://live.nike.sk/api/prematch/menu" {:params          {}
                                                                               :response-format (ajax.core/json-response-format {:raw true})
                                                                               :handler         (fn [response]
                                                                                                  (re-frame/dispatch [:menu-loaded path response]))
                                                                               :error-handler   (fn [response]
                                                                                                  (js/console.log "error" response))}))))

(re-frame/reg-event-db :menu-loaded
  (fn [db [_ path data]]
    (assoc-in db path data)))

(re-frame/reg-event-db :boxes-loaded
  (fn [db [_ path data]]
    (let [db (-> db
                 (update-in path dissoc :boxes)
                 (update-in path dissoc :box-ids))
          db (reduce
               (fn [db box]
                 (assoc-in db (into path [:boxes (:boxId box) :box]) box))
               db
               (:boxes data))
          db (reduce
               (fn [db sport-event]
                 (reduce (fn [db box-id]
                           (assoc-in db (into path [:boxes box-id :sport-events (:sportEventId sport-event) :sport-event]) sport-event))
                         db
                         (:boxIds sport-event)))
               db
               (:sportEvents data))
          db (reduce
               (fn [db market]
                 (reduce (fn [db box-id]
                           (assoc-in db (into path [:boxes box-id :markets (:marketId market) :market]) market))
                         db
                         (:boxIds market)))
               db
               (:markets data))
          db (reduce
               (fn [db bet]
                 (reduce (fn [db box-id]
                           (assoc-in db (into path [:boxes box-id :sport-events (:sportEventId bet) :bets (:betId bet) :bet]) bet))
                         db
                         (into #{} (concat (:boxIds bet)
                                           (get-in data [:betBoxIds (:betId bet)] [])))))
               db
               (:bets data))
          db (assoc-in db (conj path :box-ids)
                       (->> (get-in db (conj path :boxes))
                            vals
                            (map :box)
                            (remove nil?)
                            (sort-by :order)
                            (mapv :boxId)))]
      db)))

(re-frame/reg-event-fx :login
  (fn [ctx event]
    (process-event ctx event)))

(re-frame/reg-event-fx :logout
  (fn [ctx event]
    (process-event ctx event)))