(ns sc-demo-page.views1
  (:require [re-frame.core :as re-frame]
            [sc-demo-page.db :as db]
            [sc-demo-page.statechart :as sc]
            [reagent.ratom :refer-macros [reaction]]
            [clojure.string :as str]
            [ajax.core :refer [GET POST]]
            [reagent.core :as reagent]))

(defn classnames [m]
  (->> m
       (filter val)
       keys
       (str/join " ")))

(re-frame/reg-sub :page
                  (fn [db]
                    (get db :page)))

(re-frame/reg-sub :current-page-name
                  :<- [:page]
                  (fn [page _]
                    (:name page)))

(re-frame/reg-sub :home-page
                  (fn [db _]
                    (:home-page db)))

(re-frame/reg-sub :home-page/current-tab
                  :<- [:home-page]
                  (fn [home-page]
                    (:current-tab home-page)))

(re-frame/reg-sub :home-page/tab-boxes
                  :<- [:home-page]
                  (fn [home-page [_]]
                    (get-in home-page [:data] [])))


(re-frame/reg-sub :betting-page
                  (fn [db _]
                    (:betting-page db)))
(re-frame/reg-sub :betting-page/boxes
                  :<- [:betting-page]
                  (fn [betting-page [_]]
                    (get-in betting-page [:data] [])))

(re-frame/reg-sub :betting-page/filter
                  :<- [:betting-page]
                  (fn [betting-page [_]]
                    (get-in betting-page [:filter] {})))

(re-frame/reg-sub :menu
                  (fn [db _]
                    (get-in db [:betting :menu :data])))

(defn goto-substate [event target & opts]
  {:event     event
   :target    target
   :internal  (contains? (into #{} opts) :internal)
   :condition (sc/event-pred? (fn [[_ arg]]
                                (= arg target)))})

(defn assoc-page-layout [page-name layout positions]
  (fn [ctx]
    (sc/update-db ctx assoc :page {:name      page-name
                                   :layout    layout
                                   :positions positions})))

(defn ctx-log [message]
  (fn [ctx]
    (js/console.log message)
    ctx))

(def loading-page
  {:enter [(assoc-page-layout :page-loading :blank {:blank-layout/center [:div [:i.fa.fa-spinner.fa-5x.fa-spin]]})]})

(defn handle-dispatch [cmd]
  (fn [e]
    (re-frame/dispatch cmd)
    (.preventDefault e)))


(defn page-nav-link [page label]
  (let [current-page (re-frame/subscribe [:current-page-name])]
    (fn [page label]
      [:a.nav-link {:href     "#"
                    :class    (classnames {"active" @(reaction (= page @current-page))})
                    :on-click (handle-dispatch [:goto-page page])}
       label])))

(defn top-menu-panel []
  (let [current-page (re-frame/subscribe [:current-page-name])]
    [:nav.navbar.navbar-expand-lg.navbar-light.bg-light
     [:a.navbar-brand {:href "#"} "Company logo"]
     [:button.navbar-toggler {:type "button"}
      [:span.navbar-toggler-icon]]

     [:div.collapse.navbar-collapse
      [:ul.navbar-nav.mr-auto
       [:li.nav-item {:class (classnames {"active" @(reaction (or (= :page/home @current-page)
                                                                  (= :page/betting @current-page)
                                                                  (= :page/my-matches @current-page)))})}
        [:a.nav-link {:href     "#"
                      :on-click (handle-dispatch [:goto-page :page/home])} "Betting"]]

       [:li.nav-item
        [:a.nav-link {:href "#"} "Other"]]
       ]]]))



(defn sport-menu-item [box-filter item]
  [:li
   [:a {:href     (:slug item)
        :on-click (handle-dispatch [:set-menu (:slug item)])}
    (str (when @(reaction (= (:slug item) (get @box-filter :menu))) " * ")
         (:label item))]
   (when (seq (:items item))
     [:ul
      (for [[i item] (map-indexed vector (:items item))]
        ^{:key i} [sport-menu-item box-filter item])])])

(defn sport-menu-panel [box-filter]
  (let [menu (re-frame/subscribe [:menu])]
    (fn []
      [:ul
       (for [[i item] (map-indexed vector (:items @menu))]
         ^{:key i} [sport-menu-item box-filter item])])))

(defn menu-panel [box-filter]
  [:div.nav.flex-column.nav-pills
   [page-nav-link :page/home "Home"]
   [page-nav-link :page/betting "Betting"]
   [page-nav-link :page/my-matches "My matches"]
   [:hr]
   [sport-menu-panel box-filter]])

(defn assoc-db-value [path value]
  (fn [ctx]
    (sc/update-db ctx assoc-in path value)))

(defn make-filter [filter]
  (merge {:limit     "50"
          :live      false
          :prematch  false
          :results   false
          :videoOnly false}
         filter))
(defn update-filter [path f & args]
  (fn [ctx]
    (apply sc/update-db ctx update-in (conj path :filter) f args)))

(defn assoc-filter
  ([path filter]
   (fn [ctx]
     (assoc-filter ctx path filter)))
  ([ctx path filter]
   (sc/update-db ctx assoc-in (conj path :filter) (make-filter filter))))

(defn reset-boxes [path]
  (fn [ctx]
    (let [filter (get-in ctx (into [:ctx :db] (conj path :filter)))]
      (-> ctx
          (update-in [:ctx :load-boxes] (fnil conj [])
                     {:path   (conj path :data)
                      :filter filter})))))

(defn load-menu [path]
  (fn [ctx]
    (-> ctx
        (update-in [:ctx :load-menu] (fnil conj [])
                   {:path (conj path :data)}))))

(defn tab [current-tab tab label cmd]
  [:li.nav-item
   [:a.nav-link {:href     "#"
                 :class    (classnames {"active" @(reaction (= tab @current-tab))})
                 :on-click (handle-dispatch cmd)
                 } label]])

(defn home-page-tabs []
  (let [current-tab (re-frame/subscribe [:home-page/current-tab])]
    (fn []
      [:ul.nav.nav-tabs
       [tab current-tab :top-5 "Top 5" [:set-tab :top-5]]
       [tab current-tab :10-naj "10 naj" [:set-tab :10-naj]]
       [tab current-tab :superkurzy "Superkurzy" [:set-tab :superkurzy]]
       [tab current-tab :top-ponuka "Top ponuka" [:set-tab :top-ponuka]]])))

(defn omnifilter [box-filter]
  [:div
   [:label [:input {:type      "checkbox"
                    :checked   @(reaction (:prematch @box-filter false))
                    :on-change (handle-dispatch [:toggle-prematch])}] "Prematch"]
   [:label [:input {:type      "checkbox"
                    :checked   @(reaction (:live @box-filter false))
                    :on-change (handle-dispatch [:toggle-live])}] "Live"]
   [:label [:input {:type      "checkbox"
                    :checked   @(reaction (:results @box-filter false))
                    :on-change (handle-dispatch [:toggle-results])}] "Results"]
   ])

(defn box-panel [box-store]
  (let [box                (reagent/cursor box-store [:box])
        prematch-hierarchy (reagent/cursor box [:prematchHierarchy])]
    (fn [box-store]
      [:div
       (:name @box)
       [:ul (doall (for [group @prematch-hierarchy]
                     (doall (for [{:keys [date betHeaders] :as x} group]
                              ^{:key date}
                              [:li date
                               [:ul
                                (doall (for [{:keys [header sportEvents]} betHeaders]
                                         (doall (for [{:keys [marketId sportEventId betIds]} sportEvents]
                                                  (doall (for [bet-id betIds]
                                                           (let [bet @(reagent/cursor box-store [:sport-events sportEventId :bets bet-id :bet])]
                                                             ^{:key bet-id}
                                                             [:li (str/join " vs " (:participants bet))])))))))]]))))]])))

(defn boxes-panel [rows]
  (let [box-ids (reagent/cursor rows [:box-ids])]
    (fn [rows]
      [:ul
       (for [box-id @box-ids]
         [:li {:key box-id}
          [box-panel (reagent/cursor rows [:boxes box-id])]])])))

(defn dissoc-db-value [key]
  (fn [ctx]
    (sc/update-db ctx dissoc key)))

(def home-page
  {:type   :and
   :enter  [(ctx-log "Entering home page")
            (assoc-page-layout :page/home :column
                               {:column-layout/top    [top-menu-panel]
                                :column-layout/left   [menu-panel (re-frame/subscribe [:betting-page/filter])]
                                :column-layout/center [:div
                                                       [omnifilter (reagent/atom {})]
                                                       [home-page-tabs]
                                                       [boxes-panel (re-frame/subscribe [:home-page/tab-boxes])]]})]
   :exit   [(dissoc-db-value :home-page)
            (ctx-log "Leaving home page")]
   :states {:tabs {:init        :top-5
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
  {:exit        [(dissoc-db-value :betting-page)
                 (ctx-log "Leaving betting page")]
   :enter       [(ctx-log "Entering betting page")
                 (assoc-page-layout :page/betting :column
                                    {:column-layout/top    [top-menu-panel]
                                     :column-layout/left   [menu-panel (re-frame/subscribe [:betting-page/filter])]
                                     :column-layout/center [:div
                                                            [omnifilter (re-frame/subscribe [:betting-page/filter])]
                                                            [boxes-panel (re-frame/subscribe [:betting-page/boxes])]]})
                 (update-filter [:betting-page]
                                (fn [old]
                                  (js/console.log "betting page filter" old)
                                  (if-not old
                                    (make-filter {:live     true
                                                  :prematch true})
                                    old)))
                 (reset-boxes [:betting-page])]
   :transitions [{:event   :set-menu
                  :execute [(fn [ctx]
                              (let [[_ slug] (sc/current-event ctx)
                                    ctx ((update-filter [:betting-page] assoc :menu slug) ctx)]
                                ctx))
                            (reset-boxes [:betting-page])]
                  }
                 {:event   :toggle-prematch
                  :execute [(update-filter [:betting-page] update :prematch not)
                            (reset-boxes [:betting-page])]
                  }
                 {:event   :toggle-live
                  :execute [(update-filter [:betting-page] update :live not)
                            (reset-boxes [:betting-page])]
                  }
                 {:event   :toggle-results
                  :execute [(update-filter [:betting-page] update :results not)
                            (reset-boxes [:betting-page])]
                  }]})

(def mymatches-page
  {:enter [(assoc-page-layout :page/my-matches :column
                              {:column-layout/top    [top-menu-panel]
                               :column-layout/left   [menu-panel]
                               :column-layout/center [:div "My matches"]})]})



(def idx (sc/make-machine
           {:type   :and
            :states {:push {:enter [(ctx-log "Starting push")]
                            :exit  [(ctx-log "Stopping push")]}
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
                                                                                          (let [[_ slug] (sc/current-event ctx)
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

(re-frame/reg-event-fx :initialize-db

  (fn [_ _]
    (sc/initialize idx {:db db/default-db})))

(re-frame/reg-event-fx :goto-page
  (fn [ctx event]
    (sc/process-event idx ctx event)))

(re-frame/reg-event-fx :set-tab
  (fn [ctx event]
    (sc/process-event idx ctx event)))

(re-frame/reg-event-fx :set-menu
  (fn [ctx event]
    (sc/process-event idx ctx event)))

(re-frame/reg-event-fx :toggle-prematch
  (fn [ctx event]
    (sc/process-event idx ctx event)))

(re-frame/reg-event-fx :toggle-live
  (fn [ctx event]
    (sc/process-event idx ctx event)))

(re-frame/reg-event-fx :toggle-results
  (fn [ctx event]
    (sc/process-event idx ctx event)))

(defmulti layout (fn [page] (:layout page)))

(defmethod layout :column [page]
  (let [{:keys [positions]} page]
    [:div
     [:div (:column-layout/top positions)]
     [:div.row
      [:div.col-2 (:column-layout/left positions)]
      [:div.col (:column-layout/center positions)]
      [:div.col-2 (:column-layout/right positions)]]
     [:div (:column-layout/bottom positions)]]))

(defmethod layout :blank [page]
  (let [{:keys [positions]} page]
    [:div
     (:blank-layout/center positions)]))


(defn application []
  (let [page (re-frame/subscribe [:page])]
    (fn []
      (layout @page))))

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