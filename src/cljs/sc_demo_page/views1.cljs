(ns sc-demo-page.views1
  (:require [re-frame.core :as re-frame]
            [sc-demo-page.db :as db]
            [sc-demo-page.statechart :as sc]
            [reagent.ratom :refer-macros [reaction]]
            [clojure.string :as str]
            [ajax.core :refer [GET POST]]))

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
    (get-in home-page [:data :boxes] [])))


(re-frame/reg-sub :betting-page
  (fn [db _]
    (:betting-page db)))
(re-frame/reg-sub :betting-page/tab-boxes
  :<- [:betting-page]
  (fn [betting-page [_]]
    (get-in betting-page [:data :boxes] [])))

(re-frame/reg-sub :menu
  (fn [db _]
    (get-in db [:betting :menu :data])))

(defn goto-substate [event target]
  {:event     event
   :target    target
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
  {:id    :loading-page
   :enter [(assoc-page-layout :page-loading :blank {:blank-layout/center [:div [:i.fa.fa-spinner.fa-5x.fa-spin]]})]})

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


(defn menu-panel []
  [:div.nav.flex-column.nav-pills
   [page-nav-link :page/home "Home"]
   [page-nav-link :page/betting "Betting"]
   [page-nav-link :page/my-matches "My matches"]
   [:hr]
   [sport-menu-panel]])

(defn sport-menu-item [item]
  [:li (:label item)
   (when (seq (:items item))
     [:ul
      (for [[i item] (map-indexed vector (:items item))]
        ^{:key i} [sport-menu-item item])])])

(defn sport-menu-panel []
  (let [menu (re-frame/subscribe [:menu])]
    (fn []
      [:ul
       (for [[i item] (map-indexed vector (:items @menu))]
         ^{:key i} [sport-menu-item item])])))


(defn assoc-db-value [path value]
  (fn [ctx]
    (sc/update-db ctx assoc-in path value)))

(defn load-boxes [path filter]
  (fn [ctx]
    (-> ctx
        (sc/update-db assoc-in (conj path :filter) filter)
        (update-in [:ctx :load-boxes] (fnil conj [])
                   {:path   (conj path :data)
                    :filter filter}))))

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

(defn boxes-panel [rows]
  [:table
   [:thead
    [:tr
     [:th "Box"]]]
   [:tbody
    (for [row @rows]
      [:tr {:key (:boxId row)}
       [:td (:name row)]])]])

(defn dissoc-db-value [key]
  (fn [ctx]
    (sc/update-db ctx dissoc key)))

(def home-page
  {:id          :page/home
   :enter       [(assoc-page-layout :page/home :column
                                    {:column-layout/top    [top-menu-panel]
                                     :column-layout/left   [menu-panel]
                                     :column-layout/center [:div "Home"
                                                            [home-page-tabs]
                                                            [boxes-panel (re-frame/subscribe [:home-page/tab-boxes])]]})]
   :exit        [(dissoc-db-value :home-page)]
   :type        :xor
   :states      [{:id    :top-5
                  :enter [(assoc-db-value [:home-page :current-tab] :top-5)
                          (load-boxes [:home-page] {:boxId     ["top5"]
                                                    :limit     "50"
                                                    :live      "false"
                                                    :prematch  "true"
                                                    :results   "false"
                                                    :videoOnly "false"})]}
                 {:id    :10-naj
                  :enter [(assoc-db-value [:home-page :current-tab] :10-naj)
                          (load-boxes [:home-page] {:boxId     ["naj10"]
                                                    :limit     "50"
                                                    :live      "false"
                                                    :prematch  "true"
                                                    :results   "false"
                                                    :videoOnly "false"})]}
                 {:id    :superkurzy
                  :enter [(assoc-db-value [:home-page :current-tab] :superkurzy)
                          (load-boxes [:home-page] {:boxId     ["superoffer" "superchance" "duel"]
                                                    :limit     "50"
                                                    :live      "false"
                                                    :prematch  "true"
                                                    :results   "false"
                                                    :videoOnly "false"})]}
                 {:id    :top-ponuka
                  :enter [(assoc-db-value [:home-page :current-tab] :top-ponuka)
                          (load-boxes [:home-page] {:filter    ["topponuka"]
                                                    :limit     "50"
                                                    :live      "false"
                                                    :prematch  "true"
                                                    :results   "false"
                                                    :videoOnly "false"})]}]
   :transitions [(goto-substate :set-tab :top-5)
                 (goto-substate :set-tab :10-naj)
                 (goto-substate :set-tab :superkurzy)
                 (goto-substate :set-tab :top-ponuka)]})

(def betting-page
  {:id    :page/betting
   :exit  [(dissoc-db-value :betting-page)]
   :enter [(assoc-page-layout :page/betting :column
                              {:column-layout/top    [top-menu-panel]
                               :column-layout/left   [menu-panel]
                               :column-layout/center [:div
                                                      [boxes-panel (re-frame/subscribe [:betting-page/tab-boxes])]]})
           (load-boxes [:betting-page] {:limit     "50"
                                        :live      "false"
                                        :prematch  "true"
                                        :results   "false"
                                        :videoOnly "false"})]})

(def mymatches-page
  {:id    :page/my-matches
   :enter [(assoc-page-layout :page/my-matches :column
                              {:column-layout/top    [top-menu-panel]
                               :column-layout/left   [menu-panel]
                               :column-layout/center [:div "My matches"]})]})



(def idx (sc/make-machine
           {:id     :application
            :type   :and
            :states [{:id    :push
                      :enter [(ctx-log "Starting push")]
                      :exit  [(ctx-log "Stopping push")]}

                     {:id     :page
                      :type   :xor
                      :states [{:id     :betting
                                :type   :and
                                :states [{:id          :page
                                          :type        :xor
                                          :states      [loading-page
                                                        home-page
                                                        betting-page
                                                        mymatches-page]
                                          :transitions [(goto-substate :goto-page :page/home)
                                                        (goto-substate :goto-page :page/betting)
                                                        (goto-substate :goto-page :page/my-matches)]}
                                         {:id    :menu
                                          :enter [(load-menu [:betting :menu])]}
                                         ]}]}]}))

(re-frame/reg-event-fx :initialize-db
  (fn [_ _]
    (:ctx (sc/initialize idx {:db db/default-db}))))

(re-frame/reg-event-fx :goto-page
  (fn [ctx event]
    (sc/process-event idx ctx event)))

(re-frame/reg-event-fx :set-tab
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
      (js/console.log "Loading boxex for" path "with filter" filter)
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
    (assoc-in db path data)))